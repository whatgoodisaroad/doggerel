module Doggerel.ParserUtils (
    DParser,
    binaryOpP,
    digitChars,
    dimspecP,
    expressionP,
    identifierP,
    infixOpExpressionP,
    parenExpressionP,
    quantityP,
    scalarDimensionalityP,
    scalarLiteralP,
    statementOptionP,
    statementOptionsP,
    unitsExpressionP,
    unitsP,
    wordChars,
  ) where

import Data.Bifunctor (first)
import Data.Map.Strict as Map (fromList, singleton)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set as Set (empty, fromList)
import Doggerel.Ast
import Doggerel.Conversion;
import Doggerel.Core
import Doggerel.DegreeMap
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

data ExpressionType = VectorValued | UnitValued deriving Eq

-- Shorthand for the Doggerel parser type.
type DParser a = GenParser Char () a

-- Parses a natural number, including 0.
naturalP :: DParser Int
naturalP = fmap read $ many1 digit

-- Convert an associative map of unit names with their degrees into value of
-- type Units.
asUnits :: [(String, Int)] -> Units
asUnits = fromMap . Map.fromList . map (first mkBaseUnit)

asDimensions :: [(String, Int)] -> Dimensionality
asDimensions = fromMap . Map.fromList . map (first mkDimension)

-- Any alphabetical character.
wordChars :: String
wordChars = ['a'..'z'] ++ ['A'..'Z']

-- Any decimal digit.
digitChars :: String
digitChars = ['0'..'9']

reservedWords = [
    "convert",
    "dim",
    "if",
    "input",
    "let",
    "print",
    "relate",
    "unit",
    "while",
    "with"
  ]

-- An identifier is any string of word characters, digits or underscores, so
-- long as the first character is a word character.
identifierP :: DParser String
identifierP = do
  init <- oneOf wordChars
  rest <- many $ oneOf $ wordChars ++ digitChars ++ ['_']
  let id = init:rest
  if id `elem` reservedWords
    then unexpected "Identifier cannot be a reserved word"
    else return id

-- Parse a number into a quantity. It's a string of digits with an optional
-- decimal point somewhere in the middle.
quantityP :: DParser Double
quantityP = do
  whole <- many digit
  frac <- optionMaybe $ string "." >> many digit
  let frac' = case frac of {
      Just "" -> "0";
      Nothing -> "0";
      Just ds -> ds;
    }
  return $ read $ whole ++ "." ++ frac'

unitsP :: DParser Units
unitsP = symDegreeMap baseUnitP

-- Parse a degree map expression. Given a symbol parser, this is a series of
-- degreeExponent's of that symbol separated by whitespace and with an optional
-- '/' somewhere in the middle to separate numerator units from denominator
-- units. Without a '/', all of the units are in the numerator.
symDegreeMap :: Ord s => DParser s -> DParser (DegreeMap s)
symDegreeMap symP = do
  (s, d) <- degreeExponent symP
  let den = try $ do {
      spaces;
      char '/';
      spaces;
      den <- sepBy (degreeExponent symP) (many1 space);
      return $ invert $ fromMap $ Map.fromList den;
    }
  let num = try $ do {
      many1 space;
      symDegreeMap symP
    }
  ss <- den <|> num <|> return emptyMap
  return $ fromMap (singleton s d) `multiply` ss

-- Parses an individual unit identifier with its power optionally specified
-- using a '^' followed by any number of digits to signify the radix. A radix of
-- 1 is assumed if the suffix isn't provided.
degreeExponent :: DParser a -> DParser (a, Int)
degreeExponent symP = do
  id <- symP
  maybeExp <- optionMaybe $ char '^' >> naturalP
  let exp = fromMaybe 1 maybeExp
  return (id, exp)

-- A scalar literal is a quantity and a units expression separated by any amount
-- of whitespace.
scalarLiteralP :: DParser Scalar
scalarLiteralP = do
  mag <- quantityP
  many1 space
  Scalar mag <$> unitsP

-- A parser for ValueExpressions.
expressionP :: DParser Expr
expressionP = expressionWithRefLit VectorValued identifierP scalarLiteralP

expressionWithRefLit ::
     ExpressionType
  -> DParser ref
  -> DParser lit
  -> DParser (ValueExpression ref lit)
expressionWithRefLit eType refP litP
  =   try (prefixOpExpressionP eType refP litP)
  <|> try (postfixExponentExpressionP eType refP litP)
  <|> try (infixOpExpressionP eType refP litP)
  <?> "expression"

referenceP ::
     DParser ref
  -> DParser (ValueExpression ref lit)
referenceP ref = Reference <$> ref

-- An atomic expression is an expression that doesn't use an operator at the top
-- level.
atomicExpressionP ::
     ExpressionType
  -> DParser ref
  -> DParser lit
  -> DParser (ValueExpression ref lit)
atomicExpressionP eType refP litP
  = opts <?> "expression (non-operator)"
  where
    common
      =   try (functionApplicationP eType refP litP)
      <|> try (parenExpressionP eType refP litP)
      <|> try (referenceP refP)
      <|> try (fmap Literal litP)
    opts =
      if eType == VectorValued
        then common <|> (try $ listLiteralExpressionP refP litP)
        else common

-- A function application is any identifier immediately followed by parens
-- containing an expression.
functionApplicationP ::
     ExpressionType
  -> DParser ref
  -> DParser lit
  -> DParser (ValueExpression ref lit)
functionApplicationP eType refP litP = do
  id <- refP
  char '('
  spaces
  expr <- expressionWithRefLit eType refP litP
  spaces
  char ')'
  return $ FunctionApply id expr

postfixExponentExpressionP ::
     ExpressionType
  -> DParser ref
  -> DParser lit
  -> DParser (ValueExpression ref lit)
postfixExponentExpressionP eType refP litP = do
  mantissa <- atomicExpressionP eType refP litP
  spaces
  char '^'
  spaces
  radix <- quantityP
  return $ UnaryOperatorApply (Exponent radix) mantissa

-- An infix operator expression is a sequence of atomic expressions separated by
-- binary operators. The resulting sequence is converted into a tree by the
-- order of operator precedence.
infixOpExpressionP ::
     ExpressionType
  -> DParser ref
  -> DParser lit
  -> DParser (ValueExpression ref lit)
infixOpExpressionP eType refP litP = do
  e <- atomicExpressionP eType refP litP
  spaces
  tailLists <- fmap concat $ many (infixSegment eType refP litP)
  let list = (Right e):tailLists
  case subTreeOperatorsInOrder binaryOperatorPrecendence list of
    [Right e] -> return e
    _ -> unexpected "Failed to parse infix expression."

-- Intermediate representation for an infix expression with potentially many
-- operators.
type BinOpSeq ref lit = [Either BinaryOperator (ValueExpression ref lit)]

-- Parse one segment of an infix sequence: namely an operator and an expression
-- to it's right (not necessarily its right operand, depending on precedence).
infixSegment ::
     ExpressionType
  -> DParser ref
  -> DParser lit
  -> DParser (BinOpSeq ref lit)
infixSegment eType refP litP = do
  op <- binaryOpP
  spaces
  e <- atomicExpressionP eType refP litP
  spaces
  return [Left op, Right e]

-- Given a binary operator and an infix sequence, associate all instances of
-- that operator with its neighbors. For example, using the times operator,
-- (1, *, 2, +, 3) becomes ((1 * 2), +, 3).
makeSubtrees :: BinaryOperator -> BinOpSeq lit ref -> BinOpSeq lit ref
makeSubtrees _ [] = []
makeSubtrees op ((Right e1):(Left op'):(Right e2):rest) =
  if op == op'
  then makeSubtrees op $ (Right $ BinaryOperatorApply op e1 e2):rest
  else (Right e1):(Left op'):(makeSubtrees op $ (Right e2):rest)
makeSubtrees _ s = s

-- Given a list of operators sorted in binding order (strongest first) and given
-- an infix sequence, make subtrees for each.
subTreeOperatorsInOrder ::
     [BinaryOperator]
  -> BinOpSeq lit ref
  -> BinOpSeq lit ref
subTreeOperatorsInOrder [] acc = acc
subTreeOperatorsInOrder (op:ops) acc =
  subTreeOperatorsInOrder ops $ makeSubtrees op acc

-- A prefix operator expression is currently only a negative sign preceding an
-- expression.
prefixOpExpressionP ::
     ExpressionType
  -> DParser ref
  -> DParser lit
  -> DParser (ValueExpression ref lit)
prefixOpExpressionP eType refP litP = do
  spaces
  op <- unaryOpP
  e <- expressionWithRefLit eType refP litP
  return $ UnaryOperatorApply op e

unaryOpP :: DParser UnaryOperator
unaryOpP
  =   (char '-' >> return Negative)
  <|> (char '!' >> return LogicalNot)

-- A parenthesized expression is any expression wrapped in open+close
-- parenthesis and with any amount of whitespace padding inside the parens.
parenExpressionP ::
     ExpressionType
  -> DParser ref
  -> DParser lit
  -> DParser (ValueExpression ref lit)
parenExpressionP eType refP litP = do
  char '('
  spaces
  e <- expressionWithRefLit eType refP litP
  spaces
  char ')'
  return e

listLiteralExpressionP ::
     DParser ref
  -> DParser lit
  -> DParser (ValueExpression ref lit)
listLiteralExpressionP refP litP = do
  string "["
  spaces
  let eP = expressionWithRefLit VectorValued refP litP
  es <- (spaces >> eP) `sepBy1` (spaces >> char ',')
  spaces
  string "]"
  return $ ListLiteral es

-- Parse a single binary operator.
binaryOpP :: DParser BinaryOperator
binaryOpP
  =   try (char '+' >> return Add)
  <|> try (char '-' >> return Subtract)
  <|> try (char '/' >> return Divide)
  <|> try (char '*' >> return Multiply)
  <|> try (string "&&" >> return LogicalAnd)
  <|> try (string "||" >> return LogicalOr)
  <|> try (string "<=" >> return LessThanOrEqualTo)
  <|> try (string ">=" >> return GreaterThanOrEqualTo)
  <|> try (char '<' >> return LessThan)
  <|> try (char '>' >> return GreaterThan)
  <?> "binary operator"

baseUnitP :: DParser BaseUnit
baseUnitP = do
  id <- identifierP
  maybeIndex <- optionMaybe $ do {
      char '(';
      digits <- many $ oneOf digitChars;
      char ')';
      return $ (read :: String -> Int) digits
    }
  return $ BaseUnit id maybeIndex

-- A units expression is used in relations where the references are base units
-- and the values are scalars.
-- Note: this doesn't allow references of compound units, but maybe it should.
-- If it does, we'll need a syntax to distinguish (unit^n)^p from unit^(n*p).
unitsExpressionP :: DParser (ValueExpression Units Quantity)
unitsExpressionP = expressionWithRefLit UnitValued (toMap <$> baseUnitP) quantityP

-- Parser for a predetermined set of options to be used at the end of a
-- statement. Given a list of string keys paired with parsers, provide a list of
-- parsed options with values resulting from their parsers. Options are
-- separated by commas surrounded by any amount of whitespace.
statementOptionsP :: [(String, DParser o)] -> DParser [(String, o)]
statementOptionsP
  = flip sepBy optSepP . choice . map (uncurry statementOptionP)
  where
    optSepP = do
      many space
      char ','
      many space

-- A single option parse. An option is a key and a value parsed by the given
-- sub-parser and separated by a ':' with any amount of whitespace following the
-- ':'.
statementOptionP :: String -> DParser o -> DParser (String, o)
statementOptionP id optP = do
  string id
  char ':'
  many space
  opt <- optP
  return (id, opt)

scalarDimensionalityP :: DParser Dimensionality
scalarDimensionalityP = fmap (fmapDM mkDimension) $ symDegreeMap identifierP

dimspecTermP :: DParser DimspecTerm
dimspecTermP
  =   try dimspecVarP
  <|> try dimspecRangeP
  <|> try dimspecTermDimP

dimspecVarP :: DParser DimspecTerm
dimspecVarP = do
  char ':'
  id <- identifierP
  maybeDeg <- optionMaybe $ char '^' >> naturalP
  let deg = fromMaybe 1 maybeDeg
  return $ DSTermVar id deg

dimspecRangeP :: DParser DimspecTerm
dimspecRangeP = do
  id <- identifierP
  char '('
  low <- optionMaybe naturalP
  maybeRangeSuffix <- optionMaybe $ do {
      string "..";
      optionMaybe naturalP
    }
  char ')'
  return $ case maybeRangeSuffix of {
      Nothing   -> DSTermDim id low 1;
      Just high -> DSTermRange id low high 1;
    }

dimspecTermDimP :: DParser DimspecTerm
dimspecTermDimP = do
  id <- identifierP
  maybeDeg <- optionMaybe $ char '^' >> naturalP
  let deg = fromMaybe 1 maybeDeg
  return $ DSTermDim id Nothing deg

followedByWhitespace :: DParser a -> DParser a
followedByWhitespace p = p >>= \v -> spaces >> return v

dimspecProductNumP :: DParser Dimspec
dimspecProductNumP = do
  terms <- many1 $ followedByWhitespace dimspecTermP
  return $ if length terms == 1
    then DSTerm $ terms !! 0
    else DSProduct $ map DSTerm terms

invDeg :: DimspecTerm -> DimspecTerm
invDeg (DSTermDim id mi deg) = DSTermDim id mi $ 0 - deg
invDeg (DSTermRange id low high deg) = DSTermRange id low high $ 0 - deg
invDeg (DSTermVar id deg) = DSTermVar id $ 0 - deg

dimspecProductP :: DParser Dimspec
dimspecProductP = do
  num <- dimspecProductNumP
  let nums = case num of {
      DSTerm term -> [DSTerm term];
      DSProduct terms -> terms;
    }
  maybeDen <- optionMaybe $ char '/' >> spaces >> dimspecProductNumP
  return $ case maybeDen of {
      Nothing -> num;
      Just (DSTerm den) -> DSProduct $ nums ++ [DSTerm $ invDeg den];
      Just (DSProduct dens) ->
        DSProduct $ nums ++ (map (\(DSTerm den) -> DSTerm $ invDeg den) dens);
    }

dimspecSumP :: DParser Dimspec
dimspecSumP = do
  terms <- (followedByWhitespace dimspecProductP) `sepBy1` (char '+' >> spaces)
  return $ if length terms == 1
    then terms !! 0
    else DSSum terms

dimspecP :: DParser Dimspec
dimspecP
  =   try dimspecSumP
  <|> try dimspecProductP
