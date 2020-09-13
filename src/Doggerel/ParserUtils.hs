module Doggerel.ParserUtils (
    DParser,
    atomicExpressionP,
    binaryOpP,
    digitChars,
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
import Data.Map.Strict as Map (fromList)
import Data.Maybe (fromMaybe)
import Data.Set as Set (empty, fromList)
import Doggerel.Ast
import Doggerel.Conversion;
import Doggerel.Core
import Doggerel.DegreeMap
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

-- Convert an associative map of unit names with their degrees into value of
-- type Units.
asUnits :: [(String, Int)] -> Units
asUnits = fromMap . Map.fromList . map (first BaseUnit)

asDimensions :: [(String, Int)] -> Dimensionality
asDimensions = fromMap . Map.fromList . map (first Dimension)

-- Shorthand for the Doggerel parser type.
type DParser st a = GenParser Char st a

-- Any alphabetical character.
wordChars :: String
wordChars = ['a'..'z'] ++ ['A'..'Z']

-- Any decimal digit.
digitChars :: String
digitChars = ['0'..'9']

reservedWords = [
    "convert",
    "dim",
    "input",
    "let",
    "print",
    "relate",
    "unit",
    "with"
  ]

-- An identifier is any string of word characters, digits or underscores, so
-- long as the first character is a word character.
identifierP :: DParser st String
identifierP = do
  init <- oneOf wordChars
  rest <- many $ oneOf $ wordChars ++ digitChars ++ ['_']
  let id = init:rest
  if id `elem` reservedWords
    then unexpected "Identifier cannot be a reserved word"
    else return id

-- Parse a number into a quantity. It's a string of digits with an optional
-- decimal point somewhere in the middle.
quantityP :: DParser st Double
quantityP = do
  whole <- many digit
  frac <- optionMaybe $ string "." >> many digit
  let frac' = case frac of {
      Just "" -> "0";
      Nothing -> "0";
      Just ds -> ds;
    }
  return $ read $ whole ++ "." ++ frac'

-- Parse a degree map expression. This is a series of unitExponenPs separated by
-- whitespace and with an optional '/' somewhere in the middle to separate
-- numerator units from denominator units. Without a '/', all of the units are
-- in the numerator.
degreeMapP ::
     Ord a
  => ([(String, Int)] -> DegreeMap a)
  -> DParser st (DegreeMap a)
degreeMapP wrapper = do
  u <- degreeExponent
  let den = try $ do {
      spaces;
      char '/';
      spaces;
      den <- sepBy degreeExponent (many1 space);
      return $ invert $ wrapper den;
    }
  let num = try $ do {
      many1 space;
      degreeMapP wrapper
    }
  us <- den <|> num <|> return (wrapper [])
  return $ wrapper [u] `multiply` us

unitsP :: DParser st Units
unitsP = degreeMapP asUnits

-- Parses an individual unit identifier with its power optionally specified
-- using a '^' followed by any number of digits to signify the radix. A radix of
-- 1 is assumed if the suffix isn't provided.
degreeExponent :: DParser st (String, Int)
degreeExponent = do
  id <- identifierP
  maybeExp <- optionMaybe $ do {
      char '^';
      digits <- many1 digit;
      return $ (read :: String -> Int) digits;
    }
  let exp = fromMaybe 1 maybeExp
  return (id, exp)

-- A scalar literal is a quantity and a units expression separated by any amount
-- of whitespace.
scalarLiteralP :: DParser st Scalar
scalarLiteralP = do
  mag <- quantityP
  many1 space
  Scalar mag <$> unitsP

-- A parser for ValueExpressions.
expressionP :: DParser st Expr
expressionP = expressionWithRefLit identifierP scalarLiteralP

expressionWithRefLit ::
     GenParser Char st ref
  -> GenParser Char st lit
  -> GenParser Char st (ValueExpression ref lit)
expressionWithRefLit refP litP
  =   try (prefixOpExpressionP refP litP)
  <|> try (postfixExponentExpressionP refP litP)
  <|> try (infixOpExpressionP refP litP)
  <|> atomicExpressionP refP litP

referenceP ::
     GenParser Char st ref
  -> GenParser Char st (ValueExpression ref lit)
referenceP ref = Reference <$> ref

-- An atomic expression is any expression that cannot be decomposed into
-- multiple sub-expressions.
atomicExpressionP ::
     GenParser Char st ref
  -> GenParser Char st lit
  -> DParser st (ValueExpression ref lit)
atomicExpressionP refP litP
  =   try (functionApplicationP refP litP)
  <|> try (parenExpressionP refP litP)
  <|> referenceP refP
  <|> fmap Literal litP

-- A function application is any identifier immediately followed by parens
-- containing an expression.
functionApplicationP ::
     GenParser Char st ref
  -> GenParser Char st lit
  -> DParser st (ValueExpression ref lit)
functionApplicationP refP litP = do
  id <- refP
  char '('
  spaces
  expr <- expressionWithRefLit refP litP
  spaces
  char ')'
  return $ FunctionApply id expr

postfixExponentExpressionP ::
     GenParser Char st ref
  -> GenParser Char st lit
  -> DParser st (ValueExpression ref lit)
postfixExponentExpressionP refP litP = do
  mantissa <- atomicExpressionP refP litP
  spaces
  char '^'
  spaces
  radix <- quantityP
  return $ UnaryOperatorApply (Exponent radix) mantissa

-- An infix operator expression is any two expressions separated by a bunary
-- operator. The operator may be separated by any amount of whitespace.
infixOpExpressionP ::
     GenParser Char st ref
  -> GenParser Char st lit
  -> DParser st (ValueExpression ref lit)
infixOpExpressionP refP litP = do
  lhs <- atomicExpressionP refP litP
  spaces
  op <- binaryOpP
  spaces
  rhs <- atomicExpressionP refP litP
  return $ BinaryOperatorApply op lhs rhs

-- A prefix operator expression is currently only a negative sign preceding an
-- expression.
prefixOpExpressionP ::
     GenParser Char st ref
  -> GenParser Char st lit
  -> DParser st (ValueExpression ref lit)
prefixOpExpressionP refP litP = do
  spaces
  op <- unaryOpP
  e <- expressionWithRefLit refP litP
  return $ UnaryOperatorApply op e

unaryOpP :: DParser st UnaryOperator
unaryOpP
  =   (char '-' >> return Negative)
  <|> (char '!' >> return LogicalNot)

-- A parenthesized expression is any expression wrapped in open+close
-- parenthesis and with any amount of whitespace padding inside the parens.
parenExpressionP ::
     GenParser Char st ref
  -> GenParser Char st lit
  -> DParser st (ValueExpression ref lit)
parenExpressionP refP litP = do
  string "("
  spaces
  e <- expressionWithRefLit refP litP
  spaces
  string ")"
  return e

-- Parse a single binary operator.
binaryOpP :: DParser st BinaryOperator
binaryOpP
  =   (char '+' >> return Add)
  <|> (char '-' >> return Subtract)
  <|> (char '/' >> return Divide)
  <|> (char '*' >> return Multiply)
  <|> (string "&&" >> return LogicalAnd)
  <|> (string "||" >> return LogicalOr)
  <|> (string "<=" >> return LessThanOrEqualTo)
  <|> (string ">=" >> return GreaterThanOrEqualTo)
  <|> (char '<' >> return LessThan)
  <|> (char '>' >> return GreaterThan)

baseUnitP :: GenParser Char st Units
baseUnitP = toMap . BaseUnit <$> identifierP

parenUnitP :: GenParser Char st Units
parenUnitP = do
  char '('
  spaces
  u <- unitsP
  spaces
  char ')'
  return u

expressionUnitsP :: GenParser Char st Units
expressionUnitsP = try parenUnitP <|> baseUnitP

unitsExpressionP :: GenParser Char st (ValueExpression Units Quantity)
unitsExpressionP = expressionWithRefLit expressionUnitsP quantityP

-- Parser for a predetermined set of options to be used at the end of a
-- statement. Given a list of string keys paired with parsers, provide a list of
-- parsed options with values resulting from their parsers. Options are
-- separated by commas surrounded by any amount of whitespace.
statementOptionsP :: [(String, DParser st o)] -> DParser st [(String, o)]
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
statementOptionP :: String -> DParser st o -> DParser st (String, o)
statementOptionP id optP = do
  string id
  char ':'
  many space
  opt <- optP
  return (id, opt)

scalarDimensionalityP :: DParser st Dimensionality
scalarDimensionalityP = degreeMapP asDimensions
