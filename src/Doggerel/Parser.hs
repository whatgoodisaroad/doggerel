module Doggerel.Parser (parseFile) where

import Data.List (nub)
import Data.Map.Strict as Map (fromList)
import Data.Set as Set (empty, fromList)
import Doggerel.Ast
import Doggerel.Conversion;
import Doggerel.Core
import Doggerel.DegreeMap
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

type DParser st a = GenParser Char st a

wordChars :: String
wordChars = ['a'..'z'] ++ ['A'..'Z']

digitChars :: String
digitChars = ['0'..'9']

identifierP :: DParser st String
identifierP = do
  init <- oneOf $ wordChars
  rest <- many $ oneOf $ wordChars ++ digitChars ++ ['_']
  return $ init:rest

dimDeclP :: DParser st Statement
dimDeclP = do
  string "dim"
  many1 space
  id <- identifierP
  spaces
  char ';'
  return $ DeclareDimension id

unitDclP :: DParser st Statement
unitDclP = do {
    string "unit";
    many1 space;
    id <- identifierP;
    maybeDim <- optionMaybe $ do {
        many1 space;
        string "of";
        many1 space;
        dim <- identifierP;
        return dim;
      };
    spaces;
    char ';';
    return $ DeclareUnit id maybeDim;
  }

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

asUnits :: [(String, Int)] -> Units
asUnits = fromMap . Map.fromList . map (\(bu, d) -> (BaseUnit bu, d))

unitsP :: DParser st Units
unitsP = do
  nums <- unitProduct
  maybeDen <- optionMaybe $ spaces >> char '/' >> spaces >> unitProduct
  return $ case maybeDen of {
      Nothing -> asUnits nums;
      Just den -> asUnits nums `divide` asUnits den;
    }

scalarLiteralP :: DParser st Scalar
scalarLiteralP = do
  mag <- quantityP
  many1 space
  units <- unitsP
  return $ Scalar mag units

unitProduct :: DParser st [(String, Int)]
unitProduct = unitExponentP `sepBy1` many1 space

unitExponentP :: DParser st (String, Int)
unitExponentP = do
  id <- identifierP
  maybeExp <- optionMaybe $ do {
      char '^';
      digits <- many1 digit;
      return $ (read :: String -> Int) digits;
    }
  let exp = case maybeExp of {
      Nothing -> 1;
      Just exp' -> exp';
    }
  return (id, exp)

expressionP :: DParser st ValueExpression
expressionP = try infixOpExpressionP <|> atomicExpressionP

referenceExpressionP :: GenParser Char st ValueExpression
referenceExpressionP = identifierP >>= return . Reference

atomicExpressionP :: DParser st ValueExpression
atomicExpressionP
  =   try parenExpressionP
  <|> referenceExpressionP
  <|> fmap ScalarLiteral scalarLiteralP

infixOpExpressionP :: DParser st ValueExpression
infixOpExpressionP = do
  lhs <- atomicExpressionP
  spaces
  op <- binaryOpP
  spaces
  rhs <- atomicExpressionP
  return $ BinaryOperatorApply op lhs rhs

parenExpressionP :: DParser st ValueExpression
parenExpressionP = do
  string "("
  spaces
  e <- expressionP
  spaces
  string ")"
  return e

binaryOpP :: DParser st BinaryOperator
binaryOpP = do
  op <- oneOf "+*/-"
  return $ case op of
    '+' -> Add
    '*' -> Multiply
    '/' -> Divide
    '-' -> Subtract

statementOptionsP :: [(String, DParser st o)] -> DParser st [(String, o)]
statementOptionsP
  = flip sepBy optSepP . choice . map (\(id, optP) -> statementOptionP id optP)
  where
    optSepP = do
      many space
      char ','
      many space

statementOptionP :: String -> DParser st o -> DParser st (String, o)
statementOptionP id optP = do
  string id
  char ':'
  many space
  opt <- optP
  return (id, opt)

data ParserAssignmentOptions
 = AssignmentScalarConstraint

assignmentOptionsP :: DParser st [(String, ParserAssignmentOptions)]
assignmentOptionsP = do
  opts <- statementOptionsP [
      ("scalar", string "true" >> return AssignmentScalarConstraint)
    ]
  let names = fmap fst opts
  if (length $ nub names) < (length names)
    then unexpected "An option is repeated."
    else return opts

assignmentOptionsExprP :: DParser st [(String, ParserAssignmentOptions)]
assignmentOptionsExprP = do
  opts <- optionMaybe $ do {
      string "with";
      many1 space;
      assignmentOptionsP;
    }
  return $ case opts of {
      Just opts -> opts;
      Nothing -> [];
    }

assignmentP :: GenParser Char st Statement
assignmentP = do
  string "let"
  many1 space
  id <- identifierP
  spaces
  char '='
  spaces
  e <- expressionP
  spaces
  opts <- assignmentOptionsExprP
  spaces
  char ';'
  let astOpts = case lookup "scalar" opts of {
      Just AssignmentScalarConstraint -> Set.fromList [ConstrainedScalar];
      Nothing -> empty;
    }
  return $ Assignment id e astOpts

conversionDeclP :: DParser st Statement
conversionDeclP = do
  string "convert"
  many1 space
  fromU <- identifierP
  many1 space
  string "to"
  many1 space
  toU <- identifierP
  many1 space
  string "with"
  many1 space
  lhsU <- identifierP
  spaces
  string "="
  spaces
  factor <- quantityP
  spaces
  string "*"
  spaces
  rhsU <- identifierP
  if (toU == lhsU && fromU == rhsU) || (toU == rhsU && fromU == lhsU)
    then return ()
    else fail "Inconsistent units in converstion"
  char ';'
  return $ DeclareConversion lhsU rhsU $ LinearTransform factor

data ParserPrintOption
  = PrintUnitsConstraint Units
  | PrintStyleFraction
  deriving Show

printOptionsP :: DParser st [(String, ParserPrintOption)]
printOptionsP = do
  opts <- statementOptionsP [
      ("units", fmap PrintUnitsConstraint unitsP),
      ("style", string "fractions" >> return PrintStyleFraction)
    ]
  let names = fmap fst opts
  if (length $ nub names) < (length names)
    then unexpected "An option is repeated."
    else return opts

printOptionsExprP :: DParser st [(String, ParserPrintOption)]
printOptionsExprP = do
  opts <- optionMaybe $ do {
      string "with";
      many1 space;
      printOptionsP;
    }
  return $ case opts of {
      Just opts -> opts;
      Nothing -> [];
    }

printP :: DParser st Statement
printP = do
  string "print"
  many1 space
  e <- expressionP
  spaces
  opts <- printOptionsExprP
  spaces
  let maybeUnits = fmap (\(PrintUnitsConstraint us) -> us) $ lookup "units" opts
  let astPrintOptions = case lookup "style" opts of {
      Just PrintStyleFraction -> Set.fromList [MultiLineFractions];
      Nothing -> empty;
    }
  char ';'
  return $ Print e maybeUnits astPrintOptions

commentP :: DParser st Statement
commentP = do
  char '#'
  manyTill anyChar $ char '\n'
  return Comment

statementP :: DParser st Statement
statementP
  =   dimDeclP
  <|> unitDclP
  <|> conversionDeclP
  <|> assignmentP
  <|> printP
  <|> commentP

programP :: DParser st Program
programP = do
  p <- many $ do
    spaces
    s <- statementP
    spaces
    return s
  eof
  return p

parseFile :: String -> Either ParseError Program
parseFile = parse programP "Failed to parse"
