module Doggerel.Parser (parseFile) where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec

import Doggerel.Ast
import Doggerel.Conversion;
import Doggerel.Core
import Doggerel.DegreeMap

wordChars :: String
wordChars = ['a'..'z'] ++ ['A'..'Z']

digitChars :: String
digitChars = ['0'..'9']

identifierP :: GenParser Char st String
identifierP = do
  init <- oneOf wordChars
  rest <- many $ oneOf $ wordChars ++ digitChars
  return $ init:rest

dimDeclP :: GenParser Char st Statement
dimDeclP = do
  string "dim"
  many1 space
  id <- identifierP
  spaces
  char ';'
  return $ DeclareDimension id

unitDclP :: GenParser Char st Statement
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

quantityP :: GenParser Char st Double
quantityP = do
  whole <- many digit
  frac <- optionMaybe $ string "." >> many digit
  let frac' = case frac of {
      Just "" -> "0";
      Nothing -> "0";
      Just ds -> ds;
    }
  return $ read $ whole ++ "." ++ frac'

unitsP :: GenParser Char st Units
unitsP = do
  nums <- unitProduct
  maybeDen <- optionMaybe $ spaces >> char '/' >> spaces >> unitProduct
  let asUnits = foldr1 multiply . map (toMap.BaseUnit)
  return $ case maybeDen of {
      Nothing -> asUnits nums;
      Just den -> asUnits nums `divide` asUnits den;
    }

scalarLiteralP :: GenParser Char st Scalar
scalarLiteralP = do
  mag <- quantityP
  many1 space
  units <- unitsP
  return $ Scalar mag units

unitProduct :: GenParser Char st [String]
unitProduct = identifierP `sepBy1` many1 space

expressionP :: GenParser Char st ValueExpression
expressionP = try infixOpExpressionP <|> atomicExpressionP

referenceExpressionP :: GenParser Char st ValueExpression
referenceExpressionP = identifierP >>= return . Reference

atomicExpressionP :: GenParser Char st ValueExpression
atomicExpressionP
  =   try parenExpressionP
  <|> referenceExpressionP
  <|> fmap ScalarLiteral scalarLiteralP

infixOpExpressionP :: GenParser Char st ValueExpression
infixOpExpressionP = do
  lhs <- atomicExpressionP
  spaces
  op <- binaryOpP
  spaces
  rhs <- atomicExpressionP
  return $ BinaryOperatorApply op lhs rhs

parenExpressionP :: GenParser Char st ValueExpression
parenExpressionP = do
  string "("
  spaces
  e <- expressionP
  spaces
  string ")"
  return e

binaryOpP :: GenParser Char st BinaryOperator
binaryOpP = do
  op <- oneOf "+*"
  return $ case op of
    '+' -> Add
    '*' -> Multiply

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
  char ';'
  return $ Assignment id e

conversionDeclP :: GenParser Char st Statement
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
  if toU /= lhsU
    then fail "LHS must be to units"
    else return ()
  spaces
  string "="
  spaces
  factor <- quantityP
  spaces
  string "*"
  spaces
  rhsU <- identifierP
  if fromU /= rhsU
    then fail "RHS must be from units"
    else return ()
  char ';'
  return $ DeclareConversion fromU toU $ LinearTransform factor

printP :: GenParser Char st Statement
printP = do
  string "print"
  many1 space
  e <- expressionP
  spaces
  maybeUnits <- optionMaybe $ do {
      string "as";
      many1 space;
      units <- unitsP;
      -- spaces;
      return units;
    }
  char ';'
  return $ Print e maybeUnits

commentP :: GenParser Char st Statement
commentP = do
  char '#'
  manyTill anyChar $ char '\n'
  return Comment

statementP :: GenParser Char st Statement
statementP
  =   dimDeclP
  <|> unitDclP
  <|> conversionDeclP
  <|> assignmentP
  <|> printP
  <|> commentP

programP :: GenParser Char st Program
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
