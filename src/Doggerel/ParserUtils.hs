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
    referenceExpressionP,
    scalarLiteralP,
    statementOptionP,
    statementOptionsP,
    unitProduct,
    unitsP,
    wordChars,
  ) where

import Data.Map.Strict as Map (fromList)
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
asUnits = fromMap . Map.fromList . map (\(bu, d) -> (BaseUnit bu, d))

asUnits1 :: (String, Int) -> Units
asUnits1 (bu, d) = fromMap $ Map.fromList [(BaseUnit bu, d)]

-- Shorthand for the Doggerel parser type.
type DParser st a = GenParser Char st a

-- Any alphabetical character.
wordChars :: String
wordChars = ['a'..'z'] ++ ['A'..'Z']

-- Any decimal digit.
digitChars :: String
digitChars = ['0'..'9']

-- An identifier is any string of word characters, digits or underscores, so
-- long as the first character is a word character.
identifierP :: DParser st String
identifierP = do
  init <- oneOf $ wordChars
  rest <- many $ oneOf $ wordChars ++ digitChars ++ ['_']
  return $ init:rest

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

-- Parse a units expression. This is a series of unitExponenPs separated by
-- whitespace and with an optional '/' somewhere in the middle to separate
-- numerator units from denominator units. Without a '/', all of the units are
-- in the numerator.
unitsP :: DParser st Units
unitsP = do
  u <- unitExponentP
  let den = try $ do {
      spaces;
      char '/';
      spaces;
      den <- sepBy unitExponentP (many1 space);
      return $ invert $ asUnits den;
    }
  let num = try $ do {
      many1 space;
      unitsP
    }
  us <- den <|> num <|> (return $ asUnits [])
  return $ asUnits1 u `multiply` us

-- Parses an individual unit identifier with its power optionally specified
-- using a '^' followed by any number of digits to signify the radix. A radix of
-- 1 is assumed if the suffix isn't provided.
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

unitProduct :: DParser st [(String, Int)]
unitProduct = sepBy1 unitExponentP $ many1 space

-- A scalar literal is a quantity and a units expression separated by any amount
-- of whitespace.
scalarLiteralP :: DParser st Scalar
scalarLiteralP = do
  mag <- quantityP
  many1 space
  units <- unitsP
  return $ Scalar mag units

-- A parser for ValueExpressions.
expressionP :: DParser st ValueExpression
expressionP = try infixOpExpressionP <|> atomicExpressionP

-- A reference expression is any identifier.
referenceExpressionP :: GenParser Char st ValueExpression
referenceExpressionP = identifierP >>= return . Reference

-- An atomic expression is any expression that cannot be decomposed into
-- multiple sub-expressions.
atomicExpressionP :: DParser st ValueExpression
atomicExpressionP
  =   try parenExpressionP
  <|> referenceExpressionP
  <|> fmap ScalarLiteral scalarLiteralP

-- An infix operator expression is any two expressions separated by a bunary
-- operator. The operator may be separated by any amount of whitespace.
infixOpExpressionP :: DParser st ValueExpression
infixOpExpressionP = do
  lhs <- atomicExpressionP
  spaces
  op <- binaryOpP
  spaces
  rhs <- atomicExpressionP
  return $ BinaryOperatorApply op lhs rhs

-- A parenthesized expression is any expression wrapped in open+close
-- parenthesis and with any amount of whitespace padding inside the parens.
parenExpressionP :: DParser st ValueExpression
parenExpressionP = do
  string "("
  spaces
  e <- expressionP
  spaces
  string ")"
  return e

-- Parse a single binary operator.
binaryOpP :: DParser st BinaryOperator
binaryOpP = do
  op <- oneOf "+*/-"
  return $ case op of
    '+' -> Add
    '*' -> Multiply
    '/' -> Divide
    '-' -> Subtract

-- Parser for a predetermined set of options to be used at the end of a
-- statement. Given a list of string keys paired with parsers, provide a list of
-- parsed options with values resulting from their parsers. Options are
-- separated by commas surrounded by any amount of whitespace.
statementOptionsP :: [(String, DParser st o)] -> DParser st [(String, o)]
statementOptionsP
  = flip sepBy optSepP . choice . map (\(id, optP) -> statementOptionP id optP)
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
