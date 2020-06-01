module Doggerel.Parser where

import Text.Parsec
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

import Doggerel.Ast
import Doggerel.Conversion;
import Doggerel.Core

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

-- infixExpressionP :: String -> GenParser Char st Expression
-- infixExpressionP operators = identifierP -- reference

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


statementP :: GenParser Char st Statement
statementP = dimDeclP <|> unitDclP <|> conversionDeclP

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
parseFile = parse programP "(unknown)"
