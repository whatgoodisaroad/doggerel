module Doggerel.Parser (parseFile) where

import Data.List (nub)
import Data.Map.Strict as Map (fromList)
import Data.Set as Set (empty, fromList)
import Doggerel.Ast
import Doggerel.Conversion;
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.ParserUtils
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

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
