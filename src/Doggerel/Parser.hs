module Doggerel.Parser (parseExpression, parseFile) where

import Data.List (nub)
import Data.Map.Strict as Map (fromList)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set as Set (Set, empty, insert, fromList, singleton, union)
import Doggerel.Ast
import Doggerel.Conversion;
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.ParserUtils
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

-- A dimension declararion statement is the keyword 'dim' and an identifier
-- separated by any amount of whitespace and terminated with a semicolon.
dimDeclP :: DParser st Statement
dimDeclP = do
  string "dim"
  many1 space
  id <- identifierP
  spaces
  char ';'
  return $ DeclareDimension id

-- A parser for an unit-options list.
unitOptionsP :: DParser st [(String, UnitOption)]
unitOptionsP = do
  opts <- statementOptionsP [
      ("natural", string "true" >> return NaturalUnitDecl),
      ("dims", scalarDimensionalityP >>= return . UnitDimensionality)
    ]
  let names = fmap fst opts
  if length (nub names) < length names
    then unexpected "An option is repeated."
    else return opts

unitDimsAndOpts :: DParser st (Set UnitOption)
unitDimsAndOpts = do
  many1 space
  string "of"
  many1 space
  dims <- scalarDimensionalityP
  many1 space
  string "with"
  many1 space
  opts <- unitOptionsP
  return $ (UnitDimensionality dims) `insert` (Set.fromList $ map snd opts)

unitDimsNoOpts :: DParser st (Set UnitOption)
unitDimsNoOpts = do
  many1 space
  string "of"
  many1 space
  dims <- scalarDimensionalityP
  return $ singleton $ UnitDimensionality dims

optsNoUnitDims :: DParser st (Set UnitOption)
optsNoUnitDims = do
  many1 space
  string "with"
  many1 space
  opts <- unitOptionsP
  return $ Set.fromList $ map snd opts

-- A unit declaration is the keyword 'unit' followed by an identifier for the
-- unit, an optional diemsnsionality specified by the keyword 'of' followed by
-- the identifier of the dimension, and terminated with a semicolon.
unitDclP :: DParser st Statement
unitDclP = do
  string "unit"
  many1 space
  id <- identifierP
  opts <- (
        try unitDimsAndOpts
    <|> try unitDimsNoOpts
    <|> try optsNoUnitDims
    <|> return empty)
  spaces
  char ';'
  return $ DeclareUnit id opts

data ParserAssignmentOptions
 = AssignmentScalarConstraint

-- A parser for an assignment-options list.
assignmentOptionsP :: DParser st [(String, ParserAssignmentOptions)]
assignmentOptionsP = do
  opts <- statementOptionsP [
      ("scalar", string "true" >> return AssignmentScalarConstraint)
    ]
  let names = fmap fst opts
  if length (nub names) < length names
    then unexpected "An option is repeated."
    else return opts

-- A parser for assignment options expression to optionally appear at the end of
-- an assignment statement. If the options are not provided, the list is empty.
assignmentOptionsExprP :: DParser st [(String, ParserAssignmentOptions)]
assignmentOptionsExprP = do
  opts <- optionMaybe $ do {
      string "with";
      many1 space;
      assignmentOptionsP;
    }
  return $ fromMaybe [] opts

-- An assignment statement is the keyword 'let' followed by the identifier of
-- the assignment, followed by an equals sign, followed by an expression,
-- followed by an optional set of assignment options and finally terminated with
-- a semicolon, all separated by any amount of whitespace.
assignmentP :: GenParser Char st Statement
assignmentP = do
  string "let"
  many1 space;
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

updateP :: GenParser Char st Statement
updateP = do
  id <- identifierP
  spaces
  char '='
  spaces
  e <- expressionP
  spaces
  char ';'
  return $ Update id e

blockP :: GenParser Char st Statement
blockP = do
  string "let"
  spaces
  char '{'
  p <- statementsP
  char '}'
  return $ Block p

-- A conversion declaration statement defines a conversion between two units of
-- the same dimensionality.
conversionDeclP :: DParser st Statement
conversionDeclP = do
  string "convert"
  many1 space
  lhsU <- unitsP
  spaces
  string "="
  spaces
  factor <- quantityP
  spaces
  string "*"
  spaces
  rhsU <- unitsP
  char ';'
  return $ DeclareConversion lhsU rhsU $ LinearTransform factor

data ParserPrintOption
  = PrintUnitsConstraint Units
  | PrintStyleFraction
  deriving Show

-- A parser for a print-options list.
printOptionsP :: DParser st [(String, ParserPrintOption)]
printOptionsP = do
  opts <- statementOptionsP [
      ("units", fmap PrintUnitsConstraint unitsP),
      ("style", string "fractions" >> return PrintStyleFraction)
    ]
  let names = fmap fst opts
  if length (nub names) < length names
    then unexpected "An option is repeated."
    else return opts

-- A print options expression that gives a list of options preceded by the
-- 'with' keyword. Gives empty if no options are present.
printOptionsExprP :: DParser st [(String, ParserPrintOption)]
printOptionsExprP = do
  opts <- optionMaybe $ do {
      string "with";
      many1 space;
      printOptionsP;
    }
  return $ fromMaybe [] opts

-- A print statement is the keyword 'print' followed by an expresion, an
-- optional list of  print-options and terminated with a semicolon.
printP :: DParser st Statement
printP = do
  string "print"
  many1 space
  e <- expressionP
  spaces
  opts <- printOptionsExprP
  spaces
  let unitSet = case lookup "units" opts of {
      Just (PrintUnitsConstraint us) -> singleton $ OutputUnits us;
      Nothing -> empty;
    }
  let styleSet = case lookup "style" opts of {
      Just PrintStyleFraction -> singleton MultiLineFractions;
      Nothing -> empty;
    }
  char ';'
  return $ Print e $ unitSet `union` styleSet

-- A comment is the character '#' followed by any number of characters until the
-- EOL is reached.
commentP :: DParser st Statement
commentP = do
  char '#'
  manyTill anyChar $ char '\n'
  return Comment

inputP :: DParser st Statement
inputP = do
  string "input"
  many1 space;
  id <- identifierP
  many1 space
  string "of"
  many1 space
  dims <- scalarDimensionalityP
  char ';'
  return $ Input id dims

relationP :: DParser st Statement
relationP = do
  string "relate"
  many1 space
  id <- identifierP
  many1 space
  string "with"
  many1 space
  e1 <- unitsExpressionP
  spaces
  char '='
  spaces
  e2 <- unitsExpressionP
  spaces
  char ';'
  return $ Relation id e1 e2

conditionalP :: DParser st Statement
conditionalP = do
  string "if"
  spaces
  char '('
  e <- expressionP
  char ')'
  spaces
  char '{'
  aff <- statementsP
  char '}'
  spaces
  maybeNeg <- optionMaybe $ do {
      string "else";
      spaces;
      char '{';
      neg <- statementsP;
      char '}';
      return neg
    }
  return $ Conditional e aff maybeNeg

whileLoopP :: DParser st Statement
whileLoopP = do
  string "while"
  space
  char '('
  e <- expressionP
  char ')'
  spaces
  char '{'
  body <- statementsP
  char '}'
  return $ WhileLoop e body

-- A statement is the disjunction of each statement type.
statementP :: DParser st Statement
statementP
  =   try dimDeclP
  <|> try unitDclP
  <|> try conversionDeclP
  <|> try printP
  <|> try commentP
  <|> try relationP
  <|> try blockP
  <|> try assignmentP
  <|> try inputP
  <|> try conditionalP
  <|> try updateP
  <|> try whileLoopP

statementsP :: DParser st Program
statementsP = many $ do
  spaces
  s <- statementP
  spaces
  return s

-- Parse a list of Doggerel statements in a string into a Program.
parseFile :: String -> Either ParseError Program
parseFile = flip parse "Failed to parse" $ do
  p <- statementsP
  eof
  return p

parseExpression :: String -> Either ParseError Expr
parseExpression = flip parse "Failed to parse" $ do
  e <- expressionP
  eof
  return e
