{-# LANGUAGE FlexibleInstances #-}

module Doggerel.Exec (
    ExecFail(..),
    InputOutput,
    TestIO,
    execute,
    executeWith
  ) where

import Control.Monad.State
import Control.Monad.Identity as Identity
import Control.Monad.State.Lazy
import Control.Monad.Writer
import Data.Set (Set, empty, fromList, toList)
import Data.List (find)
import Data.Map.Strict (keys)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.DegreeMap (getMap)
import Doggerel.Eval
import Doggerel.Output
import Doggerel.ParserUtils (scalarLiteralP)
import Doggerel.Scope
import Text.ParserCombinators.Parsec (eof, parse)

-- Is the given identifier already defined in the given state as anything?
isExistingIdentifier :: Identifier -> ScopeFrame -> Bool
isExistingIdentifier id f
  =  id `elem` (getDimensions f)
  || id `elem` (map fst $ getUnits f)
  || id `elem` (map getAssignmentId $ getAssignments f)

isDefinedAsUnit :: Identifier -> ScopeFrame -> Bool
isDefinedAsUnit id f = id `elem` (map fst $ getUnits f)

isDefinedAsAssignmentOrInput :: ScopeFrame -> Identifier -> Bool
isDefinedAsAssignmentOrInput f id
  =  id `elem` (map getAssignmentId $ getAssignments f)
  || id `elem` (map getInputId $ getInputs f)

allReferencesAreDefined :: ScopeFrame -> ValueExpression -> Bool
allReferencesAreDefined f e
  = all (isDefinedAsAssignmentOrInput f)
  $ referencesOfExpr e

allUnitsAreDefined :: ScopeFrame -> ValueExpression -> Bool
allUnitsAreDefined f e
  = all (flip isDefinedAsUnit f)
  $ map (\(BaseUnit u) -> u)
  $ unitsOfExpr e

allDimensionsAreDefined :: ScopeFrame -> Dimensionality -> Bool
allDimensionsAreDefined f d = all exists dimIds
  where
    dimIds :: [String]
    dimIds = map (\(Dimension d) -> d) $ keys $ getMap d

    exists :: String -> Bool
    exists s = s `elem` getDimensions f || s `elem` dimensionlessUnits

    dimensionlessUnits :: [String]
    dimensionlessUnits = map fst $ filter (\(_, d) -> d == Nothing) $ getUnits f

-- The InputOutput typeclass represents an IO system for the execution to use.
-- In this form, it acts as a generic wrapper for the IO monad's output, with a
-- writer monad alternative instance to allow tests to inspect output without
-- running in the IO monad.
class InputOutput m where
  output :: String -> m ()
  input :: m String

-- The IO monad is the trivial instance.
instance InputOutput IO where
  output = putStrLn
  input = getLine

-- Use WriterIO in tests.
-- type WriterIO a = (WriterT [String] Identity) a
-- instance InputOutput (WriterT [String] Identity) where
--   output s = tell [s]
--   input = return ""

type TestIO a = State ([String], [String]) a
instance InputOutput (State ([String], [String])) where
  output o = do
    (os, is) <- get
    put (os++[o], is)
    return ()
  input = do
    (os, is) <- get
    case is of
      [] -> return ""
      (i:is') -> put (os, is') >> return i

-- Execute the given program under an empty scope.
execute ::
     (Monad m, InputOutput m)
  => Program
  -> m (Either ExecFail ScopeFrame)
execute = executeWith initFrame

executeWith ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Program
  -> m (Either ExecFail ScopeFrame)
executeWith f [] = newFrame f
executeWith f (s:ss) = do
  result <- executeStatement f s
  case result of
    Left _ -> return result
    Right f' -> executeWith f' ss

-- Convert a vector for printing. If there is no result, or if there is no
-- specified target units, then do nothing and give the original vector.
-- Otherwise attempt to convert as a scalar.
convertForDisplay ::
     ScopeFrame
  -> Maybe Units
  -> Vector
  -> Maybe Vector
convertForDisplay f units vec = case units of
  Nothing -> Just vec
  Just us -> convertAsScalar f vec us

data ExecFail
  = ExecEvalFail EvalFail
  | UnknownIdentifier String
  | RedefinedIdentifier String
  | InvalidConversion String
  | UnsatisfiableConstraint String
  | UnsatisfiedConstraint String
  deriving (Eq, Show)

-- Given a set of assignment options, a scope frame and the resulting vector
-- value to be potentially recorded in the assignment, give a list of strings
-- that desceibe the ways in which constraints in those options were violated by
-- the vector. If the resulting list is empty, there are no violations
failedAssignmentConstraints ::
     Set AssignmentOption
  -> ScopeFrame
  -> Vector
  -> [String]
failedAssignmentConstraints opts f vec
  = concat $ flip fmap (toList opts)
  $ \o -> case (o, getVectorDimensionality f vec) of
    (ConstrainedScalar, dims@(VecDims dimSet)) ->
      if 1 /= length dimSet
        then [concat [
              "Constrained to scalar, but vector had multiple components:\n",
              "  actual: " ++ show dims
            ]
          ]
        else []
    (ConstrainedDimensionality target, dims) ->
      if target /= dims
        then [concat [
              "Vector does not match target dims:\n",
              "  target: " ++ show target ++ "\n",
              "  actual: " ++ show dims
            ]
          ]
        else []

execFail ::
     (Monad m, InputOutput m)
  => ExecFail
  -> m (Either ExecFail ScopeFrame)
execFail = return . Left

newFrame ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> m (Either ExecFail ScopeFrame)
newFrame = return . Right

readScalarLiteralInput ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Dimensionality
  -> m Scalar
readScalarLiteralInput f d = do
  output $ "Enter a scalar of dimensionality " ++ show d
  i <- input
  let recurse = readScalarLiteralInput f d
  case parse (scalarLiteralP >>= \s -> eof >> return s) "fail" i of
    Left _ -> recurse
    Right s -> do
      let actualDims = getDimensionality f $ getScalarUnits s
      if d == actualDims
        then return s
        else recurse

-- Execute a single statement inside a state monad carrying the mutable scope.
executeStatement ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Statement
  -> m (Either ExecFail ScopeFrame)

-- No-op.
executeStatement f Comment = newFrame f

-- A dimension can be declared so long as its identifier is untaken.
executeStatement f (DeclareDimension dimensionId)
  = if isExistingIdentifier dimensionId f
    then execFail
      $ RedefinedIdentifier
      $ "Identifier '" ++ dimensionId ++ "' is already defined."
    else newFrame $ f `withDimension` dimensionId

-- A unit can be declare so long as its identifier is untaken, and, if refers to
-- a dimension, that dimension is already defined.
executeStatement f (DeclareUnit id maybeDim) =
  -- Fail if the identifier already exists.
  if isExistingIdentifier id f
  then execFail $ RedefinedIdentifier $ redefinedMsg id

  -- If the unit states its dimension, but that diemnsion is unknown, then the
  -- declaration is not valid.
  else if not isDimValid
  then execFail $ UnknownIdentifier unknownDimMsg

  -- Otherwise, it's valid.
  else newFrame $ f `withUnit` (id, maybeDim)
    where
      isDimValid = case maybeDim of
        Nothing -> True
        (Just dim) -> dim `elem` (getDimensions f)
      redefinedMsg id = "Identifier '" ++ id ++ "' is already defined."
      unknownDimMsg = case maybeDim of
        Just dim -> "Reference to undeclared dimension '" ++ dim ++ "'"

-- A converstion can be defined so long as both units are already defined and
-- are of the same dimensionality.
executeStatement f (DeclareConversion from to transform) =
  -- Are either units unknown.
  if unknownFrom
  then execFail $ UnknownIdentifier $ noUnitMsg from
  else if unknownTo
  then execFail $ UnknownIdentifier $ noUnitMsg to

  -- Is the conversion cyclic.
  else if isCyclic
  then execFail $ InvalidConversion cyclicMsg

  -- Are either end of the conversion dimensionless.
  else if dimensionlessFrom || dimensionlessTo
  then execFail $ InvalidConversion dimensionlessMsg

  -- Are dimensional units matched?
  else if not areDimensionsMatched
  then execFail $ InvalidConversion mismatchMsg

  -- Otherwise, it's valid.
  else newFrame $ f `withConversion` (from, to, transform)
    where
      -- Find the units in scope
      fromUnits = find ((==from).fst) $ getUnits f
      toUnits = find ((==to).fst) $ getUnits f

      -- Are either units unknown in scope.
      unknownFrom = fromUnits == Nothing
      unknownTo = toUnits == Nothing

      -- Is the conversion cyclic.
      isCyclic = from == to

      -- Are either units dimensionless.
      dimensionlessFrom = case fromUnits of
        (Just (_, Nothing)) -> True
        _ -> False
      dimensionlessTo = case toUnits of
        (Just (_, Nothing)) -> True
        _ -> False

      -- Does the conversion connect units of matching dimensionality.
      areDimensionsMatched = case (fromUnits, toUnits) of
        (Just (_, Just fromDim), Just (_, Just toDim)) ->
          fromDim == toDim
        _ -> False

      -- Error messages
      noUnitMsg u = "Conversion refers to unkown unit '" ++ u ++ "'"
      cyclicMsg = "Cannot declare conversion from a unit to itself"
      dimensionlessMsg = "Cannot convert dimensionless unit"
      mismatchMsg = case (fromUnits, toUnits) of
        (Just (_, Just fromDim), Just (_, Just toDim)) ->
          "Cannot declare conversion between units of different "
            ++ "dimensions: from '" ++ fromDim ++ "' to '" ++ toDim ++ "'"

-- An assignment can be defined so long as its identifier is untaken and every
-- reference identifier in its expression tree is already defined.
executeStatement f (Assignment id expr opts) =
  -- Is the name already defined.
  if isExistingIdentifier id f
  then execFail $ RedefinedIdentifier $ redefinedMsg id

  -- Do all references in the expression refer to already-defined IDs?
  else if not $ allReferencesAreDefined f expr
  then execFail $ UnknownIdentifier "Expression refers to unknown identifier"

  -- Is every unit used in a literal an existing unit?
  else if not $ allUnitsAreDefined f expr
  then execFail $ UnknownIdentifier "Expression refers to unknown units"

  -- Otherwsie, it's valid if it can be evaluated.
  else case evaluate f expr of
    Left err -> execFail $ ExecEvalFail err
    Right vec -> if 0 < (length $ failedConstraints vec)
      then execFail $ UnsatisfiedConstraint $ (head $ failedConstraints vec)
      else newFrame $ f `withAssignment` (id, expr, vec)
    where
      redefinedMsg id = "Identifier '" ++ id ++ "' is already defined"
      failedConstraints = failedAssignmentConstraints opts f

-- A print statement can be executed if every reference identifier in its
-- expression tree is already defined.
executeStatement f (Print expr units opts) =
  if not $ allReferencesAreDefined f expr
  then execFail $ UnknownIdentifier "Expression refers to unknown identifier"
  else if not $ allUnitsAreDefined f expr
  then execFail $ UnknownIdentifier "Expression refers to unknown units"
  else case evaluate f expr of
    Left err -> execFail $ ExecEvalFail err
    Right vec -> case convertForDisplay f units vec of
      -- TODO: fail statically if target units dimensionality is mismatched.
      Nothing -> execFail $ UnsatisfiableConstraint "could not convert to units"
      Just vec' -> do
        mapM_ output $ prettyPrint opts expr vec'
        newFrame f

executeStatement f (Input id dims) =
  if isExistingIdentifier id f
  then execFail $ RedefinedIdentifier $ redefinedMsg id
  else if not $ allDimensionsAreDefined f dims
  then execFail $ UnknownIdentifier "Expression refers to unknown dimensions"
  else do
    s <- readScalarLiteralInput f dims
    newFrame $ f `withInput` (id, dims, Just s)
  where
    redefinedMsg id = "Identifier '" ++ id ++ "' is already defined"
