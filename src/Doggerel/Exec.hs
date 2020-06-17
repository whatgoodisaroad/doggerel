{-# LANGUAGE FlexibleInstances #-}

module Doggerel.Exec (
    ExecFail,
    InputOutput,
    WriterIO,
    execute,
    executeWith
  ) where

import Control.Monad.State
import Control.Monad.Identity as Identity
import Control.Monad.Writer
import Data.List (find)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.Eval

-- Is the given identifier already defined in the given state as anything?
isExistingIdentifier :: Identifier -> ScopeFrame -> Bool
isExistingIdentifier id (Frame dims units _ assignments)
  =  id `elem` dims
  || id `elem` (map fst units)
  || id `elem` (map fst assignments)

isDefinedAsUnit :: Identifier -> ScopeFrame -> Bool
isDefinedAsUnit id (Frame _ units _ _) = id `elem` map fst units

allReferencesAreDefined :: ScopeFrame -> ValueExpression -> Bool
allReferencesAreDefined f e
  = all (flip isExistingIdentifier f)
  $ referencesOfExpr e

allUnitsAreDefined :: ScopeFrame -> ValueExpression -> Bool
allUnitsAreDefined f e
  = all (flip isDefinedAsUnit f)
  $ map (\(BaseUnit u) -> u)
  $ unitsOfExpr e

-- The InputOutput typeclass represents an IO system for the execution to use.
-- In this form, it acts as a generic wrapper for the IO monad's output, with a
-- writer monad alternative instance to allow tests to inspect output without
-- running in the IO monad.
class InputOutput m where
  output :: String -> m ()

-- The IO monad is the trivial instance.
instance InputOutput IO where
  output = putStrLn

-- Use WriterIO in tests.
type WriterIO a = (WriterT [String] Identity) a
instance InputOutput (WriterT [String] Identity) where
  output s = tell [s]

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
  deriving (Eq, Show)

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

-- Execute a single statement inside a state monad carrying the mutable scope.
executeStatement ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Statement
  -> m (Either ExecFail ScopeFrame)

-- No-op.
executeStatement f Comment = newFrame f

-- A dimension can be declared so long as its identifier is untaken.
executeStatement f@(Frame ds us cs as) (DeclareDimension dimensionId)
  = if isExistingIdentifier dimensionId f
    then execFail
      $ RedefinedIdentifier
      $ "Identifier '" ++ dimensionId ++ "' is already defined."
    else newFrame $ Frame (dimensionId:ds) us cs as

-- A unit can be declare so long as its identifier is untaken, and, if refers to
-- a dimension, that dimension is already defined.
executeStatement f@(Frame ds us cs as) (DeclareUnit id maybeDim)
  = if isExistingIdentifier id f
    then execFail $ RedefinedIdentifier $ redefinedMsg id
    else if not isDimValid
      then execFail $ UnknownIdentifier unknownDimMsg
      else newFrame $ Frame ds ((id, maybeDim):us) cs as
        where
          isDimValid = case maybeDim of
            Nothing -> True
            (Just dim) -> dim `elem` ds
          redefinedMsg id = "Unit '" ++ id ++ "' is already defined."
          unknownDimMsg = case maybeDim of
            Just dim -> "Reference to undeclared dimension '" ++ dim ++ "'"

-- A converstion can be defined so long as both units are already defined and
-- are of the same dimensionality.
executeStatement f@(Frame ds us cs as) (DeclareConversion from to transform) =
  -- Are either units unknown.
  if unknownFrom
  then execFail $ UnknownIdentifier $ noUnitMsg from
  else if unknownTo
  then execFail $ UnknownIdentifier $ noUnitMsg from

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
  else newFrame $ Frame ds us ((from, to, transform):cs) as
    where
      -- Find the units in scope
      fromUnits = find ((==from).fst) us
      toUnits = find ((==to).fst) us

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
executeStatement f@(Frame ds us cs as) (Assignment id expr) =
  -- Is the name already defined.
  if isExistingIdentifier id f
  then execFail $ RedefinedIdentifier $ redefinedMsg id
  else if not $ allReferencesAreDefined f expr
  then execFail $ UnknownIdentifier "Expression refers to unknown identifier"
  else if not $ allUnitsAreDefined f expr
  then execFail $ UnknownIdentifier "Expression refers to unknown units"
  else newFrame $ Frame ds us cs ((id, expr):as)
    where
      redefinedMsg id = "Identifier '" ++ id ++ "' is already defined"

-- A print statement can be executed if every reference identifier in its
-- expression tree is already defined.
executeStatement f (Print expr units) =
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
        output $ show expr ++ " = " ++ show vec'
        newFrame f
