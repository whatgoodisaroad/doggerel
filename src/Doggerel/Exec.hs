module Doggerel.Exec (execute, executeWith) where

import Control.Monad.State
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

allReferencesAreDefined :: ValueExpression -> (StateT ScopeFrame IO) Bool
allReferencesAreDefined e = do
  f <- get
  return $ all (flip isExistingIdentifier f) $ referencesOfExpr e

allUnitsAreDefined :: ValueExpression -> (StateT ScopeFrame IO) Bool
allUnitsAreDefined e = do
  f <- get
  return
    $ all (flip isDefinedAsUnit f)
    $ map (\(BaseUnit u) -> u)
    $ unitsOfExpr e

-- Execute the given program under an empty scope.
execute :: Program -> IO ScopeFrame
execute = executeWith initFrame

executeWith :: ScopeFrame -> Program -> IO ScopeFrame
executeWith f p
  = fmap snd
  $ flip runStateT f
  $ mapM executeStatement p

-- Execute a single statement inside a state monad carrying the mutable scope.
executeStatement :: Statement -> (StateT ScopeFrame IO) ()

-- A dimension can be declared so long as its identifier is untaken.
executeStatement (DeclareDimension dimensionId) = do
  f@(Frame ds us cs as) <- get
  if isExistingIdentifier dimensionId f
    then fail $ "Identifier '" ++ dimensionId ++ "' is already defined."
    else put $ Frame (dimensionId:ds) us cs as

-- A unit can be declare so long as its identifier is untaken, and, if refers to
-- a dimension, that dimension is already defined.
executeStatement (DeclareUnit id maybeDim) = do
  f@(Frame ds us cs as) <- get

  -- Is the unit ID unused?
  if isExistingIdentifier id f
    then fail $ "Identifier '" ++ id ++ "' is already defined."
    else return ()

  -- If there is a dimension spec, does it refer to an existing dimension?
  case maybeDim of
    Nothing -> return ()
    (Just dim) -> if dim `elem` ds
      then return ()
      else fail $ "Reference to undeclared dimension '" ++ dim ++ "'"

  put $ Frame ds ((id, maybeDim):us) cs as

-- A converstion can be defined so long as both units are already defined and
-- are of the same dimensionality.
executeStatement (DeclareConversion from to transform) = do
  f@(Frame ds us cs as) <- get

  if from == to
    then fail $ "Cannot declare conversion from a unit to itself"
    else return ()

  let fromUnits = find ((==from).fst) us
  let toUnits = find ((==to).fst) us

  let noUnitMsg = \u -> "Conversion refers to unkown unit '" ++ u ++ "'"
  let partialUnitlessMsg = \u -> "Cannot convert dimensionless unit '" ++ u ++ "'"

  case (fromUnits, toUnits) of
    -- Fail if either side of conversion is unknown units.
    (Nothing, _) -> fail $ noUnitMsg from
    (_, Nothing) -> fail $ noUnitMsg to

    -- Fail if units are of different dimensionalities
    (Just (_, Just fromDim), Just (_, Just toDim)) ->
      if fromDim == toDim
        then return ()
        else fail $ "Cannot declare conversion between units of different "
          ++ "dimensions: from '" ++ fromDim ++ "' to '" ++ toDim ++ "'"
    (Just (_, Nothing), _) ->
      fail $ partialUnitlessMsg from
    (_, Just (_, Nothing)) ->
      fail $ partialUnitlessMsg to

  put $ Frame ds us ((from, to, transform):cs) as

-- An assignment can be defined so long as its identifier is untaken and every
-- reference identifier in its expression tree is already defined.
executeStatement (Assignment id expr) = do
  f@(Frame ds us cs as) <- get
  if isExistingIdentifier id f
    then fail $ "Identifier '" ++ id ++ "' is already defined"
    else return ()

  refsDefined <- allReferencesAreDefined expr
  if not refsDefined
    then fail $ "Expression refers to unknown identifier"
    else return ()

  unitsDefined <- allUnitsAreDefined expr
  if not unitsDefined
    then fail "Expression refers to unknown units"
    else return ()

  put $ Frame ds us cs ((id, expr):as)

-- A print statement can be executed if every reference identifier in its
-- expression tree is already defined.
executeStatement (Print expr _) = do
  f <- get

  refsDefined <- allReferencesAreDefined expr
  if not refsDefined
    then fail $ "Expression refers to unknown identifier"
    else return ()

  unitsDefined <- allUnitsAreDefined expr
  if not unitsDefined
    then fail "Expression refers to unknown units"
    else return ()

  case evaluate f expr of
    Left err -> fail $ show err
    Right v -> lift $ putStrLn $ show expr ++ " = " ++ show v

executeStatement Comment = return ()