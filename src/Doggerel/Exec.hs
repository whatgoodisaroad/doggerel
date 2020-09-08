{-# LANGUAGE FlexibleInstances #-}

module Doggerel.Exec (
    ExecFail(..),
    InputOutput,
    TestIO,
    execute,
    executeWith
  ) where

import Control.Monad.Identity as Identity
import Control.Monad.State
import Control.Monad.State.Lazy
import Control.Monad.Writer
import Data.Set as Set (Set, empty, insert, fromList, member, toList)
import Data.List (find)
import Data.Map.Strict (keys)
import Data.Maybe (fromJust, isNothing)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.DegreeMap (allKeys, getMap)
import Doggerel.Eval
import Doggerel.Output
import Doggerel.ParserUtils (scalarLiteralP)
import Doggerel.Relation (allRefsAreUnique, asVectorMap)
import Doggerel.Scope
import Text.ParserCombinators.Parsec (eof, parse)

-- Is the given identifier already defined in the given state as anything?
isExistingIdentifier :: Identifier -> ScopeFrame -> Bool
isExistingIdentifier id f
  =  id `elem` getDimensions f
  || id `elem` map fst (getUnits f)
  || id `elem` map getAssignmentId (getAssignments f)
  || id `elem` map getInputId (getInputs f)
  || id `elem` map getRelationId (getRelations f)

isDefinedAsUnit :: Identifier -> ScopeFrame -> Bool
isDefinedAsUnit id f = id `elem` map fst (getUnits f)

isDefinedAsAssignmentRelationOrInput :: ScopeFrame -> Identifier -> Bool
isDefinedAsAssignmentRelationOrInput f id
  =  id `elem` map getAssignmentId (getAssignments f)
  || id `elem` map getInputId (getInputs f)
  || id `elem` map getRelationId (getRelations f)

allReferencesAreDefined :: ScopeFrame -> Expr -> Bool
allReferencesAreDefined f e
  = all (isDefinedAsAssignmentRelationOrInput f)
  $ referencesOfExpr e

allBaseUnitsAreDefined :: ScopeFrame -> [BaseUnit] -> Bool
allBaseUnitsAreDefined f
  = all ((`isDefinedAsUnit` f) . (\ (BaseUnit u) -> u))

allUnitsOfExpressionAreDefined :: ScopeFrame -> Expr -> Bool
allUnitsOfExpressionAreDefined f = allBaseUnitsAreDefined f . unitsOfExpr

unitsAreDefined :: ScopeFrame -> Units -> Bool
unitsAreDefined f = allBaseUnitsAreDefined f . keys . getMap

allDimensionsAreDefined :: ScopeFrame -> Dimensionality -> Bool
allDimensionsAreDefined f d = all exists dimIds
  where
    dimIds :: [String]
    dimIds = map (\(Dimension d) -> d) $ keys $ getMap d

    exists :: String -> Bool
    exists s = s `elem` getDimensions f || s `elem` dimensionlessUnits

    dimensionlessUnits :: [String]
    dimensionlessUnits = map fst $ filter (\(_, d) -> isNothing d) $ getUnits f

allUnitsAreDefined :: ScopeFrame -> Units -> Bool
allUnitsAreDefined f = allBaseUnitsAreDefined f . keys . getMap

allUnitsAreDimensional :: ScopeFrame -> Units -> Bool
allUnitsAreDimensional f = allKeys isDefinedWithDim
  where
    isDefinedWithDim :: BaseUnit -> Bool
    isDefinedWithDim (BaseUnit id) = not $ isNothing $ getUnitDimensionById f id

allRefsOfUnitsExpressionDefined ::
     ScopeFrame
  -> ValueExpression Units Quantity
  -> Bool
allRefsOfUnitsExpressionDefined f
  = all (allUnitsAreDefined f) . referencesOfExpr

containsExponent :: ValueExpression ref lit -> Bool
containsExponent (UnaryOperatorApply (Exponent _) _) = True
containsExponent (UnaryOperatorApply _ e) = containsExponent e
containsExponent (BinaryOperatorApply _ e1 e2)
  = containsExponent e1 || containsExponent e2
containsExponent (FunctionApply _ e) = containsExponent e
containsExponent _ = False

-- The InputOutput typeclass represents an IO system for the execution to use.
-- In this form, it acts as a generic wrapper for the IO monad's output, with a
-- writer monad alternative instance to allow tests to inspect output without
-- running in the IO monad.
class InputOutput m where
  output :: String -> m ()
  input :: m String
  errorOutput :: String -> m ()

-- The IO monad is the trivial instance.
instance InputOutput IO where
  output = putStrLn
  input = getLine
  errorOutput = putStrLn

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
  -- Errors are currently not tested via output, so we drop it in this instance.
  errorOutput _ = return ()

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
    Left err -> do
      errorOutput $ "Encountered error: " ++ show err
      return result
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
  | InvalidVectorExpression String
  deriving (Eq, Show)

-- Given a set of assignment options, a scope frame and the resulting vector
-- value to be potentially recorded in the assignment, give a list of strings
-- that desceibe the ways in which constraints in those options were violated by
-- the vector. If the resulting list is empty, there are no violations
failedAssignmentConstraints ::
     Set AssignmentOption
  -> ScopeFrame
  -> VectorDimensionality
  -> [String]
failedAssignmentConstraints opts f dims
  = concat $ flip fmap (toList opts)
  $ \o -> case (o, dims) of
    (ConstrainedScalar, dims@(VecDims dimSet)) ->
      (["Constrained to scalar, but vector had multiple components:\n" ++
        "  actual: " ++ show dims
        | 1 /= length dimSet])
    (ConstrainedDimensionality target, dims) ->
      (["Vector does not match target dims:\n" ++
        "  target: " ++ show target ++ "\n" ++ "  actual: " ++ show dims
        | target /= dims])

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
  output $ "Enter a scalar of dimensionality {" ++ show d ++ "}"
  i <- input
  let recurse = output "Try again..." >> readScalarLiteralInput f d
  case parse (scalarLiteralP >>= \s -> eof >> return s) "fail" i of
    Left err -> do
      output $ "Failed to parse input as a scalar: " ++ show err
      recurse
    Right s -> do
      let actualUnits = getScalarUnits s
      let actualDims = getDimensionality f actualUnits
      if not $ unitsAreDefined f actualUnits
        then do
          output $ "Unknown units: " ++ show actualUnits
          recurse
        else if d == actualDims
          then return s
          else do
            output   "Mismatched dimensionality"
            output $ "  Expected: {" ++ show d ++ "}"
            output $ "     Found: {" ++ show actualDims ++ "}"
            recurse

exponentMsg = "Exponent operator not allowed in vector-valued expressions"

-- Given a scope frame and an expression, resolve each unresolved input referred
-- to in the expression to a value by requesting it from input.
resolveInputs ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Expr
  -> m ScopeFrame
resolveInputs f (Reference r) = do
  case getInputById f r of
    Just (_, Right s) -> return f
    Just (_, Left dims) -> do
      s <- readScalarLiteralInput f dims
      return $ f `replaceInput` (r, Right s)
    Nothing -> case getAssignmentById f r of
      Just (_, e) -> resolveInputs f e
resolveInputs f (UnaryOperatorApply _ e) = resolveInputs f e
resolveInputs f (BinaryOperatorApply _ e1 e2) = do
  f' <- resolveInputs f e1
  resolveInputs f' e2
resolveInputs f (FunctionApply _ e) = resolveInputs f e
resolveInputs f _ = return f

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
executeStatement f (DeclareUnit id maybeDim)
  -- Fail if the identifier already exists.
  | isExistingIdentifier id f
  = execFail $ RedefinedIdentifier $ redefinedMsg id
  -- If the unit states its dimension, but that diemnsion is unknown, then the
  -- declaration is not valid.
  | not isDimValid = execFail $ UnknownIdentifier unknownDimMsg
  -- Otherwise, it's valid.
  | otherwise = newFrame $ f `withUnit` (id, maybeDim)
  where
    isDimValid = isNothing maybeDim || allKeys dimExists (fromJust maybeDim)
    dimExists (Dimension dim) = dim `elem` getDimensions f
    redefinedMsg id = "Identifier '" ++ id ++ "' is already defined."
    unknownDimMsg
      = case maybeDim of {
          Just dim
            -> "Reference to undeclared dimension in '" ++ show dim ++ "'" }

-- A converstion can be defined so long as both units are already defined and
-- are of the same dimensionality.
executeStatement f (DeclareConversion from to transform)
  -- Are either units unknown.
  | unknownFrom = execFail $ UnknownIdentifier $ noUnitMsg from
  | unknownTo = execFail $ UnknownIdentifier $ noUnitMsg to

  -- Is the conversion cyclic.
  | isCyclic = execFail $ InvalidConversion cyclicMsg

  -- Are either end of the conversion dimensionless.
  | dimensionlessFrom || dimensionlessTo
  = execFail $ InvalidConversion dimensionlessMsg

  -- Are dimensional units matched?
  | not areDimensionsMatched = execFail $ InvalidConversion mismatchMsg

  -- Otherwise, it's valid.
  | otherwise = newFrame $ f `withConversion` (from, to, transform)
  where
    -- Are either units unknown in scope.
    unknownFrom = not $ allUnitsAreDefined f from
    unknownTo = not $ allUnitsAreDefined f to

    -- Is the conversion cyclic.
    isCyclic = from == to

    -- Are either units dimensionless.
    dimensionlessFrom = not $ allUnitsAreDimensional f from
    dimensionlessTo = not $ allUnitsAreDimensional f to

    -- Does the conversion connect units of matching dimensionality.
    fromDim = getDimensionality f from
    toDim = getDimensionality f to
    areDimensionsMatched = fromDim == toDim

    -- Error messages
    noUnitMsg u = "Conversion refers to unkown unit '" ++ show u ++ "'"
    cyclicMsg = "Cannot declare conversion from a unit to itself"
    dimensionlessMsg = "Cannot convert dimensionless unit"
    mismatchMsg = "Cannot declare conversion between units of different " ++
      "dimensions: from '" ++ show fromDim ++ "' to '" ++ show toDim ++ "'"

-- An assignment can be defined so long as its identifier is untaken and every
-- reference identifier in its expression tree is already defined.
executeStatement f (Assignment id expr opts)
  -- Is the name already defined.
  | isExistingIdentifier id f = execFail $ RedefinedIdentifier $ redefinedMsg id

  -- Do all references in the expression refer to already-defined IDs?
  | not $ allReferencesAreDefined f expr
  = execFail $ UnknownIdentifier "Expression refers to unknown identifier"

  -- Is every unit used in a literal an existing unit?
  | not $ allUnitsOfExpressionAreDefined f expr
  = execFail $ UnknownIdentifier "Expression refers to unknown units"

  -- Currently don't support exponents at this level. TODO: add support.
  | containsExponent expr = execFail $ InvalidVectorExpression exponentMsg

  -- Otherwise, it's valid if it can be evaluated.
  | otherwise =
    if isNothing staticDims
    then execFail $ UnsatisfiableConstraint staticFailMsg
    else if not $ null $ failedConstraints $ fromJust staticDims
    then execFail
      $ UnsatisfiedConstraint
      $ head
      $ failedConstraints
      $ fromJust staticDims
    else newFrame $ f `withAssignment` (id, expr)
  where
    staticDims = staticEval f expr
    redefinedMsg id = "Identifier '" ++ id ++ "' is already defined"
    failedConstraints = failedAssignmentConstraints opts f
    staticFailMsg = "cannot statically determine dims of expression"

-- A print statement can be executed if every reference identifier in its
-- expression tree is already defined.
executeStatement f (Print expr units opts)
  | not $ allReferencesAreDefined f expr
  = execFail $ UnknownIdentifier "Expression refers to unknown identifier"
  | not $ allUnitsOfExpressionAreDefined f expr
  = execFail $ UnknownIdentifier "Expression refers to unknown units"
  | containsExponent expr = execFail $ InvalidVectorExpression exponentMsg
  | otherwise = do
    f' <- resolveInputs f expr
    case evaluate f' expr of
      Left err -> execFail $ ExecEvalFail err
      Right vec -> case convertForDisplay f' units vec of
        -- TODO: fail statically if target units dimensionality is mismatched.
        Nothing ->
          execFail $ UnsatisfiableConstraint "could not convert to units"
        Just vec' -> do
          mapM_ output $ prettyPrint optsToUse expr vec'
          newFrame f'
  where
    optsToUse = if f `hasPragma` AsciiOutput
      then AsciiOnlyPragma `Set.insert` opts
      else opts

executeStatement f (Input id dims)
  | isExistingIdentifier id f = execFail $ RedefinedIdentifier $ redefinedMsg id
  | not $ allDimensionsAreDefined f dims
  = execFail $ UnknownIdentifier "Expression refers to unknown dimensions"
  | otherwise = newFrame $ f `withInput` (id, Left dims)
  where
    redefinedMsg id = "Identifier '" ++ id ++ "' is already defined"

executeStatement f (Relation id e1 e2)
  | isExistingIdentifier id f = execFail $ RedefinedIdentifier $ redefinedMsg id
  | not $ allRefsOfUnitsExpressionDefined f e1
  = execFail $ UnknownIdentifier unknownUnitMsg
  | not $ allRefsOfUnitsExpressionDefined f e2
  = execFail $ UnknownIdentifier unknownUnitMsg
  | not $ allRefsAreUnique e1 e2 = execFail $ RedefinedIdentifier reusedMsg
  | otherwise = newFrame $ f `withRelation` (id, asVectorMap e1 e2)
  where
    reusedMsg = "Units are repeated within relation."
    redefinedMsg id = "Identifier '" ++ id ++ "' is already defined"
    unknownUnitMsg = "Relation refers to unknown units"
