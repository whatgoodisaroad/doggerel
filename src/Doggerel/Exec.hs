{-# LANGUAGE FlexibleInstances, LambdaCase #-}

module Doggerel.Exec (
    ExecFail(..),
    InputOutput(..),
    TestIO,
    execute,
    executeWith
  ) where

import Control.Monad (void)
import Control.Monad.Identity as Identity
import Control.Monad.State
import Control.Monad.State.Lazy
import Control.Monad.Writer
import Data.Set as Set (Set, empty, insert, fromList, member, singleton, toList)
import Data.List (find)
import Data.Map.Strict (keys)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.DegreeMap (allKeys, toMap)
import Doggerel.Eval
import Doggerel.Output
import Doggerel.ParserUtils (scalarLiteralP)
import Doggerel.Relation (allRefsAreUnique, allRefsAreUniqueDims, asVectorMap)
import Doggerel.Scope
import Doggerel.Validation
import Text.ParserCombinators.Parsec (eof, parse)

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

-- TODO: Make this a record with accessors.
type TestIOState = (
    [String], -- stdin
    [String], -- stdout
    [String], -- stderr
    [String]  -- interleaved stdout + stderr
  )
type TestIO a = State TestIOState a

instance InputOutput (State TestIOState) where
  output o = do
    (stdout, stdin, stderr, iout) <- get
    put (stdout++[o], stdin, stderr, iout++[o])
    return ()
  input = do
    (stdout, stdin, stderr, iout) <- get
    case stdin of
      [] -> return ""
      (i:is') -> put (stdout, is', stderr, iout) >> return i
  errorOutput e = do
    (stdout, stdin, stderr, iout) <- get
    put (stdout, stdin, stderr++[e], iout++[e])
    return ()

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
      let maybeUnitErr = invalidUnitError f actualUnits
      if isJust maybeUnitErr
        then do
          output $ show $ fromJust maybeUnitErr
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
    Nothing -> return f
resolveInputs f (UnaryOperatorApply _ e) = resolveInputs f e
resolveInputs f (BinaryOperatorApply _ e1 e2) = do
  f' <- resolveInputs f e1
  resolveInputs f' e2
resolveInputs f (FunctionApply _ e) = resolveInputs f e
resolveInputs f _ = return f

-- Evaluate the given expression to a vector under the given scope and give the
-- resulting vector, along with the augmented scope.
materializeExpr ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Expr
  -> m (Either ExecFail (ScopeFrame, Vector))
materializeExpr f expr
  | not $ allReferencesAreDefined f expr
  = return $ Left $ UnknownIdentifier "Expression refers to unknown identifier"
  | isJust $ maybeUnitsError = return $ Left $ fromJust maybeUnitsError
  | containsExponent expr = return $ Left $ InvalidVectorExpression exponentMsg
  | otherwise = do
    f' <- resolveInputs f expr
    case failedOperatorConstraints f expr of
      Just msg -> return $ Left $ UnsatisfiedConstraint msg
      Nothing -> case evaluate f' expr of
        Left err -> return $ Left $ ExecEvalFail err
        Right vec -> return $ Right (f', vec)
  where
    maybeUnitsError = invalidExprUnitsError f expr

-- Pop the scope in execution. Fail if we're in the top-level already.
execPop ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> m (Either ExecFail ScopeFrame)
execPop f = case popScope f of
  Just f' -> newFrame f'
  Nothing -> execFail $ InternalExecError "Tried to pop top-level closure."

executeStatement ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Statement
  -> m (Either ExecFail ScopeFrame)

-- No-op.
executeStatement f Comment = newFrame f

-- A dimension can be declared so long as its identifier is untaken and, if it
-- is defined by a dimspec, when that dimspec is valid.
executeStatement f (DeclareDimension dimensionId maybeDimspec) =
  if dimensionId `isLocalIdentifier` f || dimensionId `isStaticIdentifier` f
  then execFail
    $ RedefinedIdentifier
    $ "Identifier '" ++ dimensionId ++ "' is already defined."
  else case maybeDimspec of
    -- If it's a base dimension, just define it.
    Nothing -> newFrame $ f `withDimension` (dimensionId, empty)
    -- If it's an alias for a dimspec, then define if the dimspec validates.
    Just ds -> case invalidDimspecError f $ materializeDimspec f ds of
      Just err -> execFail err
      Nothing -> newFrame $ f `withDimensionAlias` (dimensionId, ds)

-- A unit can be declare so long as its identifier is untaken, and, if refers
-- to a dimension, that dimension is already defined.
executeStatement f (DeclareUnit id declOpts)
  -- Fail if the identifier already exists.
  | id `isLocalIdentifier` f || id `isStaticIdentifier` f
  = execFail $ RedefinedIdentifier $ redefinedMsg id
  -- If the unit states its dimension, but that dimension is unknown, then the
  -- declaration is not valid.
  | not $ isNothing maybeDimspecError = execFail $ fromJust maybeDimspecError
  -- If the dimspec is not monomorphically scalar, then it can't be used as the
  -- dimension of a unit.
  | (not $ isNothing maybeDim) && isNothing maybeDimspecDims
  = execFail $ InvalidUnitSpec nonScalarMsg
  -- Otherwise, it's valid.
  | otherwise = newFrame $ f `withUnit` (id, opts)
  where
    maybeDimspecError = maybeDim >>= invalidDimspecError f
    maybeDimspecDims = maybeDim >>= dimspecAsDimensionality
    redefinedMsg id = "Identifier '" ++ id ++ "' is already defined."
    unknownDimMsg
      = case maybeDim of {
          Just dim
            -> "Reference to undeclared dimension in '" ++ show dim ++ "'" }
    maybeDim
      = flip firstJust (toList declOpts)
      $ \case
          (UnitDimensionality d) -> Just $ materializeDimspec f d
          _ -> Nothing
    isNatural = NaturalUnitDecl `elem` declOpts
    opts = Set.fromList
      $   [ NaturalUnit | isNatural ]
      ++  [ UnitDim $ fromJust maybeDimspecDims | isJust maybeDimspecDims ]

    nonScalarMsg = "Unit dimension spec must be scalar"

-- A conversion can be defined so long as both units are already defined and are
-- of the same dimensionality.
executeStatement f (DeclareConversion from to transform)
  -- Are either units unknown.
  | isJust unknownFrom = execFail $ fromJust unknownFrom
  | isJust unknownTo = execFail $ fromJust unknownTo

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
    unknownFrom = invalidUnitError f from
    unknownTo = invalidUnitError f to

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
    noUnitMsg u = "Conversion refers to unknown unit '" ++ show u ++ "'"
    cyclicMsg = "Cannot declare conversion from a unit to itself"
    dimensionlessMsg = "Cannot convert dimensionless unit"
    mismatchMsg = "Cannot declare conversion between units of different " ++
      "dimensions: from '" ++ show fromDim ++ "' to '" ++ show toDim ++ "'"

-- An assignment can be defined so long as its identifier is untaken and every
-- reference identifier in its expression tree is already defined.
executeStatement f (Assignment id expr opts)
  -- Is the name already defined.
  | isLocalIdentifier id f = execFail $ RedefinedIdentifier $ redefinedMsg id

  -- Currently don't support exponents at this level. TODO: add support.
  | containsExponent expr = execFail $ InvalidVectorExpression exponentMsg

  -- Otherwise, it's valid if it can be evaluated.
  | otherwise = do
    r <- materializeExpr f expr
    case r of
      Left err -> execFail err
      Right (f', vec) -> if isNothing staticDims
        then execFail $ UnsatisfiableConstraint staticFailMsg
        else if not $ null $ failedConstraints $ fromJust staticDims
        then execFail
          $ UnsatisfiedConstraint
          $ head
          $ failedConstraints
          $ fromJust staticDims
        else case failedOperatorConstraints f' expr of
          Just msg -> execFail $ UnsatisfiedConstraint msg
          Nothing -> newFrame $ f' `withAssignment` (id, vec)
  where
    staticDims = staticEval f expr
    redefinedMsg id = "Identifier '" ++ id ++ "' is already defined"
    failedConstraints = failedAssignmentConstraints opts f
    staticFailMsg = "cannot statically determine dims of expression"

executeStatement f (Update id expr) = do
  case getAssignmentById f id of
    Nothing -> execFail $ UnknownIdentifier unknownIdMsg
    Just (_, oldVec) -> do
      r <- materializeExpr f expr
      case r of
        Left err -> execFail err
        Right (f', vec) -> if isNothing maybeNewDims
          then execFail $ UnsatisfiableConstraint staticFailMsg
          else if fromJust maybeNewDims /= getVectorDimensionality f oldVec
            then execFail $ UnsatisfiedConstraint mismatchMsg
            else newFrame $ replaceAssignment f' (id, vec)
  where
    unknownIdMsg = "Updating an unknown identifier."
    maybeNewDims = staticEval f expr
    staticFailMsg = "cannot statically determine dims of expression"
    mismatchMsg = "mismatched dimensions in update"

-- A print statement can be executed if every reference identifier in its
-- expression tree is already defined.
executeStatement f (Print expr opts) = do
  let units = getPrintUnits opts
  let maybeUnitsError = units >>= invalidUnitError f
  if isJust maybeUnitsError
    then execFail $ fromJust maybeUnitsError
    else do
      r <- materializeExpr f expr
      case r of
        Left err -> execFail err
        Right (f', vec) -> case convertForDisplay f' units vec of
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

executeStatement f (Relation id params e1 e2)
  | isExistingIdentifier id f = execFail $ RedefinedIdentifier $ redefinedMsg id
  | isJust invalidParamUnits = execFail $ fromJust invalidParamUnits
  | isJust invalidParamNames = execFail $ fromJust invalidParamNames
  | isJust invalid1 = execFail $ fromJust invalid1
  | isJust invalid2 = execFail $ fromJust invalid2
  | isJust reused = execFail $ fromJust reused
  | otherwise = newFrame $ f `withRelation` (id, asVectorMap e1' e2')
  where
    invalidParamUnits = invalidRelationParamUnits f params
    invalidParamNames = invalidRelationParamNames params
    invalid1 = invalidRelationRefError params e1
    invalid2 = invalidRelationRefError params e2
    reused = relationIdentifierReusedError e1 e2
    lookupU :: Identifier -> Units
    lookupU id = snd . fromJust $ find ((==id).fst) params
    e1' = mapRefs lookupU e1
    e2' = mapRefs lookupU e2
    redefinedMsg id = "Identifier '" ++ id ++ "' is already defined"

executeStatement f (Block p) = do
  r <- executeWith (pushScope f) p
  case r of
    Left err -> execFail err
    Right f' -> execPop f'

executeStatement f (Conditional expr aff maybeNeg) = do
  r <- materializeExpr f expr
  case r of
    Left err -> execFail err
    Right (f', vec) -> if staticEval f expr /= Just booleanDims
      then execFail $ UnsatisfiedConstraint conditionMsg
      else do
        r' <- executeWith (pushScope f')
          $ if vec /= logicalFalse then aff else neg
        case r' of
          Left err -> execFail err
          Right f'' -> execPop f''
  where
    neg = fromMaybe [] maybeNeg
    conditionMsg = "A conditional expression must be of boolean dimension"

executeStatement f s@(WhileLoop expr body) = do
  r <- materializeExpr f expr
  case r of
    Left err -> execFail err
    Right (f', vec) -> if vec == logicalFalse
      -- The loop is terminated, so the final result is this frame.
      then newFrame f'
      -- Otherwise we have at least one additional iteration.
      else do
        r' <- executeWith f' body
        case r' of
          Left err -> execFail err
          Right f'' -> executeStatement f'' s
