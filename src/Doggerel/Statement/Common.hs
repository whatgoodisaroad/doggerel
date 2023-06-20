module Doggerel.Statement.Common (
    execFail,
    exponentMsg,
    materializeExpr,
    newFrame
  ) 
  where

import Data.Maybe (fromJust, isJust)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.Eval (evaluate)
import Doggerel.IO
import Doggerel.ParserUtils (scalarLiteralP)
import Doggerel.Scope
import Doggerel.Validation
import Text.ParserCombinators.Parsec (eof, parse)

newFrame ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> m (Either ExecFail ScopeFrame)
newFrame = return . Right

execFail ::
     (Monad m, InputOutput m)
  => ExecFail
  -> m (Either ExecFail ScopeFrame)
execFail = return . Left

-- Evaluate the given expression to a vector under the given scope and give the
-- resulting vector, along with the augmented scope.
materializeExpr ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Expr
  -> m (Either ExecFail (ScopeFrame, Vector, Dimspec))
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
        Right (vec, dims) -> return $ Right (f', vec, dims)
  where
    maybeUnitsError = invalidExprUnitsError f expr

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
