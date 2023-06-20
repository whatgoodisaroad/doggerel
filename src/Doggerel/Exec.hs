{-# LANGUAGE LambdaCase #-}

module Doggerel.Exec (
    ExecFail(..),
    InputOutput(..),
    TestIO,
    execute,
    executeWith
  ) where

import Data.Maybe (fromMaybe)
import Doggerel.Ast
import Doggerel.Core (booleanDims, logicalFalse)
import Doggerel.Eval (staticEval)
import Doggerel.IO (InputOutput (errorOutput, output), TestIO)
import Doggerel.Statement.Assignment (declareAssignment, executeUpdate)
import Doggerel.Statement.Common (execFail, materializeExpr, newFrame)
import Doggerel.Statement.Conversion (declareConversion)
import Doggerel.Statement.Dim (declareDimension)
import Doggerel.Statement.Print (executePrint)
import Doggerel.Statement.Relation (declareRelation)
import Doggerel.Statement.Unit (declareUnit)
import Doggerel.Scope (ScopeFrame, initFrame, popScope, pushScope)
import Doggerel.Validation

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

executeStatement f Comment = newFrame f
executeStatement f s@(DeclareDimension _ _) = declareDimension f s
executeStatement f s@(DeclareUnit _ _) = declareUnit f s
executeStatement f s@(DeclareConversion _ _ _) = declareConversion f s
executeStatement f s@(Assignment _ _ _) = declareAssignment f s
executeStatement f s@(Update _ _) = executeUpdate f s
executeStatement f s@(Print _ _) = executePrint f s
executeStatement f s@(Input _ _) = executePrint f s
executeStatement f s@(Relation _ _ _ _) = declareRelation f s

executeStatement f (Block p) = do
  r <- executeWith (pushScope f) p
  case r of
    Left err -> execFail err
    Right f' -> execPop f'

executeStatement f (Conditional expr aff maybeNeg) = do
  r <- materializeExpr f expr
  case r of
    Left err -> execFail err
    Right (f', vec, _) -> if staticEval f expr /= Just booleanDims
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
    Right (f', vec, _) -> if vec == logicalFalse
      -- The loop is terminated, so the final result is this frame.
      then newFrame f'
      -- Otherwise we have at least one additional iteration.
      else do
        r' <- executeWith f' body
        case r' of
          Left err -> execFail err
          Right f'' -> executeStatement f'' s
