module Doggerel.Statement.Assignment (
    declareAssignment,
    executeUpdate,
  ) where

import Data.Maybe (fromJust, isJust, isNothing)
import Data.Set as Set (empty)
import Doggerel.Ast (Statement(Assignment, Update))
import Doggerel.Eval (staticEval)
import Doggerel.IO (InputOutput)
import Doggerel.Scope
import Doggerel.Statement.Common (execFail, exponentMsg, materializeExpr, newFrame)
import Doggerel.Validation

declareAssignment ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Statement
  -> m (Either ExecFail ScopeFrame)
-- An assignment can be defined so long as its identifier is untaken and every
-- reference identifier in its expression tree is already defined.
declareAssignment f (Assignment id expr opts)

  -- Is the name already defined.
  | isLocalIdentifier id f = execFail $ RedefinedIdentifier $ redefinedMsg id

  -- Currently don't support exponents at this level. TODO: add support.
  | containsExponent expr = execFail $ InvalidVectorExpression exponentMsg

  -- Are the constraint dimensions valid
  | isJust invalidConstraints = execFail $ fromJust invalidConstraints

  -- Otherwise, it's valid if it can be evaluated.
  | otherwise = do
    r <- materializeExpr f expr
    case r of
      Left err -> execFail err
      Right (f', vec, dims) -> if isNothing staticDims
        then execFail $ UnsatisfiableConstraint staticFailMsg
        else if not $ null $ failedConstraints $ fromJust staticDims
        then execFail
          $ UnsatisfiedConstraint
          $ head
          $ failedConstraints
          $ fromJust staticDims
        else case failedOperatorConstraints f' expr of
          Just msg -> execFail $ UnsatisfiedConstraint msg
          Nothing -> newFrame $ f' `withAssignment` (id, vec, dims)
  where
    staticDims = staticEval f expr
    redefinedMsg id = "Identifier '" ++ id ++ "' is already defined"
    invalidConstraints = invalidAssignmentConstraints opts f
    failedConstraints = failedAssignmentConstraints opts f
    staticFailMsg = "cannot statically determine dims of expression"

executeUpdate ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Statement
  -> m (Either ExecFail ScopeFrame)
executeUpdate f (Update id expr) = do
  case getAssignmentById f id of
    Nothing -> execFail $ UnknownIdentifier unknownIdMsg
    Just (_, oldVec, _) -> do
      r <- materializeExpr f expr
      case r of
        Left err -> execFail err
        Right (f', vec, dims) -> if isNothing maybeNewDims
          then execFail $ UnsatisfiableConstraint staticFailMsg
          else if fromJust maybeNewDims /= getVectorDimensionality f oldVec
            then execFail $ UnsatisfiedConstraint mismatchMsg
            else newFrame $ replaceAssignment f' (id, vec, dims)
  where
    unknownIdMsg = "Updating an unknown identifier."
    maybeNewDims = staticEval f expr
    staticFailMsg = "cannot statically determine dims of expression"
    mismatchMsg = "mismatched dimensions in update"