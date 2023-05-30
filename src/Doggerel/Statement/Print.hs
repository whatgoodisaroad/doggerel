module Doggerel.Statement.Print (
    executePrint,
  ) where

import Data.Maybe (fromJust, isJust, isNothing)
import Data.Set as Set (empty, insert)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.Eval
import Doggerel.IO (InputOutput(output))
import Doggerel.Output (prettyPrint)
import Doggerel.Scope
import Doggerel.Statement.Common (execFail, exponentMsg, materializeExpr, newFrame)
import Doggerel.Validation

executePrint ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Statement
  -> m (Either ExecFail ScopeFrame)

-- A print statement can be executed if every reference identifier in its
-- expression tree is already defined.
executePrint f (Print expr opts) = do
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
