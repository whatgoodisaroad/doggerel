{-# LANGUAGE LambdaCase #-}

module Doggerel.Statement.Conversion (
    declareConversion
  ) where

import Data.Set as Set (fromList, toList)
import Data.Maybe (fromJust, isJust, isNothing)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.IO
import Doggerel.Scope
import Doggerel.Statement.Common (execFail, newFrame)
import Doggerel.Validation

declareConversion ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Statement
  -> m (Either ExecFail ScopeFrame)
-- A conversion can be defined so long as both units are already defined and are
-- of the same dimensionality.
declareConversion f (DeclareConversion from to transform)
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