{-# LANGUAGE LambdaCase #-}

module Doggerel.Statement.Unit (
    declareUnit    
  ) where

import Data.Set as Set (fromList, toList)
import Data.Maybe (fromJust, isJust, isNothing)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.IO
import Doggerel.Scope
import Doggerel.Statement.Common (execFail, newFrame)
import Doggerel.Validation

-- A unit can be declare so long as its identifier is untaken, and, if refers
-- to a dimension, that dimension is already defined.
declareUnit ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Statement
  -> m (Either ExecFail ScopeFrame)
declareUnit f (DeclareUnit id declOpts)
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