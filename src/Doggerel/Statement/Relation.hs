{-# LANGUAGE LambdaCase #-}

module Doggerel.Statement.Relation (
    declareRelation
  ) where

import Data.List (find)
import Data.Set as Set (fromList, toList)
import Data.Maybe (fromJust, isJust, isNothing)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.IO
import Doggerel.Relation (allRefsAreUnique, allRefsAreUniqueDims, asVectorMap)
import Doggerel.Scope
import Doggerel.Statement.Common (execFail, newFrame)
import Doggerel.Validation

declareRelation ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Statement
  -> m (Either ExecFail ScopeFrame)
declareRelation f (Relation id params e1 e2)
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