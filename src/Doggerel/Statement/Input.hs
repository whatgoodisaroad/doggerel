{-# LANGUAGE LambdaCase #-}

module Doggerel.Statement.Input (
    declareInput
  ) where

import Data.Set as Set (fromList, toList)
import Data.Maybe (fromJust, isJust, isNothing)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.IO
import Doggerel.Scope
import Doggerel.Statement.Common (execFail, newFrame)
import Doggerel.Validation

declareInput ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Statement
  -> m (Either ExecFail ScopeFrame)
declareInput f (Input id dims)
  | isExistingIdentifier id f = execFail $ RedefinedIdentifier $ redefinedMsg id
  | not $ allDimensionsAreDefined f dims
  = execFail $ UnknownIdentifier "Expression refers to unknown dimensions"
  | otherwise = newFrame $ f `withInput` (id, Left dims)
  where
    redefinedMsg id = "Identifier '" ++ id ++ "' is already defined"