module Doggerel.Statement.Dim (
    declareDimension    
  ) where

import Data.Set as Set (empty)
import Doggerel.Ast
import Doggerel.IO (InputOutput)
import Doggerel.Scope
import Doggerel.Statement.Common (execFail, newFrame)
import Doggerel.Validation

declareDimension ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> Statement
  -> m (Either ExecFail ScopeFrame)
-- A dimension can be declared so long as its identifier is untaken and, if it
-- is defined by a dimspec, when that dimspec is valid.
declareDimension f (DeclareDimension dimensionId maybeDimspec) =
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
