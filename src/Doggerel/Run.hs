module Doggerel.Run where

import Control.Monad (unless, void, when)
import Data.List (find, isPrefixOf)
import Doggerel.Exec
import Doggerel.Parser
import Doggerel.Scope
import System.Environment (getArgs)
import System.IO

executeSource ::
     (Monad m, InputOutput m)
  => ScopeFrame
  -> String
  -> m ()
executeSource startFrame source = case parseFile source of
  Left failure -> output $ show failure
  Right ast -> void (executeWith startFrame ast)

loadedStandardFrame :: ScopeFrame -> Bool -> IO ScopeFrame
loadedStandardFrame startFrame printOut = do
  src <- readFile "libraries/standard.dog"
  let parsed = parseFile src
  result <- case parsed of
    Left failure -> fail $ "Encountered error parsing stdlib: " ++ show failure
    Right ast -> executeWith startFrame ast
  case result of
    Left failure -> fail
      $ "Encountered error executing stdlib: " ++ show failure
    Right frame -> do
      when printOut $ putStrLn "Loaded stdlib"
      return frame