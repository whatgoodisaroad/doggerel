module Main where

import Control.Monad (unless, void, when)
import Data.List (find, isPrefixOf)
import Doggerel.Exec
import Doggerel.Parser
import Doggerel.Repl
import Doggerel.Run (executeSource, loadedStandardFrame)
import Doggerel.Scope
import System.Environment (getArgs)
import System.IO

getInputPath :: [String] -> Maybe String
getInputPath = find $ not . isPrefixOf "--"

main :: IO ()
main = do
  args <- getArgs
  let useRepl = "--repl" `elem` args
  let useAscii = "--ascii" `elem` args
  when useRepl $ greetingHeader useAscii
  let startFrame = if useAscii
      then initFrame `withPragma` AsciiOutput else initFrame
  loadedFrame <- if "--stdlib" `elem` args
      then loadedStandardFrame startFrame useRepl
      else return startFrame
  when useRepl $ do
    putStrLn "Ready (:help for commands)"
  if useRepl
    then execRepl loadedFrame
    else do
      source <- case getInputPath args of
        Nothing -> getContents
        Just p -> readFile p
      executeSource loadedFrame source
