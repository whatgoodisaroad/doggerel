module Main where

import Control.Monad (unless, void, when)
import Data.List (find, isPrefixOf)
import Doggerel.Exec
import Doggerel.Parser
import Doggerel.Repl
import Doggerel.Scope
import System.Environment (getArgs)
import System.IO

executeSource :: ScopeFrame -> String -> IO ()
executeSource startFrame source = case parseFile source of
  Left failure -> print failure
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
    putStrLn "Ready (:q to quit)"
  if useRepl
    then execRepl loadedFrame
    else do
      source <- case getInputPath args of
        Nothing -> getContents
        Just p -> readFile p
      executeSource loadedFrame source
