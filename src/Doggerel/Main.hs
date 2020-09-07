module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, void, when)
import Doggerel.Eval
import Doggerel.Exec
import Doggerel.Parser
import Doggerel.Scope
import System.Environment (getArgs)
import System.IO

executeFromStdin :: ScopeFrame -> IO ()
executeFromStdin startFrame = do
  source <- getContents
  case parseFile source of
    Left failure -> print failure
    Right ast -> void (executeWith startFrame ast)

execRepl :: ScopeFrame -> IO ()
execRepl frame = do
  putStr "> "
  hFlush stdout
  line <- getLine
  if line == ":q"
    then return ()
    else case parseFile line of
      Left failure -> do
        print failure
        execRepl frame
      Right ast -> do
        result <- executeWith frame ast
        case result of
          Left err -> do
            print err
            execRepl frame
          Right frame' -> execRepl frame'

segmentDelay = 100000

printWithDelay :: String -> IO ()
printWithDelay s = threadDelay segmentDelay >> putStr s >> hFlush stdout

greetingHeader :: Bool -> IO ()
greetingHeader ascii = do
  unless ascii $ putStr " "
  putStrLn "Initializing Doggerel repl..."
  unless ascii $ do
    putStrLn "╒╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╕"
    mapM_ printWithDelay [
      "╵0   ", "╵⅙   ", "╵⅔   ", "╵½   ", "╵⅔   ", "╵⅚   ", "╵1"]
    threadDelay segmentDelay
    putStrLn "\n"

loadedStandardFrame :: ScopeFrame -> IO ScopeFrame
loadedStandardFrame startFrame = do
  src <- readFile "libraries/standard.dog"
  let parsed = parseFile src
  result <- case parsed of
    Left failure -> fail $ "Encountered error parsing stdlib: " ++ show failure
    Right ast -> executeWith startFrame ast
  case result of
    Left failure -> fail 
      $ "Encountered error executing stdlib: " ++ show failure
    Right frame -> do
      putStrLn "Loaded stdlib"
      return frame

main :: IO ()
main = do
  args <- getArgs
  let useRepl = "--repl" `elem` args
  let useAscii = "--ascii" `elem` args
  when useRepl $ greetingHeader useAscii
  let startFrame = if useAscii
      then initFrame `withPragma` AsciiOutput else initFrame
  loadedFrame <- if "--stdlib" `elem` args
      then loadedStandardFrame startFrame else return startFrame
  putStrLn "Ready"
  if useRepl
    then execRepl loadedFrame
    else executeFromStdin loadedFrame
