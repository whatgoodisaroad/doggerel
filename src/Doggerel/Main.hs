module Main where

import Control.Concurrent (threadDelay)
import Doggerel.Eval
import Doggerel.Exec
import Doggerel.Parser
import Doggerel.Scope
import System.Environment (getArgs)
import System.IO

executeFromStdin :: IO ()
executeFromStdin = do
  source <- getContents
  case parseFile source of
    Left failure -> print failure
    Right ast -> execute ast >> return ()

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

openRepl :: IO ()
openRepl = do
  putStrLn " Initializing Doggerel repl..."
  putStrLn $ "╒╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╤╕"
  threadDelay segmentDelay
  putStr "╵0   "
  hFlush stdout
  threadDelay segmentDelay
  putStr "╵⅙   "
  hFlush stdout
  threadDelay segmentDelay
  putStr "╵⅔   "
  hFlush stdout
  threadDelay segmentDelay
  putStr "╵½   "
  hFlush stdout
  threadDelay segmentDelay
  putStr "╵⅔   "
  hFlush stdout
  threadDelay segmentDelay
  putStr "╵⅚   "
  hFlush stdout
  threadDelay segmentDelay
  putStrLn "╵1"
  threadDelay segmentDelay
  putStrLn "Ready"
  execRepl initFrame

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--stdin"] -> executeFromStdin
    ["--repl"] -> openRepl
    _ -> do
      putStrLn "Unrecognized args"
      putStrLn "valid options are either --stdin or --repl"
