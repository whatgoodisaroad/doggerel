module Doggerel.Repl (execRepl, greetingHeader) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Doggerel.Charset
import Doggerel.Eval
import Doggerel.Exec
import Doggerel.Parser
import Doggerel.Scope
import System.IO

reflectDims :: ScopeFrame -> String -> IO ()
reflectDims frame exprString = case parseExpression exprString of
  Left failure -> print failure
  Right expr -> case staticEval frame expr of
    Nothing -> print
      $ "Failed to statically evaluate expression: " ++ exprString
    Just dims -> putStrLn $ showForCharset charset dims
      where
        charset = if frame `hasPragma` AsciiOutput
          then AsciiCharset else UnicodeCharset

execRepl :: ScopeFrame -> IO ()
execRepl frame = execRepl' frame frame

execRepl' :: ScopeFrame -> ScopeFrame -> IO ()
execRepl' initialFrame frame = do
  putStr "> "
  hFlush stdout
  line <- getLine

  let continueWith = execRepl' initialFrame
  let continue = continueWith initialFrame
  let line' = if (not $ null line) && last line == ';'
      then line
      else line ++ ";"

  if line == ":help"
  then do
    putStrLn "The following macros are available in the repl:"
    putStrLn "- :q to quit"
    putStrLn "- :r to reload"
    putStrLn "- :dims <x> to print the dmensionality of x"
    putStrLn "- :help to print this message"
    putStrLn ""
    continue

  else if line == ":q"
  then return ()

  else if line == ":r"
  then putStrLn "Reloaded" >> continueWith initialFrame

  else if take 6 line == ":dims "
  then reflectDims frame (drop 6 line) >> continue

  else if head line' == ':'
  then putStrLn "Error: Unrecognized macro" >> continue

  else if null $ filter (not . flip elem " \t\n") line
  then continue

  -- Otherwise, it's not a macro, so treat it as regular Doggerel code.
  else case parseFile line' of
    Left failure -> print failure >> continue
    Right ast -> do
      result <- executeWith frame ast
      case result of
        Left err -> do
          print err
          continue
        Right frame' -> continueWith frame'

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
    putStr "\n"