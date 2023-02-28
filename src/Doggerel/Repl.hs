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
execRepl frame = do
  putStr "> "
  hFlush stdout
  line <- getLine

  let line' = if last line == ';' then line else line ++ ";"

  -- Intercept macros:

  -- The ":q" macro exits the REPL.
  if line == ":q"
  then return ()

  -- The ":dims <expr>" macro prints the statically evaluated vector
  -- dimensionality of the given expression.
  else if take 6 line == ":dims "
  then reflectDims frame (drop 6 line) >> execRepl frame

  else if head line == ':'
  then putStrLn "Error: Unrecognized macro" >> execRepl frame

  -- Otherwise, it's not a macro, so treat it as regular Doggerel code.
  else case parseFile line' of
    Left failure -> print failure >> execRepl frame
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
    putStr "\n"