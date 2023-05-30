{-# LANGUAGE FlexibleInstances #-}

module Doggerel.IO (
    InputOutput(..),
    TestIO
  )
  where

import Control.Monad.State
import Control.Monad.State.Lazy

-- The InputOutput typeclass represents an IO system for the execution to use.
-- In this form, it acts as a generic wrapper for the IO monad's output, with a
-- writer monad alternative instance to allow tests to inspect output without
-- running in the IO monad.
class InputOutput m where
  output :: String -> m ()
  input :: m String
  errorOutput :: String -> m ()

-- The IO monad is the trivial instance.
instance InputOutput IO where
  output = putStrLn
  input = getLine
  errorOutput = putStrLn

-- TODO: Make this a record with accessors.
type TestIOState = (
    [String], -- stdin
    [String], -- stdout
    [String], -- stderr
    [String]  -- interleaved stdout + stderr
  )
type TestIO a = State TestIOState a

instance InputOutput (State TestIOState) where
  output o = do
    (stdout, stdin, stderr, iout) <- get
    put (stdout++[o], stdin, stderr, iout++[o])
    return ()
  input = do
    (stdout, stdin, stderr, iout) <- get
    case stdin of
      [] -> return ""
      (i:is') -> put (stdout, is', stderr, iout) >> return i
  errorOutput e = do
    (stdout, stdin, stderr, iout) <- get
    put (stdout, stdin, stderr++[e], iout++[e])
    return ()
