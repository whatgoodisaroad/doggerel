module Main where

import System.Exit (exitFailure)
import Test.HUnit

import Doggerel.Conversion

unitTests = []

main = do
  count <- runTestTT (TestList unitTests)
  if failures count > 0 then exitFailure else return ()
