module Main where

import System.Exit (exitFailure)
import Test.HUnit

import Doggerel.Ast
import Doggerel.Conversion
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.Eval
import Doggerel.Exec

declareDim = TestCase
  $ assertEqual "declare a dim" expected actual
  where
    frame = initFrame
    expected = 3
    actual = 4

unitTests = [
    declareDim
  ]

main = do
  count <- runTestTT (TestList unitTests)
  if failures count > 0 then exitFailure else return ()
