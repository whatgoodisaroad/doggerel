module Main where

import System.Exit (exitFailure)
import Test.HUnit

import Doggerel.Ast
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.Eval

scalarLiteralExpression = TestCase
  $ assertEqual "scalar literal value" expected actual
  where
    f = initFrame
    s = Scalar 42
      $ (toMap $ BaseUnit "mile") `divide` (toMap $ BaseUnit "hour")
    expected = Right $ scalarToVector s
    actual = evaluate f $ ScalarLiteral s

referenceExpression = TestCase
  $ assertEqual "reference expression" expected actual
  where
    f = Frame [] [] [] [("x", ScalarLiteral s)]
    s = Scalar 42
      $ (toMap $ BaseUnit "newton") `divide` (toMap $ BaseUnit "meter")
    expected = Right $ scalarToVector s
    actual = evaluate f $ Reference "x"

unitTests = [
    scalarLiteralExpression,
    referenceExpression
  ]

main = do
  count <- runTestTT (TestList unitTests)
  if failures count > 0 then exitFailure else return ()
