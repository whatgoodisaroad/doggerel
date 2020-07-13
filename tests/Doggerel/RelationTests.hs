module Main where

import Doggerel.Ast
import Doggerel.Relation
import System.Exit (exitFailure)
import Test.HUnit

solveForNegation = TestCase $ assertEqual "solve for negation" expected actual
  where
    expected = Just $ UnaryOperatorApply Negative $ Reference "b"
    actual
      = solveFor
        (UnaryOperatorApply Negative $ Reference "a")
        (Reference "b")
        "a"

solveForAddLeft = TestCase $ assertEqual "solve for add left" expected actual
  where
    expected
      = Just $ BinaryOperatorApply Subtract (Reference "c") (Reference "b")
    actual
      = solveFor
        (BinaryOperatorApply Add (Reference "a") (Reference "b"))
        (Reference "c")
        "a"

solveForAddRight = TestCase $ assertEqual "solve for add right" expected actual
  where
    expected
      = Just $ BinaryOperatorApply Subtract (Reference "c") (Reference "a")
    actual
      = solveFor
        (BinaryOperatorApply Add (Reference "a") (Reference "b"))
        (Reference "c")
        "b"

solveForSubtractLeft
  = TestCase $ assertEqual "solve for subtract left" expected actual
  where
    expected
      = Just $ BinaryOperatorApply Add (Reference "c") (Reference "b")
    actual
      = solveFor
        (BinaryOperatorApply Subtract (Reference "a") (Reference "b"))
        (Reference "c")
        "a"

solveForSubtractRight
  = TestCase $ assertEqual "solve for subtract right" expected actual
  where
    expected
      = Just $ BinaryOperatorApply Subtract (Reference "a") (Reference "c")
    actual
      = solveFor
        (BinaryOperatorApply Subtract (Reference "a") (Reference "b"))
        (Reference "c")
        "b"

solveForMultiplyLeft
  = TestCase $ assertEqual "solve for multiply left" expected actual
  where
    expected
      = Just $ BinaryOperatorApply Divide (Reference "c") (Reference "b")
    actual
      = solveFor
        (BinaryOperatorApply Multiply (Reference "a") (Reference "b"))
        (Reference "c")
        "a"

solveForMultiplyRight
  = TestCase $ assertEqual "solve for multiply right" expected actual
  where
    expected
      = Just $ BinaryOperatorApply Divide (Reference "a") (Reference "c")
    actual
      = solveFor
        (BinaryOperatorApply Multiply (Reference "a") (Reference "b"))
        (Reference "c")
        "b"

solveForDivideLeft
  = TestCase $ assertEqual "solve for divide left" expected actual
  where
    expected
      = Just $ BinaryOperatorApply Multiply (Reference "c") (Reference "b")
    actual
      = solveFor
        (BinaryOperatorApply Divide (Reference "a") (Reference "b"))
        (Reference "c")
        "a"

solveForDivideRight
  = TestCase $ assertEqual "solve for divide right" expected actual
  where
    expected
      = Just $ BinaryOperatorApply Divide (Reference "a") (Reference "c")
    actual
      = solveFor
        (BinaryOperatorApply Divide (Reference "a") (Reference "b"))
        (Reference "c")
        "b"

unitTests = [
    solveForNegation,
    solveForAddLeft,
    solveForAddRight,
    solveForSubtractLeft,
    solveForSubtractRight,
    solveForMultiplyLeft,
    solveForMultiplyRight,
    solveForDivideLeft,
    solveForDivideRight
  ]

main = do
  count <- runTestTT (TestList unitTests)
  if failures count > 0 then exitFailure else return ()
