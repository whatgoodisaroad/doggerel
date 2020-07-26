module Main where

import Data.Map.Strict as Map
import Data.Set as Set (Set, fromList)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.Relation
import System.Exit (exitFailure)
import Test.HUnit

type TestExpr = ValueExpression String Quantity

solveForNegation = TestCase $ assertEqual "solve for negation" expected actual
  where
    expected, actual :: Maybe TestExpr
    expected = Just $ UnaryOperatorApply Negative $ Reference "b"
    actual
      = solveFor
        (UnaryOperatorApply Negative $ Reference "a")
        (Reference "b")
        "a"

solveForExponentiation
  = TestCase $ assertEqual "solve for exponent" expected actual
  where
    expected, actual :: Maybe TestExpr
    expected = Just $ UnaryOperatorApply (Exponent 3) $ Reference "b"
    actual
      = solveFor
        (UnaryOperatorApply (Exponent $ 1/3) $ Reference "a")
        (Reference "b")
        "a"

solveForAddLeft = TestCase $ assertEqual "solve for add left" expected actual
  where
    expected, actual :: Maybe TestExpr
    expected
      = Just $ BinaryOperatorApply Subtract (Reference "c") (Reference "b")
    actual
      = solveFor
        (BinaryOperatorApply Add (Reference "a") (Reference "b"))
        (Reference "c")
        "a"

solveForAddRight = TestCase $ assertEqual "solve for add right" expected actual
  where
    expected, actual :: Maybe TestExpr
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
    expected, actual :: Maybe TestExpr
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
    expected, actual :: Maybe TestExpr
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
    expected, actual :: Maybe TestExpr
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
    expected, actual :: Maybe TestExpr
    expected
      = Just $ BinaryOperatorApply Divide (Reference "c") (Reference "a")
    actual
      = solveFor
        (BinaryOperatorApply Multiply (Reference "a") (Reference "b"))
        (Reference "c")
        "b"

solveForDivideLeft
  = TestCase $ assertEqual "solve for divide left" expected actual
  where
    expected, actual :: Maybe TestExpr
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
    expected, actual :: Maybe TestExpr
    expected
      = Just $ BinaryOperatorApply Divide (Reference "a") (Reference "c")
    actual
      = solveFor
        (BinaryOperatorApply Divide (Reference "a") (Reference "b"))
        (Reference "c")
        "b"

asVectorMapTest = TestCase $ assertEqual "generate vector map" expected actual
  where
    expected, actual :: Map (Set String) (String, TestExpr)
    expected = Map.fromList [
        (
          Set.fromList ["b", "c"],
          ("a", BinaryOperatorApply Multiply (Reference "b") (Reference "c"))
        ),
        (
          Set.fromList ["a", "b"],
          ("c", BinaryOperatorApply Divide (Reference "a") (Reference "b"))
        ),
        (
          Set.fromList ["a", "c"],
          ("b", BinaryOperatorApply Divide (Reference "a") (Reference "c"))
        )
      ]
    actual =
      asVectorMap
        (Reference "a")
        (BinaryOperatorApply Multiply (Reference "b") (Reference "c"))

unitTests = [
    solveForNegation,
    solveForExponentiation,
    solveForAddLeft,
    solveForAddRight,
    solveForSubtractLeft,
    solveForSubtractRight,
    solveForMultiplyLeft,
    solveForMultiplyRight,
    solveForDivideLeft,
    solveForDivideRight,
    asVectorMapTest
  ]

main = do
  count <- runTestTT (TestList unitTests)
  if failures count > 0 then exitFailure else return ()
