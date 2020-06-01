module Main where

import Data.Map.Strict (assocs, fromList, keys)
import System.Exit (exitFailure)
import Test.HUnit

import Doggerel.Ast
import Doggerel.Conversion
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.Eval

tolarance :: Double
tolarance = 0.001

(~=) :: Vector -> Vector -> Bool
(Vector v1) ~= (Vector v2) = sameDims && all (< tolarance) deltas
  where
    sameDims = keys v1 == keys v2

    deltas :: [Double]
    deltas = zipWith
      (\m1 m2 -> abs $ m1 - m2)
      (map snd $ assocs v1)
      (map snd $ assocs v2)

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

u :: String -> Units
u = toMap . BaseUnit

testFrame :: ScopeFrame
testFrame = Frame
  ["length", "time"]
  [
    ("second", Just "time"),
    ("minute", Just "time"),
    ("hour", Just "time"),
    ("meter", Just "length"),
    ("kilometer", Just "length"),
    ("mile", Just "length")
  ]
  [
    ("kilometer", "meter", LinearTransform 1000),
    ("hour", "minute", LinearTransform 60),
    ("minute", "second", LinearTransform 60),
    ("mile", "kilometer", LinearTransform 1.60934)
  ]
  [
    ("x", ScalarLiteral (Scalar 1 (u "meter" `divide` u "second"))),
    ("y", ScalarLiteral (Scalar 1 (u "mile" `divide` u "hour"))),
    ("z", ScalarLiteral (Scalar 32 (u "second")))
  ]

addSameDimensionality = TestCase
  $ assertEqual "adding scalars of the same dimensionality" (Right True)
  $ fmap (expected ~=) actual
  where
    expected
      = scalarToVector
      $ Scalar 1.44704 $ u "meter" `divide` u "second"
    actual
      = evaluate testFrame
      $ BinaryOperatorApply Add (Reference "x") (Reference "y")

addDifferentDimensionalities = TestCase
  $ assertEqual "adding scalars of the different dimensionalities" True
  $ expected == actual
  where
    expected
      = Right
      $ Vector
      $ fromList [(u "meter" `divide` u "second", 1), (u "second", 32)]
    actual
      = evaluate testFrame
      $ BinaryOperatorApply Add (Reference "x") (Reference "z")

unitTests = [
    scalarLiteralExpression,
    referenceExpression,
    addSameDimensionality,
    addDifferentDimensionalities
  ]

main = do
  count <- runTestTT (TestList unitTests)
  if failures count > 0 then exitFailure else return ()
