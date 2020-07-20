module Main where

import Data.Map.Strict (assocs, fromList, keys)
import System.Exit (exitFailure)
import Test.HUnit

import Doggerel.Ast
import Doggerel.Conversion
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.Eval
import Doggerel.Scope

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

scalarToAssignment ::
     Identifier
  -> Scalar
  -> (Identifier, ValueExpression Identifier Scalar, Vector)
scalarToAssignment id s = (id, Literal s, scalarToVector s)

scalarLiteralExpression = TestCase
  $ assertEqual "scalar literal value" expected actual
  where
    f = initFrame
    s = Scalar 42
      $ (toMap $ BaseUnit "mile") `divide` (toMap $ BaseUnit "hour")
    expected = Right $ scalarToVector s
    actual = evaluate f $ Literal s

referenceExpression = TestCase
  $ assertEqual "reference expression" expected actual
  where
    f = initFrame `withAssignment` (scalarToAssignment "x" s)
    s = Scalar 42
      $ (toMap $ BaseUnit "newton") `divide` (toMap $ BaseUnit "meter")
    expected = Right $ scalarToVector s
    actual = evaluate f $ Reference "x"

u :: String -> Units
u = toMap . BaseUnit

testFrame :: ScopeFrame
testFrame = initFrame
  `withDimension` "length"
  `withDimension` "time"
  `withUnit` ("second", Just "time")
  `withUnit` ("minute", Just "time")
  `withUnit` ("hour", Just "time")
  `withUnit` ("meter", Just "length")
  `withUnit` ("kilometer", Just "length")
  `withUnit` ("mile", Just "length")
  `withConversion` ("kilometer", "meter", LinearTransform 1000)
  `withConversion` ("hour", "minute", LinearTransform 60)
  `withConversion` ("minute", "second", LinearTransform 60)
  `withConversion` ("mile", "kilometer", LinearTransform 1.60934)
  `withAssignment`
    (scalarToAssignment "x" (Scalar 1 (u "meter" `divide` u "second")))
  `withAssignment`
    (scalarToAssignment "y" (Scalar 1 (u "mile" `divide` u "hour")))
  `withAssignment` (scalarToAssignment "z" (Scalar 32 (u "second")))
  `withInput` ("w", Right $ Scalar 100 $ u "mile")

inputReferenceExpression = TestCase
  $ assertEqual "reference expression" expected actual
  where
    s = Scalar 100 $ u "mile"
    expected = Right $ scalarToVector s
    actual = evaluate testFrame $ Reference "w"

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

subtractSameDimensionality = TestCase
  $ assertEqual "subtracting scalars of the same dimensionality" expected actual
  where
    expected
      = Right
      $ scalarToVector
      $ Scalar 0.5 $ u "minute"
    actual
      = evaluate testFrame
      $ BinaryOperatorApply
        Subtract
        (Literal $ Scalar 2 $ u "minute")
        (Literal $ Scalar 90 $ u "second")

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

cancellation = TestCase
  $ assertEqual "cancellation" True
  $ expected == actual
  where
    expected = Right $ Vector $ fromList [(u "mile", 42)]
    speed = Literal $ Scalar 42 $ (u "mile") `divide` (u "hour")
    time = Literal $ Scalar 60 $ u "minute"
    actual = evaluate testFrame $ BinaryOperatorApply Multiply speed time

division = TestCase $ assertEqual "division" expected actual
  where
    expected = Right $ Vector $ fromList [((u "mile") `divide` (u "hour"), 10)]
    distance = Literal $ Scalar 1 $ u "mile"
    time = Literal $ Scalar 0.1 $ u "hour"
    actual = evaluate testFrame $ BinaryOperatorApply Divide distance time

divisionByZero = TestCase $ assertEqual "division by zero" expected actual
  where
    expected = Left DivideByZero
    distance = Literal $ Scalar 100 $ u "mile"
    time = Literal $ Scalar 0 $ u "hour"
    actual = evaluate testFrame $ BinaryOperatorApply Divide distance time

unitTests = [
    scalarLiteralExpression,
    referenceExpression,
    inputReferenceExpression,
    addSameDimensionality,
    subtractSameDimensionality,
    addDifferentDimensionalities,
    cancellation,
    division,
    divisionByZero
  ]

main = do
  count <- runTestTT (TestList unitTests)
  if failures count > 0 then exitFailure else return ()
