module Main where

import Control.Monad (when)
import Data.Map.Strict as Map (assocs, fromList, keys)
import Data.Set as Set (fromList, singleton)
import Doggerel.Ast
import Doggerel.Conversion
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.Eval
import Doggerel.Scope
import System.Exit (exitFailure)
import Test.HUnit

falseE, trueE :: Expr
falseE = Literal $ Scalar 0 $ toMap $ BaseUnit "bool"
trueE = Literal $ Scalar 1 $ toMap $ BaseUnit "bool"

tolarance :: Double
tolarance = 0.001

idToMaybeDim :: Identifier -> Maybe Dimensionality
idToMaybeDim = Just . toMap . Dimension

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
  -> (Identifier, ValueExpression Identifier Scalar)
scalarToAssignment id s = (id, Literal s)

scalarLiteralExpression = TestCase
  $ assertEqual "scalar literal value" expected actual
  where
    f = initFrame
    s = Scalar 42
      $ toMap (BaseUnit "mile") `divide` toMap (BaseUnit "hour")
    expected = Right $ scalarToVector s
    actual = evaluate f $ Literal s

referenceExpression = TestCase
  $ assertEqual "reference expression" expected actual
  where
    f = initFrame `withAssignment` scalarToAssignment "x" s
    s = Scalar 42
      $ toMap (BaseUnit "newton") `divide` toMap (BaseUnit "meter")
    expected = Right $ scalarToVector s
    actual = evaluate f $ Reference "x"

u :: String -> Units
u = toMap . BaseUnit

d :: String -> Dimensionality
d = toMap . Dimension

testFrame :: ScopeFrame
testFrame = initFrame
  `withDimension` "length"
  `withDimension` "time"
  `withUnit` ("second", idToMaybeDim "time")
  `withUnit` ("minute", idToMaybeDim "time")
  `withUnit` ("hour", idToMaybeDim "time")
  `withUnit` ("meter", idToMaybeDim "length")
  `withUnit` ("kilometer", idToMaybeDim "length")
  `withUnit` ("mile", idToMaybeDim "length")
  `withConversion` (u "kilometer", u "meter", LinearTransform 1000)
  `withConversion` (u "hour", u "minute", LinearTransform 60)
  `withConversion` (u "minute", u "second", LinearTransform 60)
  `withConversion` (u "mile", u "kilometer", LinearTransform 1.60934)
  `withAssignment`
    scalarToAssignment "x" (Scalar 1 (u "meter" `divide` u "second"))
  `withAssignment`
    scalarToAssignment "y" (Scalar 1 (u "mile" `divide` u "hour"))
  `withAssignment` scalarToAssignment "z" (Scalar 32 (u "second"))
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
      $ Map.fromList [(u "meter" `divide` u "second", 1), (u "second", 32)]
    actual
      = evaluate testFrame
      $ BinaryOperatorApply Add (Reference "x") (Reference "z")

cancellation = TestCase
  $ assertEqual "cancellation" True
  $ expected == actual
  where
    expected = Right $ Vector $ Map.fromList [(u "mile", 42)]
    speed = Literal $ Scalar 42 $ u "mile" `divide` u "hour"
    time = Literal $ Scalar 60 $ u "minute"
    actual = evaluate testFrame $ BinaryOperatorApply Multiply speed time

division = TestCase $ assertEqual "division" expected actual
  where
    expected
      = Right $ Vector $ Map.fromList [(u "mile" `divide` u "hour", 10)]
    distance = Literal $ Scalar 1 $ u "mile"
    time = Literal $ Scalar 0.1 $ u "hour"
    actual = evaluate testFrame $ BinaryOperatorApply Divide distance time

divisionByZero = TestCase $ assertEqual "division by zero" expected actual
  where
    expected = Left DivideByZero
    distance = Literal $ Scalar 100 $ u "mile"
    time = Literal $ Scalar 0 $ u "hour"
    actual = evaluate testFrame $ BinaryOperatorApply Divide distance time

negation = TestCase $ assertEqual "negated vector" expected actual
  where
    expected = Right $ Vector $ Map.fromList [(u "second", -4), (u "meter", 5)]
    actual
      = evaluate testFrame
      $ UnaryOperatorApply Negative
      $ BinaryOperatorApply Add
        (Literal $ Scalar 4 $ u "second")
        (Literal $ Scalar (-5) $ u "meter")

relationTest = TestCase $ assertEqual "relation application" expected actual
  where
    expected = Right $ scalarToVector $ Scalar 120 $ u "dollar"
    actual
      = evaluate relationFrame
      $ FunctionApply "testRelation"
      $ BinaryOperatorApply Add
        (Literal $ Scalar 10 $ u "kilometer")
        (Literal $ Scalar 2 $ u "second")
    relationFrame = testFrame
      `withDimension` "money"
      `withUnit` ("dollar", idToMaybeDim "money")
      `withRelation` testRelation
    testRelation = ("testRelation", Map.fromList [
        (
          Set.fromList [u "kilometer", u "second"],
          (
            u "dollar",
            BinaryOperatorApply
              Add
              (UnaryOperatorApply
                (Exponent 2)
                (Reference $ u "kilometer"))
              (BinaryOperatorApply
                Multiply
                (Literal 10)
                (Reference $ u "second"))
          )
        )
      ])

logicalAnd = TestCase $ assertEqual "logical and" expected actual
  where
    expected = (
        Right logicalTrue,
        Right logicalFalse,
        Right logicalFalse,
        Right logicalFalse
      )
    applyTo e1 e2 = evaluate testFrame $ BinaryOperatorApply LogicalAnd e1 e2
    actual = (
        applyTo trueE trueE,
        applyTo trueE falseE,
        applyTo falseE trueE,
        applyTo falseE falseE
      )

logicalOr = TestCase $ assertEqual "logical or" expected actual
  where
    expected = (
        Right logicalTrue,
        Right logicalTrue,
        Right logicalTrue,
        Right logicalFalse
      )
    applyTo e1 e2 = evaluate testFrame $ BinaryOperatorApply LogicalOr e1 e2
    actual = (
        applyTo trueE trueE,
        applyTo trueE falseE,
        applyTo falseE trueE,
        applyTo falseE falseE
      )

logicalNot = TestCase $ assertEqual "logical not" expected actual
  where
    expected = (
        Right logicalFalse,
        Right logicalTrue
      )
    applyTo e = evaluate testFrame $ UnaryOperatorApply LogicalNot e
    actual = (
        applyTo trueE,
        applyTo falseE
      )

staticLiteral
  = TestCase $ assertEqual "static analysis of literal" expected actual
  where
    expected = Just $ VecDims $ singleton $ d "length" `divide` d "time"
    actual
      = staticEval testFrame
      $ Literal $ Scalar 1337 $ u "mile" `divide` u "hour"

staticAssignment
  = TestCase $ assertEqual "static analysis of assignment" expected actual
  where
    expected = Just $ VecDims $ singleton $ d "time"
    actual = staticEval testFrame $ Reference "z"

staticInput
  = TestCase $ assertEqual "static analysis of input" expected actual
  where
    expected = Just $ VecDims $ singleton $ d "length"
    actual = staticEval testFrame $ Reference "w"

staticUnknownReference
  = TestCase $ assertEqual "static analysis of bad ref" expected actual
  where
    expected = Nothing
    actual = staticEval testFrame $ Reference "hello"

staticAdd
  = TestCase $ assertEqual "static analysis of addition" expected actual
  where
    expected = Just $ VecDims $ Set.fromList [d "length", d "time"]
    actual
      = staticEval testFrame
      $ BinaryOperatorApply Add (Reference "w") (Reference "z")

staticMultiply
  = TestCase $ assertEqual "static analysis of multiplication" expected actual
  where
    expected = Just $ VecDims $ Set.fromList [
        (d "length" `multiply` d "length") `divide` d "time",
        d "length"
      ]
    actual
      = staticEval testFrame
      $ BinaryOperatorApply Multiply
        (Reference "x")
        (BinaryOperatorApply Add (Reference "w") (Reference "z"))

staticRelation
  = TestCase $ assertEqual "static analysis of relation" expected actual
  where
    expected = Just $ VecDims $ singleton $ d "length"
    actual
      = staticEval frame
      $ FunctionApply "myRel" $ Reference "z"
    frame = testFrame `withRelation` (
        "myRel",
        Map.fromList [
            (Set.fromList [u "minute"], (u "mile", Reference $ u "second"))
          ]
      )

staticRelationNoMatch
  = TestCase
  $ assertEqual "static analysis of relation without match" expected actual
  where
    expected = Nothing
    actual
      = staticEval frame
      $ FunctionApply "myRel" $ Reference "w"
    frame = testFrame `withRelation` (
        "myRel",
        Map.fromList [
            (Set.fromList [u "minute"], (u "mile", Reference $ u "second"))
          ]
      )

staticLogic = TestCase
  $ assertEqual "static analysis of logical operators logical" expected actual
  where
    expected = (
        staticEval testFrame
          $ BinaryOperatorApply LogicalAnd undefined undefined,
        staticEval testFrame
          $ BinaryOperatorApply LogicalOr undefined undefined,
        staticEval testFrame $ UnaryOperatorApply LogicalNot undefined
      )
    actual = (Just booleanDims, Just booleanDims, Just booleanDims)

unitTests = [
    scalarLiteralExpression,
    referenceExpression,
    inputReferenceExpression,
    addSameDimensionality,
    subtractSameDimensionality,
    addDifferentDimensionalities,
    cancellation,
    division,
    divisionByZero,
    negation,
    relationTest,
    logicalAnd,
    logicalOr,
    logicalNot,

    -- staticEval
    staticLiteral,
    staticAssignment,
    staticInput,
    staticUnknownReference,
    staticAdd,
    staticMultiply,
    staticRelation,
    staticRelationNoMatch,
    staticLogic
  ]

main = do
  count <- runTestTT (TestList unitTests)
  when (failures count > 0) exitFailure
