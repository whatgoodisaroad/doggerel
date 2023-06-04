module Main where

import Control.Monad (when)
import Data.Map.Strict as Map (assocs, fromList, keys)
import Data.Set as Set (Set, empty, fromList, singleton)
import Doggerel.Ast
import Doggerel.Conversion
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.Eval
import Doggerel.Scope
import System.Exit (exitFailure)
import Test.HUnit

withPlainDimension :: ScopeFrame -> String -> ScopeFrame
withPlainDimension f d = f `withDimension` (d, Set.empty)

falseE, trueE :: Expr
falseE = Literal $ Scalar 0 $ toMap $ BaseUnit "bool" Nothing
trueE = Literal $ Scalar 1 $ toMap $ BaseUnit "bool" Nothing

tolerance :: Double
tolerance = 0.001

idToMaybeDim :: Identifier -> Maybe Dimensionality
idToMaybeDim = Just . toMap . mkDimension

idToUnitOpts :: Identifier -> Set UnitOptions
idToUnitOpts = Set.singleton . UnitDim . toMap . mkDimension

(~=) :: Vector -> Vector -> Bool
(Vector v1) ~= (Vector v2) = sameDims && all (< tolerance) deltas
  where
    sameDims = keys v1 == keys v2

    deltas :: [Double]
    deltas = zipWith
      (\m1 m2 -> abs $ m1 - m2)
      (map snd $ assocs v1)
      (map snd $ assocs v2)

isApproximately ::
     Vector
  -> Dimspec
  -> Either EvalFail (Vector, Dimspec)
  -> Bool
isApproximately v1 d1 vd2 = case vd2 of
  Left _ -> False
  Right (v2, d2) -> d1 == d2 && v1 ~= v2

scalarToAssignment ::
     Identifier
  -> Scalar
  -> (Identifier, Vector)
scalarToAssignment id s = (id, scalarToVector s)

booleanDimspec :: Dimspec
booleanDimspec = vecDimsToDimspec booleanDims

scalarLiteralExpression = TestCase
  $ assertEqual "scalar literal value" expected actual
  where
    f = initFrame
      `withPlainDimension` "length"
      `withPlainDimension` "time"
      `withUnit` ("mile", idToUnitOpts "length")
      `withUnit` ("hour", idToUnitOpts "time")
    s = Scalar 42
      $ toMap (BaseUnit "mile" Nothing) `divide` toMap (BaseUnit "hour" Nothing)
    expected = Right (
        scalarToVector s,
        DSProduct [
            DSTerm $ DSTermDim "length" Nothing 1,
            DSTerm $ DSTermDim "time" Nothing (-1)
          ]
      )
    actual = evaluate f $ Literal s

referenceExpression = TestCase
  $ assertEqual "reference expression" expected actual
  where
    s = Scalar 42
      $ toMap (BaseUnit "newton" Nothing)
        `divide` toMap (BaseUnit "meter" Nothing)
    f = initFrame
      `withPlainDimension` "force"
      `withPlainDimension` "length"
      `withUnit` ("newton", idToUnitOpts "force")
      `withUnit` ("meter", idToUnitOpts "length")
      `withAssignment` scalarToAssignment "x" s
    expected = Right $ (
        scalarToVector s,
        DSProduct [
            DSTerm $ DSTermDim "force" Nothing 1,
            DSTerm $ DSTermDim "length" Nothing (-1)
        ]
      )
    actual = evaluate f $ Reference "x"

u :: String -> Units
u = toMap . flip BaseUnit Nothing

d :: String -> Dimensionality
d = toMap . mkDimension

indexUnit :: Int -> Units
indexUnit = toMap . BaseUnit "index" . Just

indexDim :: Int -> Dimensionality
indexDim = toMap . Dimension "index" . Just

testFrame :: ScopeFrame
testFrame = initFrame
  `withPlainDimension` "length"
  `withPlainDimension` "time"
  `withUnit` ("second", idToUnitOpts "time")
  `withUnit` ("minute", idToUnitOpts "time")
  `withUnit` ("hour", idToUnitOpts "time")
  `withUnit` ("meter", idToUnitOpts "length")
  `withUnit` ("kilometer", idToUnitOpts "length")
  `withUnit` ("mile", idToUnitOpts "length")
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
    expected = Right (scalarToVector s, DSTerm $ DSTermDim "length" Nothing 1)
    actual = evaluate testFrame $ Reference "w"

addSameDimensionality = TestCase
  $ assertEqual "adding scalars of the same dimensionality" True
  $ isApproximately expectedV expectedD actual
  where
    expectedV
      = scalarToVector
      $ Scalar 1.44704 $ u "meter" `divide` u "second"
    expectedD = DSProduct [
        DSTerm $ DSTermDim "length" Nothing 1,
        DSTerm $ DSTermDim "time" Nothing (-1)
      ]
    actual
      = evaluate testFrame
      $ BinaryOperatorApply Add (Reference "x") (Reference "y")

subtractSameDimensionality = TestCase
  $ assertEqual "subtracting scalars of the same dimensionality" expected actual
  where
    expected
      = Right (
          scalarToVector $ Scalar 0.5 $ u "minute",
          DSTerm $ DSTermDim "time" Nothing 1
        )
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
      = Right (
        Vector
          $ Map.fromList [(u "meter" `divide` u "second", 1), (u "second", 32)],
          DSSum [
              DSProduct [
                  DSTerm $ DSTermDim "length" Nothing 1,
                  DSTerm $ DSTermDim "time" Nothing (-1)
                ],
              DSTerm $ DSTermDim "time" Nothing 1
            ]
      )
    actual
      = evaluate testFrame
      $ BinaryOperatorApply Add (Reference "x") (Reference "z")

cancellation = TestCase
  $ assertEqual "cancellation" True
  $ expected == actual
  where
    expected = Right (
        Vector $ Map.fromList [(u "mile", 42)],
        DSTerm $ DSTermDim "length" Nothing 1
      )
    speed = Literal $ Scalar 42 $ u "mile" `divide` u "hour"
    time = Literal $ Scalar 60 $ u "minute"
    actual = evaluate testFrame $ BinaryOperatorApply Multiply speed time

division = TestCase $ assertEqual "division" expected actual
  where
    expected = Right (
        Vector $ Map.fromList [(u "mile" `divide` u "hour", 10)],
        DSProduct [
            DSTerm $ DSTermDim "length" Nothing 1,
            DSTerm $ DSTermDim "time" Nothing (-1)
          ]
      )
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
    expected = Right (
        Vector $ Map.fromList [(u "second", -4), (u "meter", 5)],
        DSSum [
            DSTerm $ DSTermDim "length" Nothing 1,
            DSTerm $ DSTermDim "time" Nothing 1
          ]
      )
    actual
      = evaluate testFrame
      $ UnaryOperatorApply Negative
      $ BinaryOperatorApply Add
        (Literal $ Scalar 4 $ u "second")
        (Literal $ Scalar (-5) $ u "meter")

relationTest = TestCase $ assertEqual "relation application" expected actual
  where
    expected = Right (
        scalarToVector $ Scalar 120 $ u "dollar",
        DSTerm $ DSTermDim "money" Nothing 1
      )
    actual
      = evaluate relationFrame
      $ FunctionApply "testRelation"
      $ BinaryOperatorApply Add
        (Literal $ Scalar 10 $ u "kilometer")
        (Literal $ Scalar 2 $ u "second")
    relationFrame = testFrame
      `withPlainDimension` "money"
      `withUnit` ("dollar", idToUnitOpts "money")
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
        Right (logicalTrue, booleanDimspec),
        Right (logicalFalse, booleanDimspec),
        Right (logicalFalse, booleanDimspec),
        Right (logicalFalse, booleanDimspec)
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
        Right (logicalTrue, booleanDimspec),
        Right (logicalTrue, booleanDimspec),
        Right (logicalTrue, booleanDimspec),
        Right (logicalFalse, booleanDimspec)
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
        Right (logicalFalse, booleanDimspec),
        Right (logicalTrue, booleanDimspec)
      )
    applyTo e = evaluate testFrame $ UnaryOperatorApply LogicalNot e
    actual = (
        applyTo trueE,
        applyTo falseE
      )

inequalities = TestCase $ assertEqual "inequality ops" expected actual
  where
    expected = (
        Right (logicalFalse, booleanDimspec),
        Right (logicalTrue, booleanDimspec),
        Right (logicalTrue, booleanDimspec)
      )
    actual = (
        apply LessThan (Reference "x") (Reference "y"),
        apply GreaterThan (Reference "x") (Reference "y"),
        apply GreaterThanOrEqualTo (Reference "x") (Reference "x")
      )
    apply op e1 e2 = evaluate testFrame $ BinaryOperatorApply op e1 e2

inequalitiesMalformed =
  TestCase $ assertEqual "inequality ops misapplied" expected actual
  where
    expected = Left
      $ UnsatisfiableArgument "Cannot convert operand in inequality"
    actual = apply LessThan (Reference "w") (Literal $ Scalar 12 $ u "inch")
    disjointUnitsFrame = testFrame `withUnit` ("inch", idToUnitOpts "length")
    apply op e1 e2 = evaluate disjointUnitsFrame
      $ BinaryOperatorApply op e1 e2

naturalIndicesDecideEquality = TestCase
  $ assertEqual "equality of units involves natural indices" expected actual
  where
    u1 = toMap $ BaseUnit "nat" $ Just 1
    u2 = toMap $ BaseUnit "nat" $ Just 2
    expected = Right (
        Vector $ Map.fromList [(u1, 3), (u2, 3)],
        DSSum [
            DSTerm $ DSTermDim "nat" (Just 1) 1,
            DSTerm $ DSTermDim "nat" (Just 2) 1
          ]
      )
    actual
      = evaluate initFrame
      $ BinaryOperatorApply Add
        (Literal $ Scalar 1 u1)
        (BinaryOperatorApply Add
          (Literal $ Scalar 2 u1)
          (Literal $ Scalar 3 u2))

evaluateListLiteral
  = TestCase $ assertEqual "evaluate list literal" expected actual
  where
    expected = Right (
        Vector $ Map.fromList [
            (indexUnit 0 `multiply` u "meter" `divide` u "second",  1),
            (indexUnit 1 `multiply` u "second",                     32)
          ],
        DSSum [
            DSProduct [
                DSTerm $ DSTermDim "index" (Just 0) 1,
                DSTerm $ DSTermDim "length" Nothing 1,
                DSTerm $ DSTermDim "time" Nothing (-1)
              ],
            DSProduct [
                DSTerm $ DSTermDim "index" (Just 1) 1,
                DSTerm $ DSTermDim "time" Nothing 1
              ]
          ]
      )
    actual = evaluate testFrame $ ListLiteral [
        Reference "x",
        Reference "z"
      ]

staticLiteral
  = TestCase $ assertEqual "static analysis of literal" expected actual
  where
    expected = Just $ VecDims $ singleton $ d "length" `divide` d "time"
    actual
      = staticEval testFrame
      $ Literal $ Scalar 1337 $ u "mile" `divide` u "hour"

staticNaturalIndex
  = TestCase
  $ assertEqual "natural unit index statically becomes dimension index"
    expected actual
  where
    expected = Just $ VecDims $ singleton $ toMap $ Dimension "index" $ Just 2
    actual
      = staticEval testFrame
      $ Literal $ Scalar 1337 $ toMap $ BaseUnit "index" $ Just 2

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
    actual = (
        staticEval testFrame
          $ BinaryOperatorApply LogicalAnd undefined undefined,
        staticEval testFrame
          $ BinaryOperatorApply LogicalOr undefined undefined,
        staticEval testFrame $ UnaryOperatorApply LogicalNot undefined
      )
    expected = (Just booleanDims, Just booleanDims, Just booleanDims)

staticInequality = TestCase
  $ assertEqual "static analysis of inequality operators" expected actual
  where
    actual = (
        staticEval testFrame
          $ BinaryOperatorApply LessThan undefined undefined,
        staticEval testFrame
          $ BinaryOperatorApply GreaterThan undefined undefined,
        staticEval testFrame
          $ BinaryOperatorApply LessThanOrEqualTo undefined undefined,
        staticEval testFrame
          $ BinaryOperatorApply GreaterThanOrEqualTo undefined undefined
      )
    expected = (
        Just booleanDims,
        Just booleanDims,
        Just booleanDims,
        Just booleanDims
      )

staticAnalysisListLiteral = TestCase
  $ assertEqual "static analysis of list literal" expected actual
  where
    actual = staticEval testFrame $ ListLiteral [
        Reference "z",
        ListLiteral [
            Reference "z",
            Reference "w"
          ]
      ]
    expected = Just $ VecDims $ Set.fromList [
        d "time"    `multiply` indexDim 0,
        d "time"    `multiply` indexDim 0 `multiply` indexDim 1,
        d "length"  `multiply` indexDim 1 `multiply` indexDim 1
      ]

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
    inequalities,
    inequalitiesMalformed,
    naturalIndicesDecideEquality,
    evaluateListLiteral,

    -- staticEval
    staticLiteral,
    staticNaturalIndex,
    staticAssignment,
    staticInput,
    staticUnknownReference,
    staticAdd,
    staticMultiply,
    staticRelation,
    staticRelationNoMatch,
    staticLogic,
    staticInequality,
    staticAnalysisListLiteral
  ]

main = do
  count <- runTestTT (TestList unitTests)
  when (failures count > 0) exitFailure
