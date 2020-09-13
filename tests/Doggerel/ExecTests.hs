module Main where

import Control.Monad (when)
import Control.Monad.State.Lazy (runState)
import Data.Map.Strict as Map
import Data.Set as Set (empty, fromList)
import Doggerel.Ast
import Doggerel.Conversion
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.Eval
import Doggerel.Exec
import Doggerel.Scope
import System.Exit (exitFailure)
import Test.HUnit

u :: String -> Units
u = toMap . BaseUnit

falseScalar, trueScalar :: Scalar
falseScalar = Scalar 0 $ toMap $ BaseUnit "bool"
trueScalar = Scalar 1 $ toMap $ BaseUnit "bool"

idToMaybeDim :: Identifier -> Maybe Dimensionality
idToMaybeDim = Just . toMap . Dimension

scalarToAssignment ::
     Identifier
  -> Scalar
  -> (Identifier, ValueExpression Identifier Scalar)
scalarToAssignment id s = (id, Literal s)

runTestIOWithInputs :: [String] -> TestIO a -> (a, [String])
runTestIOWithInputs is t = case runState t ([], is) of (a, (o, i)) -> (a, o)

runTestIO :: TestIO a -> (a, [String])
runTestIO = runTestIOWithInputs []

declareDim = TestCase $ assertEqual "declare a dim" expected actual
  where
    expected = (Right $ initFrame `withDimension` "myDim", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [DeclareDimension "myDim"]

redefineDim = TestCase $ assertEqual "re-declare a dim fails" expected actual
  where
    expected
      = (Left $ RedefinedIdentifier "Identifier 'foo' is already defined.", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result
      = executeWith (initFrame `withDimension` "foo") [DeclareDimension "foo"]

declareUnitInDim
  = TestCase $ assertEqual "declare a unit in a dimension" expected actual
  where
    expected = (
        Right $ initFrame
          `withDimension` "length"
          `withUnit` ("mile", idToMaybeDim "length"),
        []
      )
    startFrame = initFrame `withDimension` "length"
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [DeclareUnit "mile" $ idToMaybeDim "length"]

declareDimensionlessUnit
  = TestCase $ assertEqual "declare a dimensionless unit" expected actual
  where
    expected = (Right $ initFrame `withUnit` ("potato", Nothing), [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith initFrame [DeclareUnit "potato" Nothing]

refefineUnit
  = TestCase $ assertEqual "redeclare unit fails" expected actual
  where
    expected
      = (Left $ RedefinedIdentifier "Identifier 'foo' is already defined.", [])
    startFrame = initFrame `withUnit` ("foo", Nothing)
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [DeclareUnit "foo" Nothing]

unknownUnitDim
  = TestCase $ assertEqual "declare unit with unknown dim fails" expected actual
  where
    expected
      = (
          Left $ UnknownIdentifier
            "Reference to undeclared dimension in 'bar'",
          []
        )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith initFrame [DeclareUnit "foo" $ idToMaybeDim "bar"]

declareConversion
  = TestCase $ assertEqual "declare a conversion" expected actual
  where
    units = [
        ("meter", idToMaybeDim "length"),
        ("kilometer", idToMaybeDim "length")
      ]
    startFrame = initFrame
      `withDimension` "length"
      `withUnit` head units
      `withUnit` (units !! 1)
    transform = LinearTransform 1000
    expectedFrame = initFrame
      `withDimension` "length"
      `withUnit` head units
      `withUnit` (units !! 1)
      `withConversion` (u "kilometer", u "meter", transform)
    expected = (Right expectedFrame, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareConversion (u "kilometer") (u "meter") transform
      ]

declareConversionUnknownTo
  = TestCase
  $ assertEqual "declare a dim with unknown to unit fails" expected actual
  where
    units = [("meter", idToMaybeDim "length")]
    startFrame = initFrame
      `withDimension` "length"
      `withUnit` head units
    transform = LinearTransform 1000
    expected = (
        Left $ UnknownIdentifier "Conversion refers to unkown unit 'kilometer'",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareConversion (u "kilometer") (u "meter") transform
      ]

declareConversionUnknownFrom
  = TestCase
  $ assertEqual "declare a dim with unknown to unit fails" expected actual
  where
    units = [("kilometer", idToMaybeDim "length")]
    startFrame = initFrame
      `withDimension` "length"
      `withUnit` head units
    transform = LinearTransform 1000
    expected = (
        Left $ UnknownIdentifier "Conversion refers to unkown unit 'meter'",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareConversion (u "kilometer") (u "meter") transform
      ]

declareCyclicConversion
  = TestCase
  $ assertEqual "declare a dim from unit to itself fails" expected actual
  where
    units = [("kilometer", idToMaybeDim "length")]
    startFrame = initFrame
      `withDimension` "length"
      `withUnit` head units
    transform = LinearTransform 1
    expected = (
        Left
          $ InvalidConversion "Cannot declare conversion from a unit to itself",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareConversion (u "kilometer") (u "kilometer") transform
      ]

declareConversionWithoutFromDim
  = TestCase
  $ assertEqual "declare a conversion without from dimensionality"
    expected actual
  where
    units = [("bar", idToMaybeDim "foo"), ("baz", Nothing)]
    startFrame = initFrame
      `withDimension` "foo"
      `withUnit` head units
      `withUnit` (units !! 1)
    transform = LinearTransform 42
    expected
      = (Left $ InvalidConversion "Cannot convert dimensionless unit", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result
      = executeWith startFrame [DeclareConversion (u "bar") (u "baz") transform]

declareConversionWithoutToDim
  = TestCase
  $ assertEqual "declare a conversion without from dimensionality"
    expected actual
  where
    units = [("bar", Nothing), ("baz", idToMaybeDim "foo")]
    startFrame = initFrame
      `withDimension` "foo"
      `withUnit` head units
      `withUnit` (units !! 1)
    transform = LinearTransform 42
    expected
      = (Left $ InvalidConversion "Cannot convert dimensionless unit", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result
      = executeWith startFrame [DeclareConversion (u "bar") (u "baz") transform]

declareConversionMismatchedDims
  = TestCase
  $ assertEqual "declare a conversion between different dimensions fails"
    expected actual
  where
    units = [("bar", idToMaybeDim "foo"), ("baz", idToMaybeDim "blah")]
    startFrame = initFrame
      `withDimension` "foo"
      `withUnit` head units
      `withUnit` (units !! 1)
    transform = LinearTransform 42
    expectedMsg = "Cannot declare conversion between units of different "
            ++ "dimensions: from 'foo' to 'blah'"
    expected = (Left $ InvalidConversion expectedMsg, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result
      = executeWith startFrame [DeclareConversion (u "bar") (u "baz") transform]

declareAssignment
  = TestCase $ assertEqual "declare an assignment" expected actual
  where
    scalar = Scalar 500 $ u "foo" `divide` u "bar"
    expectedFrame
      = initFrame
        `withUnit` ("foo", Nothing)
        `withUnit` ("bar", Nothing)
        `withAssignment` scalarToAssignment "baz" scalar
    expected = (Right expectedFrame, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Assignment "baz" (Literal scalar) Set.empty
      ]

redeclareAssignment
  = TestCase $ assertEqual "redeclare an assignment fails" expected actual
  where
    expr = Literal $ Scalar 500 $ u "foo" `divide` u "bar"
    expected
      = (Left $ RedefinedIdentifier "Identifier 'baz' is already defined", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Assignment "baz" expr Set.empty,
        Assignment "baz" expr Set.empty
      ]

assignmentWithUnknownReference
  = TestCase
  $ assertEqual "an assignment with unknown reference fails" expected actual
  where
    expected
      = (Left $ UnknownIdentifier "Expression refers to unknown identifier", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Assignment "baz" (Reference "doesNotExist") Set.empty
      ]

assignmentWithUnknownUnits
  = TestCase
  $ assertEqual "an assignment with unknown units fails" expected actual
  where
    expr = Literal $ Scalar 500 $ u "foo" `divide` u "doesNotExist"
    expected
      = (Left $ UnknownIdentifier "Expression refers to unknown units", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        Assignment "bar" expr Set.empty
      ]

assignmentViolatesScalarConstraint
  = TestCase
  $ assertEqual "assignment fails to be scalar" expected actual
  where
    expr = BinaryOperatorApply Add
      (Literal $ Scalar 12 $ u "foo")
      (Literal $ Scalar 12 $ u "bar")
    expected = (Left $ UnsatisfiedConstraint msg, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Assignment "baz" expr (Set.fromList [ConstrainedScalar])
      ]
    msg = "Constrained to scalar, but vector had multiple components:\n" ++
          "  actual: { bar, foo }"

assignmentViolatesDimensionConstraint
  = TestCase
  $ assertEqual "assignment fails to be scalar" expected actual
  where
    expr = Literal $ Scalar 12 $ u "foo"
    expected = (Left $ UnsatisfiedConstraint msg, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Assignment "baz" expr (Set.fromList [ConstrainedDimensionality target])
      ]
    target :: VectorDimensionality
    target = VecDims $ Set.fromList [toMap $ Dimension "bar"]
    msg = concat [
        "Vector does not match target dims:\n",
        "  target: { bar }\n",
        "  actual: { foo }"
      ]

assignmentContainsExponent
  = TestCase
  $ assertEqual "assignment with exponent fails" expected actual
  where
    expected = (Left $ InvalidVectorExpression msg, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        Assignment "baz" expr Set.empty
      ]
    expr = UnaryOperatorApply (Exponent 2) $ Literal $ Scalar 2 $ u "foo"
    msg = "Exponent operator not allowed in vector-valued expressions"

assignmentWellFormedLogic = TestCase
  $ assertEqual "assign well formed logical operator" expected actual
  where
    expected = (Right $ initFrame `withAssignment` ("foo", expr), [])
    actual = runTestIO result
    expr = BinaryOperatorApply LogicalAnd (Literal trueScalar)
      (Literal falseScalar)
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        Assignment "foo" expr Set.empty
      ]

assignmentMalformedLogic = TestCase
  $ assertEqual "assign malformed logical binary operator" expected actual
  where
    unit = ("foo", Nothing)
    expr = BinaryOperatorApply LogicalAnd (Literal trueScalar)
      (Literal $ Scalar 42 $ u "foo")
    expected = (
        Left $ UnsatisfiedConstraint
          $   "The logical and operator must be applied to a boolean vector, "
          ++  "but was applied to but was applied to: { foo }",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        Assignment "bar" expr Set.empty
      ]

assignmentMalformedUnaryLogic = TestCase
  $ assertEqual "assign malformed logical unary operator" expected actual
  where
    unit = ("foo", Nothing)
    expr = UnaryOperatorApply LogicalNot (Literal $ Scalar 42 $ u "foo")
    expected = (
        Left $ UnsatisfiedConstraint
          $   "The logical not operator must be applied to a boolean vector, "
          ++  "but was applied to but was applied to: { foo }",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        Assignment "bar" expr Set.empty
      ]

assignmentMalformedInequalityLogic = TestCase
  $ assertEqual "assign malformed logical binary operator" expected actual
  where
    unit = ("foo", Nothing)
    expr = BinaryOperatorApply LessThan (Literal trueScalar)
      (Literal $ Scalar 42 $ u "foo")
    expected = (
        Left $ UnsatisfiedConstraint
          $   "The unequality less-than operator must be applied to vectors "
          ++  "of the same dimensionality, but was applied to: "
          ++  "{ bool } and { foo }",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        Assignment "bar" expr Set.empty
      ]

printSimpleScalar = TestCase $ assertEqual "print simple scalar" expected actual
  where
    expected = (
        Right $ initFrame `withUnit` ("mile", Nothing),
        ["500.0 mile = {500.0 mile}"]
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "mile" Nothing,
        Print (Literal (Scalar 500 (u "mile"))) Nothing Set.empty
      ]

printScalarTargetUnits
  = TestCase
  $ assertEqual "print simple scalar with target units" expected actual
  where
    expected = (
        Right $ initFrame
          `withDimension` "length"
          `withUnit` ("meter", idToMaybeDim "length")
          `withUnit` ("kilometer", idToMaybeDim "length")
          `withConversion` (u "kilometer", u "meter", LinearTransform 1000),
        ["7.0 kilometer = {7000.0 meter}"]
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareDimension "length",
        DeclareUnit "meter" $ idToMaybeDim "length",
        DeclareUnit "kilometer" $ idToMaybeDim "length",
        DeclareConversion (u "kilometer") (u "meter") (LinearTransform 1000),
        Print
          (Literal (Scalar 7 (u "kilometer"))) (Just $ u "meter") Set.empty
      ]

printEvalFail = TestCase $ assertEqual "print failed eval" expected actual
  where
    expected = (Left $ ExecEvalFail DivideByZero, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "mile" Nothing,
        DeclareUnit "hour" Nothing,
        Print (
          BinaryOperatorApply
            Divide
            (Literal (Scalar 1 (u "mile")))
            (Literal (Scalar 0 (u "hour")))
          )
          Nothing
          Set.empty
      ]

printWithEvalFailure
  = TestCase
  $ assertEqual "an assignment that fails to eval" expected actual
  where
    expr = BinaryOperatorApply Divide
      (Literal $ Scalar 42 $ u "foo")
      (Literal $ Scalar 0 $ u "bar")
    expected = (Left $ ExecEvalFail DivideByZero, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Print expr Nothing Set.empty
      ]

printConstraintFail
  = TestCase $ assertEqual "print unsatisfieable target units" expected actual
  where
    expected
      = (Left $ UnsatisfiableConstraint "could not convert to units", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "mile" Nothing,
        DeclareUnit "hour" Nothing,
        Print (Literal (Scalar 500 (u "mile"))) (Just $ u "hour") Set.empty
      ]

printWithFractionOption
  = TestCase
  $ assertEqual "print as fraction" expected actual
  where
    expr = BinaryOperatorApply Add
      (Literal $ Scalar 3 $ u "foo")
      (BinaryOperatorApply Divide
        (Literal $ Scalar 5 $ u "bar")
        (Literal $ Scalar 2 $ u "baz"))
    expected = (
        Right $ initFrame
          `withUnit` ("foo", Nothing)
          `withUnit` ("bar", Nothing)
          `withUnit` ("baz", Nothing),
        [
          "                                ⎧ 2.5 bar           ⎫",
          "3.0 foo + (5.0 bar ÷ 2.0 baz) = ⎨ ─────── , 3.0 foo ⎬",
          "                                ⎩     baz           ⎭"
        ]
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        DeclareUnit "baz" Nothing,
        Print expr Nothing (Set.fromList [MultiLineFractions])
      ]

printWithFractionOptionButNoNegativeDegree
  = TestCase
  $ assertEqual "print as fraction w/o negative degrees" expected actual
  where
    expr = BinaryOperatorApply Add
      (Literal $ Scalar 12 $ u "foo")
      (Literal $ Scalar 12 $ u "bar")
    expected = (
        Right $ initFrame
          `withUnit` ("foo", Nothing)
          `withUnit` ("bar", Nothing),
        ["12.0 foo + 12.0 bar = {12.0 bar, 12.0 foo}"]
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Print expr Nothing (Set.fromList [MultiLineFractions])
      ]

printContainsExponent
  = TestCase
  $ assertEqual "print with exponent fails" expected actual
  where
    expected = (Left $ InvalidVectorExpression msg, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        Print expr Nothing Set.empty
      ]
    expr = UnaryOperatorApply (Exponent 2) $ Literal $ Scalar 2 $ u "foo"
    msg = "Exponent operator not allowed in vector-valued expressions"

inputSimple = TestCase $ assertEqual "simple input" expected actual
  where
    expected = (
        Right $ initFrame
          `withDimension` "length"
          `withUnit` ("mile", idToMaybeDim "length")
          `withInput` ("foo", Right $ Scalar 123.4 $ u "mile"),
        [
            "Enter a scalar of dimensionality {length}",
            "foo = {123.4 mile}"
          ]
      )
    actual = runTestIOWithInputs ["123.4 mile"] result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareDimension "length",
        DeclareUnit "mile" $ idToMaybeDim "length",
        Input "foo" (toMap $ Dimension "length"),
        Print (Reference "foo") Nothing Set.empty
      ]

inputParseRetry
  = TestCase $ assertEqual "simple input with bad parse" expected actual
  where
    expected = (
        Right $ initFrame
          `withDimension` "length"
          `withUnit` ("mile", idToMaybeDim "length")
          `withInput` ("foo", Right $ Scalar 123.4 $ u "mile"
            ),
        [
            "Enter a scalar of dimensionality {length}",
            "Failed to parse input as a scalar: \"fail\" (line 1, column 1):\n"
              ++ "unexpected \"b\"\n"
              ++ "expecting digit, \".\" or space",
            "Try again...",
            "Enter a scalar of dimensionality {length}",
            "foo = {123.4 mile}"
          ]
      )
    actual = runTestIOWithInputs ["bad input", "123.4 mile"] result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareDimension "length",
        DeclareUnit "mile" $ idToMaybeDim "length",
        Input "foo" (toMap $ Dimension "length"),
        Print (Reference "foo") Nothing Set.empty
      ]

inputUnknownUnitsRetry
  = TestCase $ assertEqual "simple input with unknown units" expected actual
  where
    expected = (
        Right $ initFrame
          `withDimension` "length"
          `withUnit` ("mile", idToMaybeDim "length")
          `withInput` ("foo", Right $ Scalar 123.4 $ u "mile"
            ),
        [
            "Enter a scalar of dimensionality {length}",
            "Unknown units: parsec",
            "Try again...",
            "Enter a scalar of dimensionality {length}",
            "foo = {123.4 mile}"
          ]
      )
    actual = runTestIOWithInputs ["12 parsec", "123.4 mile"] result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareDimension "length",
        DeclareUnit "mile" $ idToMaybeDim "length",
        Input "foo" (toMap $ Dimension "length"),
        Print (Reference "foo") Nothing Set.empty
      ]

inputMismatchedDimsRetry
  = TestCase $ assertEqual "simple input with mismatched dims" expected actual
  where
    expected = (
        Right $ initFrame
          `withDimension` "length"
          `withUnit` ("mile", idToMaybeDim "length")
          `withDimension` "mass"
          `withUnit` ("pound", idToMaybeDim "mass")
          `withInput` ("foo", Right $ Scalar 123.4 $ u "mile"
            ),
        [
            "Enter a scalar of dimensionality {length}",
            "Mismatched dimensionality",
            "  Expected: {length}",
            "     Found: {mass}",
            "Try again...",
            "Enter a scalar of dimensionality {length}",
            "foo = {123.4 mile}"
          ]
      )
    actual = runTestIOWithInputs ["42 pound", "123.4 mile"] result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareDimension "length",
        DeclareUnit "mile" $ idToMaybeDim "length",
        DeclareDimension "mass",
        DeclareUnit "pound" $ idToMaybeDim "mass",
        Input "foo" (toMap $ Dimension "length"),
        Print (Reference "foo") Nothing Set.empty
      ]

simpleRelation = TestCase $ assertEqual "simple relation" expected actual
  where
    expected = (
        Right $ initFrame
          `withUnit` ("foo", Nothing)
          `withUnit` ("bar", Nothing)
          `withUnit` ("baz", Nothing)
          `withRelation` ("f", Map.fromList [
              (
                Set.fromList [u "bar", u "baz"],
                (
                  u "foo",
                  BinaryOperatorApply
                    Multiply
                    (Reference $ u "bar")
                    (Reference $ u "baz")
                )
              ), (
                Set.fromList [u "foo", u "baz"],
                (
                  u "bar",
                  BinaryOperatorApply
                    Divide
                    (Reference $ u "foo")
                    (Reference $ u "baz")
                )
              ), (
                Set.fromList [u "foo", u "bar"],
                (
                  u "baz",
                  BinaryOperatorApply
                    Divide
                    (Reference $ u "foo")
                    (Reference $ u "bar")
                )
              )
            ]),
        ["f(12.0 foo + 4.0 bar) = {3.0 baz}"]
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        DeclareUnit "baz" Nothing,
        Relation
          "f"
          (Reference $ u "foo")
          (BinaryOperatorApply
            Multiply
            (Reference $ u "bar")
            (Reference $ u "baz")),
        Print
          (FunctionApply "f"
            (BinaryOperatorApply Add
              (Literal $ Scalar 12 $ u "foo")
              (Literal $ Scalar 4 $ u "bar")))
          Nothing
          Set.empty
      ]

exponentRelation = TestCase $ assertEqual "exponent relation" expected actual
  where
    expected = (
        Right $ initFrame
          `withUnit` ("foo", Nothing)
          `withUnit` ("bar", Nothing)
          `withRelation` ("g", Map.fromList [
              (
                Set.fromList [u "bar"],
                (
                  u "foo",
                  UnaryOperatorApply (Exponent 4) (Reference $ u "bar")
                )
              ), (
                Set.fromList [u "foo"],
                (
                  u "bar",
                  UnaryOperatorApply (Exponent 0.25) (Reference $ u "foo")
                )
              )
            ]),
        ["g(2.0 bar) = {16.0 foo}"]
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Relation
          "g"
          (Reference $ u "foo")
          (UnaryOperatorApply
            (Exponent 4)
            (Reference $ u "bar")),
        Print
          (FunctionApply "g"
            (Literal $ Scalar 2 $ u "bar"))
          Nothing
          Set.empty
      ]

relationRedefine =
  TestCase $ assertEqual "relation with used id" expected actual
  where
    expected =
      (Left $ RedefinedIdentifier "Identifier 'foo' is already defined", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Relation
          "foo"
          (Reference $ u "foo")
          (Reference $ u "bar")
      ]

relationUnknownUnits =
  TestCase $ assertEqual "relation with unknown unit" expected actual
  where
    expected =
      (Left $ UnknownIdentifier "Relation refers to unknown units", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        Relation
          "baz"
          (Reference $ u "foo")
          (Reference $ u "bar")
      ]

relationReusedUnits =
  TestCase $ assertEqual "relation with repeated unit" expected actual
  where
    expected =
      (Left $ RedefinedIdentifier "Units are repeated within relation.", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        Relation
          "baz"
          (Reference $ u "foo")
          (Reference $ u "foo")
      ]

unitTests = [
    -- dim
    declareDim,
    redefineDim,

    -- unit
    declareUnitInDim,
    declareDimensionlessUnit,
    refefineUnit,
    unknownUnitDim,

    -- convert
    declareConversion,
    declareConversionUnknownFrom,
    declareConversionUnknownTo,
    declareCyclicConversion,
    declareConversionWithoutFromDim,
    declareConversionWithoutToDim,
    declareConversionMismatchedDims,

    -- let
    declareAssignment,
    redeclareAssignment,
    assignmentWithUnknownReference,
    assignmentWithUnknownUnits,
    assignmentViolatesScalarConstraint,
    assignmentViolatesDimensionConstraint,
    assignmentContainsExponent,
    assignmentWellFormedLogic,
    assignmentMalformedLogic,
    assignmentMalformedUnaryLogic,
    assignmentMalformedInequalityLogic,

    -- print
    printSimpleScalar,
    printScalarTargetUnits,
    printEvalFail,
    printWithEvalFailure,
    printConstraintFail,
    printWithFractionOption,
    printWithFractionOptionButNoNegativeDegree,
    printContainsExponent,

    -- input
    inputSimple,
    inputParseRetry,
    inputUnknownUnitsRetry,
    inputMismatchedDimsRetry,

    -- relation
    simpleRelation,
    exponentRelation,
    relationRedefine,
    relationUnknownUnits,
    relationReusedUnits
  ]

main = do
  count <- runTestTT (TestList unitTests)
  when (failures count > 0) exitFailure
