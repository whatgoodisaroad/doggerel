module Main where

import Control.Monad.State.Lazy (runState)
import Data.Set (empty, fromList)
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

scalarToAssignment ::
     Identifier
  -> Scalar
  -> (Identifier, ValueExpression, Vector)
scalarToAssignment id s = (id, ScalarLiteral s, scalarToVector s)

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
          `withUnit` ("mile", Just "length"),
        []
      )
    startFrame = initFrame `withDimension` "length"
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [DeclareUnit "mile" $ Just "length"]

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
      = (Left $ UnknownIdentifier "Reference to undeclared dimension 'bar'", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith initFrame [DeclareUnit "foo" (Just "bar")]

declareConversion
  = TestCase $ assertEqual "declare a conversion" expected actual
  where
    units = [("meter", Just "length"), ("kilometer", Just "length")]
    startFrame = initFrame
      `withDimension` "length"
      `withUnit` (units !! 0)
      `withUnit` (units !! 1)
    transform = LinearTransform 1000
    expectedFrame = initFrame
      `withDimension` "length"
      `withUnit` (units !! 0)
      `withUnit` (units !! 1)
      `withConversion` ("kilometer", "meter", transform)
    expected = (Right $ expectedFrame, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareConversion "kilometer" "meter" transform
      ]

declareConversionUnknownTo
  = TestCase
  $ assertEqual "declare a dim with unknown to unit fails" expected actual
  where
    units = [("meter", Just "length")]
    startFrame = initFrame
      `withDimension` "length"
      `withUnit` (units !! 0)
    transform = LinearTransform 1000
    expected = (
        Left $ UnknownIdentifier "Conversion refers to unkown unit 'kilometer'",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareConversion "kilometer" "meter" transform
      ]

declareConversionUnknownFrom
  = TestCase
  $ assertEqual "declare a dim with unknown to unit fails" expected actual
  where
    units = [("kilometer", Just "length")]
    startFrame = initFrame
      `withDimension` "length"
      `withUnit` (units !! 0)
    transform = LinearTransform 1000
    expected = (
        Left $ UnknownIdentifier "Conversion refers to unkown unit 'meter'",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareConversion "kilometer" "meter" transform
      ]

declareCyclicConversion
  = TestCase
  $ assertEqual "declare a dim from unit to itself fails" expected actual
  where
    units = [("kilometer", Just "length")]
    startFrame = initFrame
      `withDimension` "length"
      `withUnit` (units !! 0)
    transform = LinearTransform 1
    expected = (
        Left
          $ InvalidConversion "Cannot declare conversion from a unit to itself",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareConversion "kilometer" "kilometer" transform
      ]

declareConversionWithoutFromDim
  = TestCase
  $ assertEqual "declare a conversion without from dimensionality"
    expected actual
  where
    units = [("bar", Just "foo"), ("baz", Nothing)]
    startFrame = initFrame
      `withDimension` "foo"
      `withUnit` (units !! 0)
      `withUnit` (units !! 1)
    transform = LinearTransform 42
    expected
      = (Left $ InvalidConversion "Cannot convert dimensionless unit", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [DeclareConversion "bar" "baz" transform]

declareConversionWithoutToDim
  = TestCase
  $ assertEqual "declare a conversion without from dimensionality"
    expected actual
  where
    units = [("bar", Nothing), ("baz", Just "foo")]
    startFrame = initFrame
      `withDimension` "foo"
      `withUnit` (units !! 0)
      `withUnit` (units !! 1)
    transform = LinearTransform 42
    expected
      = (Left $ InvalidConversion "Cannot convert dimensionless unit", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [DeclareConversion "bar" "baz" transform]

declareConversionMismatchedDims
  = TestCase
  $ assertEqual "declare a conversion between different dimensions fails"
    expected actual
  where
    units = [("bar", Just "foo"), ("baz", Just "blah")]
    startFrame = initFrame
      `withDimension` "foo"
      `withUnit` (units !! 0)
      `withUnit` (units !! 1)
    transform = LinearTransform 42
    expectedMsg = "Cannot declare conversion between units of different "
            ++ "dimensions: from 'foo' to 'blah'"
    expected = (Left $ InvalidConversion expectedMsg, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [DeclareConversion "bar" "baz" transform]

declareAssignment
  = TestCase $ assertEqual "declare an assignment" expected actual
  where
    scalar = Scalar 500 $ u "foo" `divide` u "bar"
    expectedFrame
      = initFrame
        `withUnit` ("foo", Nothing)
        `withUnit` ("bar", Nothing)
        `withAssignment` (scalarToAssignment "baz" scalar)
    expected = (Right $ expectedFrame, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Assignment "baz" (ScalarLiteral scalar) empty
      ]

redeclareAssignment
  = TestCase $ assertEqual "redeclare an assignment fails" expected actual
  where
    expr = ScalarLiteral $ Scalar 500 $ u "foo" `divide` u "bar"
    expected
      = (Left $ RedefinedIdentifier "Identifier 'baz' is already defined", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Assignment "baz" expr empty,
        Assignment "baz" expr empty
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
        Assignment "baz" (Reference "doesNotExist") empty
      ]

assignmentWithUnknownUnits
  = TestCase
  $ assertEqual "an assignment with unknown units fails" expected actual
  where
    expr = ScalarLiteral $ Scalar 500 $ u "foo" `divide` u "doesNotExist"
    expected
      = (Left $ UnknownIdentifier "Expression refers to unknown units", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        Assignment "bar" expr empty
      ]

assignmentWithEvalFailure
  = TestCase
  $ assertEqual "an assignment that fails to eval" expected actual
  where
    expr = BinaryOperatorApply Divide
      (ScalarLiteral $ Scalar 42 $ u "foo")
      (ScalarLiteral $ Scalar 0 $ u "bar")
    expected = (Left $ ExecEvalFail DivideByZero, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Assignment "baz" expr empty
      ]

assignmentViolatesScalarConstraint
  = TestCase
  $ assertEqual "assignment fails to be scalar" expected actual
  where
    expr = BinaryOperatorApply Add
      (ScalarLiteral $ Scalar 12 $ u "foo")
      (ScalarLiteral $ Scalar 12 $ u "bar")
    expected = (Left $ UnsatisfiedConstraint msg, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Assignment "baz" expr (fromList [ConstrainedScalar])
      ]
    msg = "Constrained to scalar, but vector had multiple components:\n" ++
          "  actual: { bar, foo }"

assignmentViolatesDimensionConstraint
  = TestCase
  $ assertEqual "assignment fails to be scalar" expected actual
  where
    expr = ScalarLiteral $ Scalar 12 $ u "foo"
    expected = (Left $ UnsatisfiedConstraint msg, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Assignment "baz" expr (fromList [ConstrainedDimensionality target])
      ]
    target :: VectorDimensionality
    target = VecDims $ fromList [toMap $ Dimension "bar"]
    msg = concat [
        "Vector does not match target dims:\n",
        "  target: { bar }\n",
        "  actual: { foo }"
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
        Print (ScalarLiteral (Scalar 500 (u "mile"))) Nothing empty
      ]

printScalarTargetUnits
  = TestCase
  $ assertEqual "print simple scalar with target units" expected actual
  where
    expected = (
        Right $ initFrame
          `withDimension` "length"
          `withUnit` ("meter", Just "length")
          `withUnit` ("kilometer", Just "length")
          `withConversion` ("kilometer", "meter", LinearTransform 1000),
        ["7.0 kilometer = {7000.0 meter}"]
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareDimension "length",
        DeclareUnit "meter" $ Just "length",
        DeclareUnit "kilometer" $ Just "length",
        DeclareConversion "kilometer" "meter" $ LinearTransform 1000,
        Print
          (ScalarLiteral (Scalar 7 (u "kilometer"))) (Just $ u "meter") empty
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
            (ScalarLiteral (Scalar 1 (u "mile")))
            (ScalarLiteral (Scalar 0 (u "hour")))
          )
          Nothing
          empty
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
        Print (ScalarLiteral (Scalar 500 (u "mile"))) (Just $ u "hour") empty
      ]

printWithFractionOption
  = TestCase
  $ assertEqual "print as fraction" expected actual
  where
    expr = BinaryOperatorApply Add
      (ScalarLiteral $ Scalar 3 $ u "foo")
      (BinaryOperatorApply Divide
        (ScalarLiteral $ Scalar 5 $ u "bar")
        (ScalarLiteral $ Scalar 2 $ u "baz"))
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
        Print expr Nothing (fromList [MultiLineFractions])
      ]

printWithFractionOptionButNoNegativeDegree
  = TestCase
  $ assertEqual "print as fraction w/o negative degrees" expected actual
  where
    expr = BinaryOperatorApply Add
      (ScalarLiteral $ Scalar 12 $ u "foo")
      (ScalarLiteral $ Scalar 12 $ u "bar")
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
        Print expr Nothing (fromList [MultiLineFractions])
      ]

inputSimple = TestCase $ assertEqual "simple input" expected actual
  where
    expected = (
        Right $ initFrame
          `withDimension` "length"
          `withUnit` ("mile", Just "length")
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
        DeclareUnit "mile" $ Just "length",
        Input "foo" (toMap $ Dimension "length"),
        Print (Reference "foo") Nothing empty
      ]

inputParseRetry
  = TestCase $ assertEqual "simple input with bad parse" expected actual
  where
    expected = (
        Right $ initFrame
          `withDimension` "length"
          `withUnit` ("mile", Just "length")
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
        DeclareUnit "mile" $ Just "length",
        Input "foo" (toMap $ Dimension "length"),
        Print (Reference "foo") Nothing empty
      ]

inputUnknownUnitsRetry
  = TestCase $ assertEqual "simple input with unknown units" expected actual
  where
    expected = (
        Right $ initFrame
          `withDimension` "length"
          `withUnit` ("mile", Just "length")
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
        DeclareUnit "mile" $ Just "length",
        Input "foo" (toMap $ Dimension "length"),
        Print (Reference "foo") Nothing empty
      ]

inputMismatchedDimsRetry
  = TestCase $ assertEqual "simple input with mismatched dims" expected actual
  where
    expected = (
        Right $ initFrame
          `withDimension` "length"
          `withUnit` ("mile", Just "length")
          `withDimension` "mass"
          `withUnit` ("pound", Just "mass")
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
        DeclareUnit "mile" $ Just "length",
        DeclareDimension "mass",
        DeclareUnit "pound" $ Just "mass",
        Input "foo" (toMap $ Dimension "length"),
        Print (Reference "foo") Nothing empty
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
    assignmentWithEvalFailure,
    assignmentViolatesScalarConstraint,
    assignmentViolatesDimensionConstraint,

    -- print
    printSimpleScalar,
    printScalarTargetUnits,
    printEvalFail,
    printConstraintFail,
    printWithFractionOption,
    printWithFractionOptionButNoNegativeDegree,

    -- input
    inputSimple,
    inputParseRetry,
    inputUnknownUnitsRetry,
    inputMismatchedDimsRetry
  ]

main = do
  count <- runTestTT (TestList unitTests)
  if failures count > 0 then exitFailure else return ()
