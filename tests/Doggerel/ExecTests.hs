module Main where

import Control.Monad.Writer (runWriter)
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

declareDim = TestCase $ assertEqual "declare a dim" expected actual
  where
    expected = (Right $ initFrame `withDimension` "myDim", [])
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
    result = execute [DeclareDimension "myDim"]

redefineDim = TestCase $ assertEqual "re-declare a dim fails" expected actual
  where
    expected
      = (Left $ RedefinedIdentifier "Identifier 'foo' is already defined.", [])
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
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
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [DeclareUnit "mile" $ Just "length"]

declareDimensionlessUnit
  = TestCase $ assertEqual "declare a dimensionless unit" expected actual
  where
    expected = (Right $ initFrame `withUnit` ("potato", Nothing), [])
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
    result = executeWith initFrame [DeclareUnit "potato" Nothing]

refefineUnit
  = TestCase $ assertEqual "redeclare unit fails" expected actual
  where
    expected
      = (Left $ RedefinedIdentifier "Identifier 'foo' is already defined.", [])
    startFrame = initFrame `withUnit` ("foo", Nothing)
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [DeclareUnit "foo" Nothing]

unknownUnitDim
  = TestCase $ assertEqual "declare unit with unknown dim fails" expected actual
  where
    expected
      = (Left $ UnknownIdentifier "Reference to undeclared dimension 'bar'", [])
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
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
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
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
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
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
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
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
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
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
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
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
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
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
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [DeclareConversion "bar" "baz" transform]

declareAssignment
  = TestCase $ assertEqual "declare an assignment" expected actual
  where
    expr = ScalarLiteral $ Scalar 500 $ u "foo" `divide` u "bar"
    expectedFrame
      = initFrame
        `withUnit` ("foo", Nothing)
        `withUnit` ("bar", Nothing)
        `withAssignment` ("baz", expr)
    expected = (Right $ expectedFrame, [])
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Assignment "baz" expr
      ]

redeclareAssignment
  = TestCase $ assertEqual "redeclare an assignment fails" expected actual
  where
    expr = ScalarLiteral $ Scalar 500 $ u "foo" `divide` u "bar"
    expected
      = (Left $ RedefinedIdentifier "Identifier 'baz' is already defined", [])
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Assignment "baz" expr,
        Assignment "baz" expr
      ]

assignmentWithUnknownReference
  = TestCase
  $ assertEqual "an assignment with unknown reference fails" expected actual
  where
    expected
      = (Left $ UnknownIdentifier "Expression refers to unknown identifier", [])
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        DeclareUnit "bar" Nothing,
        Assignment "baz" $ Reference "doesNotExist"
      ]

assignmentWithUnknownUnits
  = TestCase
  $ assertEqual "an assignment with unknown units fails" expected actual
  where
    expr = ScalarLiteral $ Scalar 500 $ u "foo" `divide` u "doesNotExist"
    expected
      = (Left $ UnknownIdentifier "Expression refers to unknown units", [])
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Nothing,
        Assignment "bar" expr
      ]

printSimpleScalar = TestCase $ assertEqual "print simple scalar" expected actual
  where
    expected = (
        Right $ initFrame `withUnit` ("mile", Nothing),
        ["500.0 mile = {500.0 mile}"]
      )
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "mile" Nothing,
        Print (ScalarLiteral (Scalar 500 (u "mile"))) Nothing
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
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareDimension "length",
        DeclareUnit "meter" $ Just "length",
        DeclareUnit "kilometer" $ Just "length",
        DeclareConversion "kilometer" "meter" $ LinearTransform 1000,
        Print (ScalarLiteral (Scalar 7 (u "kilometer"))) (Just $ u "meter")
      ]

printEvalFail = TestCase $ assertEqual "print failed eval" expected actual
  where
    expected = (Left $ ExecEvalFail DivideByZero, [])
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
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
      ]

printConstraintFail
  = TestCase $ assertEqual "print unsatisfieable target units" expected actual
  where
    expected
      = (Left $ UnsatisfiableConstraint "could not convert to units", [])
    actual = runWriter result
    result :: WriterIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "mile" Nothing,
        DeclareUnit "hour" Nothing,
        Print (ScalarLiteral (Scalar 500 (u "mile"))) $ Just $ u "hour"
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

    -- print
    printSimpleScalar,
    printScalarTargetUnits,
    printEvalFail,
    printConstraintFail
  ]

main = do
  count <- runTestTT (TestList unitTests)
  if failures count > 0 then exitFailure else return ()
