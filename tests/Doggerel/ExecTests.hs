module Main where

import Control.Monad (when)
import Control.Monad.State.Lazy (runState)
import Data.Either (isLeft)
import Data.Map.Strict as Map
import Data.Set as Set (Set, empty, fromList, singleton)
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
u = toMap . mkBaseUnit

unitDeclDims :: Identifier -> Set UnitOption
unitDeclDims
  = Set.singleton
  . UnitDimensionality
  . DSTerm
  . (\i -> DSTermDim i Nothing 1)

falseScalar, trueScalar :: Scalar
falseScalar = Scalar 0 $ toMap $ BaseUnit "bool" Nothing
trueScalar = Scalar 1 $ toMap $ BaseUnit "bool" Nothing

idToUnitOpts :: Identifier -> Set UnitOptions
idToUnitOpts = Set.singleton . UnitDim . toMap . mkDimension

withPlainDimension :: ScopeFrame -> String -> ScopeFrame
withPlainDimension f d = f `withDimension` (d, Set.empty)

runTestIOWithInputs :: [String] -> TestIO a -> (a, [String])
runTestIOWithInputs is t =
  let
    (a, (stdout, _, _, _)) = runState t ([], is, [], [])
  in
    (a, stdout)

runTestIO :: TestIO a -> (a, [String])
runTestIO = runTestIOWithInputs []

ioResultToGoodOutput ::
     (Either ExecFail ScopeFrame, [String])
  -> (Maybe ExecFail, [String])
ioResultToGoodOutput (e, o) = case e of
  Left err -> (Just err, o)
  _ -> (Nothing, o)

declareDim = TestCase $ assertEqual "declare a dim" expected actual
  where
    expected = (Right $ initFrame `withPlainDimension` "myDim", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [DeclareDimension "myDim" Nothing]

redefineDim = TestCase $ assertEqual "re-declare a dim fails" expected actual
  where
    expected
      = (Left $ RedefinedIdentifier "Identifier 'foo' is already defined.", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result
      = executeWith
        (initFrame `withPlainDimension` "foo")
        [DeclareDimension "foo" Nothing]

staticRedefineDim
  = TestCase $ assertEqual "statically redeclared dim fails" expected actual
  where
    expected
      = (Left $ RedefinedIdentifier "Identifier 'foo' is already defined.", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result
      = executeWith initFrame [
          Block [DeclareUnit "foo" Set.empty],
          DeclareDimension "foo" Nothing
        ]

declareDimensionAlias
  = TestCase $ assertEqual "declare a dimension alias" expected actual
  where
    ds = DSTerm $ DSTermDim "foo" Nothing 1
    expected = (
        Right $ initFrame
          `withPlainDimension` "foo"
          `withDimensionAlias` ("bar", ds),
        []
      )
    startFrame = initFrame `withPlainDimension` "foo"
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [DeclareDimension "bar" $ Just ds]

declareExpandedDimensionAlias
  = TestCase
  $ assertEqual "declare a dimension alias expanding another" expected actual
  where
    expected = (
        Right $ startFrame
          `withDimensionAlias` ("baz", DSTerm $ DSTermDim "bar" Nothing 2),
        []
      )
    startFrame = initFrame
      `withPlainDimension` "foo"
      `withDimensionAlias` ("bar", DSTerm $ DSTermDim "foo" Nothing 2)
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareDimension "baz" $ Just $ DSTerm $ DSTermDim "bar" Nothing 2
      ]

declareUnitInDim
  = TestCase $ assertEqual "declare a unit in a dimension" expected actual
  where
    expected = (
        Right $ initFrame
          `withPlainDimension` "length"
          `withUnit` ("mile", idToUnitOpts "length"),
        []
      )
    startFrame = initFrame `withPlainDimension` "length"
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [DeclareUnit "mile" $ unitDeclDims "length"]

declareDimensionlessUnit
  = TestCase $ assertEqual "declare a dimensionless unit" expected actual
  where
    expected = (Right $ initFrame `withUnit` ("potato", Set.empty), [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith initFrame [DeclareUnit "potato" Set.empty]

declareNaturalUnit
  = TestCase $ assertEqual "declare a natural unit" expected actual
  where
    expected = (
        Right $ initFrame `withUnit` ("potato", Set.singleton NaturalUnit),
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith initFrame [
        DeclareUnit "potato" $ Set.singleton NaturalUnitDecl
      ]

redefineUnit
  = TestCase $ assertEqual "redeclare unit fails" expected actual
  where
    expected
      = (Left $ RedefinedIdentifier "Identifier 'foo' is already defined.", [])
    startFrame = initFrame `withUnit` ("foo", Set.empty)
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [DeclareUnit "foo" Set.empty]

staticRedefineUnit
  = TestCase $ assertEqual "statically redeclared unit fails" expected actual
  where
    expected
      = (Left $ RedefinedIdentifier "Identifier 'foo' is already defined.", [])
    startFrame = initFrame `withUnit` ("foo", Set.empty)
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        Block [DeclareDimension "foo" Nothing],
        DeclareUnit "foo" Set.empty
      ]

unknownUnitDim
  = TestCase $ assertEqual "declare unit with unknown dim fails" expected actual
  where
    expected = (
        Left $ UnknownIdentifier "Dimspec refers to unknown identifier: bar",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith initFrame [DeclareUnit "foo" $ unitDeclDims "bar"]

unitInDimspec
  = TestCase
  $ assertEqual "declare a unit with a dimspec as dimension" expected actual
  where
    expected = (
        Right $ startFrame `withUnit` ("baz", idToUnitOpts "foo"),
        []
      )
    startFrame = initFrame
      `withPlainDimension` "foo"
      `withDimensionAlias` ("bar", DSTerm $ DSTermDim "foo" Nothing 1)
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareUnit "baz" $ Set.singleton $ UnitDimensionality $ DSTerm $ DSTermDim "bar" Nothing 1
      ]

unitInTransitiveDimspec = TestCase $ assertEqual
  "declare a unit with a dimspec of a dimspec as dimension"
  expected
  actual
  where
    expected = (
        Right $ startFrame `withUnit` ("u", expectedDimensionality),
        []
      )
    expectedDimensionality = Set.singleton $ UnitDim $ fromMap $ Map.fromList [
        (mkDimension "d1", 8),
        (mkDimension "d2", 4)
      ]
    ds1 = DSProduct [
        DSTerm $ DSTermDim "d1" Nothing 2,
        DSTerm $ DSTermDim "d2" Nothing 1
      ]
    startFrame = initFrame
      `withPlainDimension` "d1"
      `withPlainDimension` "d2"
      `withDimensionAlias` ("ds1", ds1)
      `withDimensionAlias` ("ds2", DSTerm $ DSTermDim "ds1" Nothing 2)
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareUnit "u" $ Set.singleton $ UnitDimensionality $ DSTerm $ DSTermDim "ds2" Nothing 2
      ]

declareConversion
  = TestCase $ assertEqual "declare a conversion" expected actual
  where
    units = [
        ("meter", idToUnitOpts "length"),
        ("kilometer", idToUnitOpts "length")
      ]
    startFrame = initFrame
      `withPlainDimension` "length"
      `withUnit` head units
      `withUnit` (units !! 1)
    transform = LinearTransform 1000
    expectedFrame = initFrame
      `withPlainDimension` "length"
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
    units = [("meter", idToUnitOpts "length")]
    startFrame = initFrame
      `withPlainDimension` "length"
      `withUnit` head units
    transform = LinearTransform 1000
    expected = (
        Left $ UnknownIdentifier "Unknown units: kilometer",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareConversion (u "kilometer") (u "meter") transform
      ]

declareConversionUnknownFrom
  = TestCase
  $ assertEqual
    "declare a conversion with unknown from unit fails"
    expected
    actual
  where
    units = [("kilometer", idToUnitOpts "length")]
    startFrame = initFrame
      `withPlainDimension` "length"
      `withUnit` head units
    transform = LinearTransform 1000
    expected = (
        Left $ UnknownIdentifier "Unknown units: meter",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareConversion (u "kilometer") (u "meter") transform
      ]

declareConversionMissingFromIndex
  = TestCase
  $ assertEqual
    "declare a conversion with missing natural unit index"
    expected
    actual
  where
    units = [
      ("meter", idToUnitOpts "length"),
      ("position", Set.fromList [
        UnitDim $ toMap $ mkDimension "length",
        NaturalUnit
      ])]
    startFrame = initFrame
      `withPlainDimension` "length"
      `withUnit` head units
      `withUnit` (units !! 1)
    transform = LinearTransform 1000
    expected = (
        Left $ InvalidUnitSpec
           "Invalid unit: position is natural, but is missing an index",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareConversion (u "position") (u "meter") transform
      ]

declareConversionInvalidFromIndex
  = TestCase
  $ assertEqual
    "declare a conversion with invalid from unit index"
    expected
    actual
  where
    units = [
        ("meter", idToUnitOpts "length"),
        ("kilometer", idToUnitOpts "length")
      ]
    startFrame = initFrame
      `withPlainDimension` "length"
      `withUnit` head units
      `withUnit` (units !! 1)
    transform = LinearTransform 1000
    expectedFrame = initFrame
      `withPlainDimension` "length"
      `withUnit` head units
      `withUnit` (units !! 1)
      `withConversion` (u "kilometer", u "meter", transform)
    expected = (Right expectedFrame, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = executeWith startFrame [
        DeclareConversion (u "kilometer") (toMap $ BaseUnit "meter" $ Just 3) transform
      ]

declareCyclicConversion
  = TestCase
  $ assertEqual "declare a dim from unit to itself fails" expected actual
  where
    units = [("kilometer", idToUnitOpts "length")]
    startFrame = initFrame
      `withPlainDimension` "length"
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
    units = [("bar", idToUnitOpts "foo"), ("baz", Set.empty)]
    startFrame = initFrame
      `withPlainDimension` "foo"
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
    units = [("bar", Set.empty), ("baz", idToUnitOpts "foo")]
    startFrame = initFrame
      `withPlainDimension` "foo"
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
    units = [("bar", idToUnitOpts "foo"), ("baz", idToUnitOpts "blah")]
    startFrame = initFrame
      `withPlainDimension` "foo"
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
    expectedFrame =
      let
        f = initFrame
          `withUnit` ("foo", Set.empty)
          `withUnit` ("bar", Set.empty)
      in
        f `withAssignment` scalarToAssignment f "baz" scalar
    expected = (Right expectedFrame, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Set.empty,
        DeclareUnit "bar" Set.empty,
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
        DeclareUnit "foo" Set.empty,
        DeclareUnit "bar" Set.empty,
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
        DeclareUnit "foo" Set.empty,
        DeclareUnit "bar" Set.empty,
        Assignment "baz" (Reference "doesNotExist") Set.empty
      ]

assignmentWithUnknownUnits
  = TestCase
  $ assertEqual "an assignment with unknown units fails" expected actual
  where
    expr = Literal $ Scalar 500 $ u "foo" `divide` u "doesNotExist"
    expected
      = (Left $ UnknownIdentifier "Unknown units: doesNotExist", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Set.empty,
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
        DeclareUnit "foo" Set.empty,
        DeclareUnit "bar" Set.empty,
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
        DeclareUnit "foo" Set.empty,
        DeclareUnit "bar" Set.empty,
        Assignment "baz" expr (Set.fromList [ConstrainedDimensionality target])
      ]
    target :: Dimspec
    target = DSTerm $ DSTermDim "bar" Nothing 1
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
        DeclareUnit "foo" Set.empty,
        Assignment "baz" expr Set.empty
      ]
    expr = UnaryOperatorApply (Exponent 2) $ Literal $ Scalar 2 $ u "foo"
    msg = "Exponent operator not allowed in vector-valued expressions"

assignmentWellFormedLogic = TestCase
  $ assertEqual "assign well formed logical operator" expected actual
  where
    expected = (
        Right $ initFrame `withAssignment` ("foo", logicalFalse, DSTerm $ DSTermDim "bool" Nothing 1),
        []
      )
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
        DeclareUnit "foo" Set.empty,
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
          ++  "but was applied to: { foo }",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Set.empty,
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
          $   "The inequality less-than operator must be applied to vectors "
          ++  "of the same dimensionality, but was applied to: "
          ++  "{ bool } and { foo }",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Set.empty,
        Assignment "bar" expr Set.empty
      ]

simpleUpdate = TestCase $ assertEqual "simple update" expected actual
  where
    expected = (
        Nothing,
        ["foo = {1.0 bar}", "foo = {2.0 bar}"]
      )
    actual = ioResultToGoodOutput $ runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "bar" Set.empty,
        Assignment "foo" (Literal $ Scalar 1 $ u "bar") Set.empty,
        Print (Reference "foo") Set.empty,
        Update "foo" $ Literal $ Scalar 2 $ u "bar",
        Print (Reference "foo") Set.empty
      ]

lexicalUpdate = TestCase
  $ assertEqual "update across lexical scope" expected actual
  where
    expected = (
        Nothing,
        ["foo = {10.0 bar}", "foo = {1.0 bar}"]
      )
    actual = ioResultToGoodOutput $ runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "bar" Set.empty,
        Assignment "foo" (Literal $ Scalar 10 $ u "bar") Set.empty,
        Block [
          Print (Reference "foo") Set.empty,
          Update "foo" $ BinaryOperatorApply Subtract
            (Reference "foo")
            (Literal $ Scalar 9 $ u "bar")
        ],
        Print (Reference "foo") Set.empty
      ]

lexicallyScopedUpdate = TestCase
  $ assertEqual "update applies only to lexical scope" expected actual
  where
    expected = (
        Nothing,
        ["foo = {100.0 bar}", "foo = {1.0 baz}", "foo = {100.0 bar}"]
      )
    actual = ioResultToGoodOutput $ runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "bar" Set.empty,
        Assignment "foo" (Literal $ Scalar 100 $ u "bar") Set.empty,
        Block [
          Print (Reference "foo") Set.empty,
          DeclareUnit "baz" Set.empty,
          Assignment "foo" (Literal $ Scalar 1 $ u "baz") Set.empty,
          Print (Reference "foo") Set.empty
        ],
        Print (Reference "foo") Set.empty
      ]

lexicallyMaskedUpdateFails = TestCase
  $ assertEqual "update fails by lexical masking" expected actual
  where
    expected = (
        Just $ UnknownIdentifier "Updating an unknown identifier.",
        ["foo = {42.0 bar}"]
      )
    actual = ioResultToGoodOutput $ runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "bar" Set.empty,
        Assignment "foo" (Literal $ Scalar 42 $ u "bar") Set.empty,
        Block [
          -- This is allowed because it's a valid reference in the parent.
          Print (Reference "foo") Set.empty,
          -- This is allowed because foo is not defined locally.
          DeclareUnit "foo" Set.empty,
          -- This fails because foo is now a unit.
          Update "foo" (Literal $ Scalar 2 $ u "bar")
        ]
      ]

updateDimsMismatchFails = TestCase
  $ assertEqual "update fails with dims mismatch" expected actual
  where
    expected = (
        Just $ UnsatisfiedConstraint "mismatched dimensions in update",
        []
      )
    actual = ioResultToGoodOutput $ runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Set.empty,
        DeclareUnit "bar" Set.empty,
        Assignment "baz" (Literal $ Scalar 42 $ u "foo") Set.empty,
        Update "baz" (Literal $ Scalar 1337 $ u "bar")
      ]

printSimpleScalar = TestCase $ assertEqual "print simple scalar" expected actual
  where
    expected = (
        Right $ initFrame `withUnit` ("mile", Set.empty),
        ["500.0 mile = {500.0 mile}"]
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "mile" Set.empty,
        Print (Literal (Scalar 500 (u "mile"))) Set.empty
      ]

printScalarTargetUnits
  = TestCase
  $ assertEqual "print simple scalar with target units" expected actual
  where
    expected = (
        Right $ initFrame
          `withPlainDimension` "length"
          `withUnit` ("meter", idToUnitOpts "length")
          `withUnit` ("kilometer", idToUnitOpts "length")
          `withConversion` (u "kilometer", u "meter", LinearTransform 1000),
        ["7.0 kilometer = {7000.0 meter}"]
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareDimension "length" Nothing,
        DeclareUnit "meter" $ unitDeclDims "length",
        DeclareUnit "kilometer" $ unitDeclDims "length",
        DeclareConversion (u "kilometer") (u "meter") (LinearTransform 1000),
        Print
          (Literal (Scalar 7 (u "kilometer")))
          (Set.singleton $ OutputUnits $ u "meter")
      ]

printEvalFail = TestCase $ assertEqual "print failed eval" expected actual
  where
    expected = (Left $ ExecEvalFail DivideByZero, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "mile" Set.empty,
        DeclareUnit "hour" Set.empty,
        Print (
          BinaryOperatorApply
            Divide
            (Literal (Scalar 1 (u "mile")))
            (Literal (Scalar 0 (u "hour")))
          )
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
        DeclareUnit "foo" Set.empty,
        DeclareUnit "bar" Set.empty,
        Print expr Set.empty
      ]

printConstraintFail
  = TestCase $ assertEqual "print unsatisfieable target units" expected actual
  where
    expected
      = (Left $ UnsatisfiableConstraint "could not convert to units", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "mile" Set.empty,
        DeclareUnit "hour" Set.empty,
        Print
          (Literal (Scalar 500 (u "mile")))
          (Set.singleton $ OutputUnits $ u "hour")
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
          `withUnit` ("foo", Set.empty)
          `withUnit` ("bar", Set.empty)
          `withUnit` ("baz", Set.empty),
        [
          "                                ⎧ 2.5 bar           ⎫",
          "3.0 foo + (5.0 bar ÷ 2.0 baz) = ⎨ ─────── , 3.0 foo ⎬",
          "                                ⎩     baz           ⎭"
        ]
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Set.empty,
        DeclareUnit "bar" Set.empty,
        DeclareUnit "baz" Set.empty,
        Print expr (Set.fromList [MultiLineFractions])
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
          `withUnit` ("foo", Set.empty)
          `withUnit` ("bar", Set.empty),
        ["12.0 foo + 12.0 bar = {12.0 bar, 12.0 foo}"]
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Set.empty,
        DeclareUnit "bar" Set.empty,
        Print expr (Set.fromList [MultiLineFractions])
      ]

printContainsExponent
  = TestCase
  $ assertEqual "print with exponent fails" expected actual
  where
    expected = (Left $ InvalidVectorExpression msg, [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Set.empty,
        Print expr Set.empty
      ]
    expr = UnaryOperatorApply (Exponent 2) $ Literal $ Scalar 2 $ u "foo"
    msg = "Exponent operator not allowed in vector-valued expressions"

inputSimple = TestCase $ assertEqual "simple input" expected actual
  where
    expected = (
        Right $ initFrame
          `withPlainDimension` "length"
          `withUnit` ("mile", idToUnitOpts "length")
          `withInput` ("foo", Right $ Scalar 123.4 $ u "mile"),
        [
            "Enter a scalar of dimensionality {length}",
            "foo = {123.4 mile}"
          ]
      )
    actual = runTestIOWithInputs ["123.4 mile"] result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareDimension "length" Nothing,
        DeclareUnit "mile" $ unitDeclDims "length",
        Input "foo" (toMap $ mkDimension "length"),
        Print (Reference "foo") Set.empty
      ]

inputParseRetry
  = TestCase $ assertEqual "simple input with bad parse" expected actual
  where
    expected = (
        Right $ initFrame
          `withPlainDimension` "length"
          `withUnit` ("mile", idToUnitOpts "length")
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
        DeclareDimension "length" Nothing,
        DeclareUnit "mile" $ unitDeclDims "length",
        Input "foo" (toMap $ mkDimension "length"),
        Print (Reference "foo") Set.empty
      ]

inputUnknownUnitsRetry
  = TestCase $ assertEqual "simple input with unknown units" expected actual
  where
    expected = (
        Right $ initFrame
          `withPlainDimension` "length"
          `withUnit` ("mile", idToUnitOpts "length")
          `withInput` ("foo", Right $ Scalar 123.4 $ u "mile"
            ),
        [
            "Enter a scalar of dimensionality {length}",
            "[Unknown ID] Unknown units: parsec",
            "Try again...",
            "Enter a scalar of dimensionality {length}",
            "foo = {123.4 mile}"
          ]
      )
    actual = runTestIOWithInputs ["12 parsec", "123.4 mile"] result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareDimension "length" Nothing,
        DeclareUnit "mile" $ unitDeclDims "length",
        Input "foo" (toMap $ mkDimension "length"),
        Print (Reference "foo") Set.empty
      ]

inputMismatchedDimsRetry
  = TestCase $ assertEqual "simple input with mismatched dims" expected actual
  where
    expected = (
        Right $ initFrame
          `withPlainDimension` "length"
          `withUnit` ("mile", idToUnitOpts "length")
          `withPlainDimension` "mass"
          `withUnit` ("pound", idToUnitOpts "mass")
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
        DeclareDimension "length" Nothing,
        DeclareUnit "mile" $ unitDeclDims "length",
        DeclareDimension "mass" Nothing,
        DeclareUnit "pound" $ unitDeclDims "mass",
        Input "foo" (toMap $ mkDimension "length"),
        Print (Reference "foo") Set.empty
      ]

simpleRelation = TestCase $ assertEqual "simple relation" expected actual
  where
    expected = (
        Right $ initFrame
          `withUnit` ("foo", Set.empty)
          `withUnit` ("bar", Set.empty)
          `withUnit` ("baz", Set.empty)
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
        DeclareUnit "foo" Set.empty,
        DeclareUnit "bar" Set.empty,
        DeclareUnit "baz" Set.empty,
        Relation
          "f"
          [("a", u "foo"), ("b", u "bar"), ("c", u "baz")]
          (Reference "a")
          (BinaryOperatorApply
            Multiply
            (Reference "b")
            (Reference "c")),
        Print
          (FunctionApply "f"
            (BinaryOperatorApply Add
              (Literal $ Scalar 12 $ u "foo")
              (Literal $ Scalar 4 $ u "bar")))
          Set.empty
      ]

exponentRelation = TestCase $ assertEqual "exponent relation" expected actual
  where
    expected = (
        Right $ initFrame
          `withUnit` ("foo", Set.empty)
          `withUnit` ("bar", Set.empty)
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
        DeclareUnit "foo" Set.empty,
        DeclareUnit "bar" Set.empty,
        Relation
          "g"
          [("a", u "foo"), ("b", u "bar")]
          (Reference "a")
          (UnaryOperatorApply
            (Exponent 4)
            (Reference "b")),
        Print
          (FunctionApply "g"
            (Literal $ Scalar 2 $ u "bar"))
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
        DeclareUnit "foo" Set.empty,
        DeclareUnit "bar" Set.empty,
        Relation
          "foo"
          [("a", u "foo"), ("b", u "bar")]
          (Reference "a")
          (Reference "b")
      ]

relationUnknownUnits =
  TestCase $ assertEqual "relation with unknown unit" expected actual
  where
    expected =
      (Left $ UnknownIdentifier "Unknown units: foo", [])
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        Relation
          "baz"
          [("a", u "foo"), ("b", u "bar")]
          (Reference "a")
          (Reference "b")
      ]

relationReusedParam =
  TestCase $ assertEqual "relation with repeated param" expected actual
  where
    expected = (
        Left $ RedefinedIdentifier
          "Identifier appears on both sides of a relation: a",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareDimension "d" Nothing,
        DeclareUnit "foo" $ unitDeclDims "d",
        Relation
          "bar"
          [("a", u "foo")]
          (Reference "a")
          (Reference "a")
      ]

blockTest
  = TestCase $ assertEqual "block with hidden assignment" expected actual
  where
    expected = (
        Left (UnknownIdentifier "Expression refers to unknown identifier"),
        ["bar = {2.0 foo}"]
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        Block [
          DeclareUnit "foo" Set.empty,
          Assignment "bar" (Literal $ Scalar 2 $ u "foo") Set.empty,
          -- This print succeeds.
          Print (Reference "bar") Set.empty
        ],
        -- This print fails.
        Print (Reference "bar") Set.empty
      ]

conditionalTest = TestCase $ assertEqual "basic conditional" expected actual
  where
    expected = ["bar = {10.0 foo}"]
    actual = snd $ runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Set.empty,
        Assignment "bar" (Literal $ Scalar 10 $ u "foo") Set.empty,
        Conditional
          (BinaryOperatorApply GreaterThan
            (Reference "bar")
            (Literal $ Scalar 1 $ u "foo"))
          [Print (Reference "bar") Set.empty]
          Nothing
      ]

conditionalNegativeTest
  = TestCase $ assertEqual "conditional taking negative branch" expected actual
  where
    expected = ["baz = {1337.0 foo}"]
    actual = snd $ runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Set.empty,
        Assignment "bar" (Literal $ Scalar 0 $ u "foo") Set.empty,
        Conditional
          (BinaryOperatorApply GreaterThan
            (Reference "bar")
            (Literal $ Scalar 1 $ u "foo"))
          [Print (Reference "bar") Set.empty]
          (Just [
              Assignment "baz" (Literal $ Scalar 1337 $ u "foo") Set.empty,
              Print (Reference "baz") Set.empty
            ])
      ]

conditionalOnNonBooleanTest
  = TestCase $ assertEqual "conditional on non boolean" expected actual
  where
    expected = (
        Left $ UnsatisfiedConstraint
          "A conditional expression must be of boolean dimension",
        []
      )
    actual = runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "foo" Set.empty,
        Assignment "bar" (Literal $ Scalar 0 $ u "foo") Set.empty,
        Conditional (Reference "bar") [] Nothing
      ]

conditionalOnInputTest
  = TestCase $ assertEqual "conditional on input" expected actual
  where
    expected = [
        "Enter a scalar of dimensionality {dim}",
        "myInput = {2.0 foo}"
      ]
    actual = snd $ runTestIOWithInputs ["2 foo"] result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareDimension "dim" Nothing,
        DeclareUnit "foo" $ unitDeclDims "dim",
        Assignment "bar" (Literal $ Scalar 1 $ u "foo") Set.empty,
        Input "myInput" (toMap $ mkDimension "dim"),
        Conditional
          (BinaryOperatorApply GreaterThan
            (Reference "bar")
            (Reference "myInput"))
          [Print (Reference "bar") Set.empty]
          (Just [Print (Reference "myInput") Set.empty])
      ]

whileLoopTest
  = TestCase $ assertEqual "while loop" expected actual
  where
    expected = (
        Nothing,
        [
          "i = {4.0 iter}",
          "i = {3.0 iter}",
          "i = {2.0 iter}",
          "i = {1.0 iter}"
        ]
      )
    actual = ioResultToGoodOutput $ runTestIO result
    result :: TestIO (Either ExecFail ScopeFrame)
    result = execute [
      DeclareUnit "iter" Set.empty,
      Assignment "i" (Literal $ Scalar 4 $ u "iter") Set.empty,
      WhileLoop
        (BinaryOperatorApply
          GreaterThan
          (Reference "i")
          (Literal $ Scalar 0 $ u "iter"))
        [
            Print (Reference "i") Set.empty,
            Update "i"
              (BinaryOperatorApply
                Subtract
                (Reference "i")
                (Literal $ Scalar 1 $ u "iter"))
          ]
      ]

unitTests = [
    -- dim
    declareDim,
    redefineDim,
    staticRedefineDim,
    declareDimensionAlias,
    declareExpandedDimensionAlias,

    -- unit
    declareUnitInDim,
    declareDimensionlessUnit,
    declareNaturalUnit,
    redefineUnit,
    staticRedefineUnit,
    unknownUnitDim,
    unitInDimspec,
    unitInTransitiveDimspec,

    -- convert
    declareConversion,
    declareConversionUnknownFrom,
    declareConversionMissingFromIndex,
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

    -- update
    simpleUpdate,
    lexicalUpdate,
    lexicallyScopedUpdate,
    lexicallyMaskedUpdateFails,
    updateDimsMismatchFails,

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
    relationReusedParam,

    -- block
    blockTest,

    -- conditionals
    conditionalTest,
    conditionalNegativeTest,
    conditionalOnNonBooleanTest,
    conditionalOnInputTest,

    -- loops
    whileLoopTest
  ]

main = do
  count <- runTestTT (TestList unitTests)
  when (failures count > 0) exitFailure
