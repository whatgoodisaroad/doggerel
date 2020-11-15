module Main where

import Control.Monad (when)
import Data.Set as Set (Set, empty, fromList, singleton)
import Doggerel.Ast
import Doggerel.Conversion
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.Parser (parseFile)
import System.Exit (exitFailure)
import Test.HUnit
import Text.ParserCombinators.Parsec

u :: String -> Units
u = toMap . mkBaseUnit

d :: String -> Dimensionality
d = toMap . Dimension

assertParsesTo :: String -> String -> Either ParseError Program -> Test
assertParsesTo msg input expected
  = TestCase $ assertEqual msg expected $ parseFile input

dimDeclPTest
  = assertParsesTo "parse simple dim declaration" "dim foo;"
  $ Right [DeclareDimension "foo"]

unitDeclPTestInDim
  = assertParsesTo "parses unit in dim" "unit foo of bar;"
  $ Right [
      DeclareUnit "foo"
        $ singleton
        $ UnitDimensionality
        $ toMap
        $ Dimension "bar"
    ]

unitDeclPTestInCompoundDim
  = assertParsesTo "parses unit in compound dim" "unit acre of length^2;"
  $ Right [
      DeclareUnit "acre"
        $ singleton
        $ UnitDimensionality
        $ toMap (Dimension "length") `multiply` toMap (Dimension "length")
    ]

unitDeclPTestNoDim
  = assertParsesTo "parses unit in no dim" "unit foo;"
  $ Right [DeclareUnit "foo" empty]

assignmentPNoOpts
  = assertParsesTo "parses simple assignment with no opts"
  "let foo = (4.5678 bar/baz) * x;"
  $ Right [
        Assignment "foo"
          (BinaryOperatorApply Multiply
            (Literal $ Scalar 4.5678 $ u "bar" `divide` u "baz")
            (Reference "x"))
          empty
      ]

assignmentPWithScalarConstraint
  = assertParsesTo "parses simple assignment with no opts"
  "let foo = (3.1415926 diameter/radius) with scalar: true;"
  $ Right [
        Assignment "foo"
          (Literal $ Scalar 3.1415926 $ u "diameter" `divide` u "radius")
          (fromList [ConstrainedScalar])
      ]

assignmentUpdate = assertParsesTo "parses an assignment update"
  "height = 5 foot + 11 inch;"
  $ Right [
        Update "height"
          (BinaryOperatorApply Add
            (Literal $ Scalar 5 $ u "foot")
            (Literal $ Scalar 11 $ u "inch"))
      ]

conversionDeclPTest
  = assertParsesTo "parse simple converstion" "convert foo = 4.2 * bar;"
  $ Right [DeclareConversion (u "foo") (u "bar") $ LinearTransform 4.2]

printPTestNoOpts
  = assertParsesTo "print with no options" "print 200 f + 10 g;"
  $ Right [
      Print
        (BinaryOperatorApply Add
          (Literal $ Scalar 200 $ u "f")
          (Literal $ Scalar 10 $ u "g"))
        empty
    ]

printPTestUnitsOpt
  = assertParsesTo "print with units option" "print (12 x) with units: y;"
  $ Right [
    Print (Literal $ Scalar 12 $ u "x") (Set.singleton $ OutputUnits $ u "y")]

printPTestStyleOpt
  = assertParsesTo "print with style option" "print z with style: fractions;"
  $ Right [Print (Reference "z") $ fromList [MultiLineFractions]]

printPTestStyleAndUnitsOpts
  = assertParsesTo "print with style and untis options"
  "print x with style: fractions, units: y;"
  $ Right [
    Print (Reference "x") $ fromList [OutputUnits $ u "y", MultiLineFractions]]

commentPTest
  = assertParsesTo "comment" "# la la la code comment\n" $ Right [Comment]

inputPTest
  = assertParsesTo "input declaration" "input foo of bar/baz^2;"
  $ Right [Input "foo" $ d "bar" `divide` (d "baz" `multiply` d "baz")]

relationPTest
  = assertParsesTo "relation declaration"
  "relate myRel with a * b = c - (d / 123.4);"
  $ Right [
    Relation
      "myRel"
      (BinaryOperatorApply Multiply (Reference $ u "a") (Reference $ u "b"))
      (BinaryOperatorApply Subtract
        (Reference $ u "c")
        (BinaryOperatorApply Divide
          (Reference $ u "d")
          (Literal 123.4)))]

blockPTest = assertParsesTo "block scope"
  (concat [
      "let {",
      "  dim foo;",
      "  unit bar of foo;",
      "  let baz = 123.4 bar;",
      "}"
    ])
  $ Right [ Block [
      DeclareDimension "foo",
      DeclareUnit "bar" $ singleton $ UnitDimensionality $ d "foo",
      Assignment "baz" (Literal $ Scalar 123.4 $ u "bar") empty
    ]]

ifThenTest = assertParsesTo "conditional with affirmative branch"
  (concat [
      "if (foo > bar) {",
      "  let baz = 123.4 mile;",
      "}"
    ])
  $ Right [
      Conditional
        (BinaryOperatorApply GreaterThan
          (Reference "foo")
          (Reference "bar"))
        [Assignment "baz" (Literal $ Scalar 123.4 $ u "mile") empty]
        Nothing
    ]

ifThenElseTest = assertParsesTo "conditional with both branches"
  (concat [
      "if (foo > bar) {",
      "  let baz = 123.4 mile;",
      "}",
      "else {",
      "  let baz = 42 kilometer;",
      "}"
    ])
  $ Right [
      Conditional
        (BinaryOperatorApply GreaterThan
          (Reference "foo")
          (Reference "bar"))
        [Assignment "baz" (Literal $ Scalar 123.4 $ u "mile") empty]
        (Just [Assignment "baz" (Literal $ Scalar 42 $ u "kilometer") empty])
    ]

whileLoopTest = assertParsesTo "while loop"
  (concat [
      "unit iter;",
      "let i = 1 iter;",
      "while (i <= 10 iter) {",
      "  print i;",
      "  i = i + 1 iter;",
      "}"
    ])
  $ Right [
      DeclareUnit "iter" empty,
      Assignment "i" (Literal $ Scalar 1 $ u "iter") empty,
      WhileLoop
        (BinaryOperatorApply
          LessThanOrEqualTo
          (Reference "i")
          (Literal $ Scalar 10 $ u "iter"))
        [
            Print (Reference "i") empty,
            Update "i"
              (BinaryOperatorApply
                Add
                (Reference "i")
                (Literal $ Scalar 1 $ u "iter"))
          ]
    ]

unitTests = [
    -- dim
    dimDeclPTest,

    -- unit
    unitDeclPTestInDim,
    unitDeclPTestInCompoundDim,
    unitDeclPTestNoDim,

    -- let
    assignmentPNoOpts,
    assignmentPWithScalarConstraint,

    -- update
    assignmentUpdate,

    -- convert
    conversionDeclPTest,

    -- print
    printPTestNoOpts,
    printPTestUnitsOpt,
    printPTestStyleOpt,
    printPTestStyleAndUnitsOpts,

    -- #
    commentPTest,

    -- input
    inputPTest,

    -- relation
    relationPTest,

    -- block
    blockPTest,

    -- conditionals
    ifThenTest,
    ifThenElseTest,

    -- while
    whileLoopTest
  ]

main = do
  count <- runTestTT (TestList unitTests)
  when (failures count > 0) exitFailure
