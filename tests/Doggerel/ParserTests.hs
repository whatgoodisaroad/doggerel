module Main where

import Data.Set (Set, empty, fromList)
import Doggerel.Ast
import Doggerel.Conversion
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.Parser (parseFile)
import System.Exit (exitFailure)
import Test.HUnit
import Text.ParserCombinators.Parsec

u :: String -> Units
u = toMap . BaseUnit

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
  $ Right [DeclareUnit "foo" $ Just $ toMap "bar"]

unitDeclPTestNoDim
  = assertParsesTo "parses unit in no dim" "unit foo;"
  $ Right [DeclareUnit "foo" Nothing]

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

conversionDeclPTest
  = assertParsesTo "parse simple converstion" "convert foo = 4.2 * bar;"
  $ Right [DeclareConversion "foo" "bar" $ LinearTransform 4.2]

printPTestNoOpts
  = assertParsesTo "print with no options" "print 200 f + 10 g;"
  $ Right [
      Print
        (BinaryOperatorApply Add
          (Literal $ Scalar 200 $ u "f")
          (Literal $ Scalar 10 $ u "g"))
        Nothing
        empty
    ]

printPTestUnitsOpt
  = assertParsesTo "print with units option" "print (12 x) with units: y;"
  $ Right [Print (Literal $ Scalar 12 $ u "x") (Just $ u "y") empty]

printPTestStyleOpt
  = assertParsesTo "print with style option" "print z with style: fractions;"
  $ Right [Print (Reference "z") Nothing $ fromList [MultiLineFractions]]

printPTestStyleAndUnitsOpts
  = assertParsesTo "print with style and untis options"
  "print x with style: fractions, units: y;"
  $ Right [Print (Reference "x") (Just $ u "y") $ fromList [MultiLineFractions]]

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

unitTests = [
    -- dim
    dimDeclPTest,

    -- unit
    unitDeclPTestInDim,
    unitDeclPTestNoDim,

    -- let
    assignmentPNoOpts,
    assignmentPWithScalarConstraint,

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
    relationPTest
  ]

main = do
  count <- runTestTT (TestList unitTests)
  if failures count > 0 then exitFailure else return ()
