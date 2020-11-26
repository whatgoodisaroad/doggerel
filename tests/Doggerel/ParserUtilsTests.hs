module Main where

import Control.Monad (when)
import Data.Set (Set, empty, fromList)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.ParserUtils
import System.Exit (exitFailure)
import Test.HUnit
import Text.ParserCombinators.Parsec

u :: String -> Units
u = toMap . mkBaseUnit

ui :: String -> Int -> Units
ui u i = toMap $ BaseUnit u $ Just i

-- Given a parser, convert it to a parser that consumes all input.
withEof :: GenParser Char () a -> GenParser Char () a
withEof p = p >>= \a -> eof >> return a

execParser :: GenParser Char () a -> String -> Either ParseError a
execParser p = parse (withEof p) "fail"

parserFails :: GenParser Char () a -> String -> Bool
parserFails p i = case parse (withEof p) "fail" i of
  Left _ -> True
  Right _ -> False

identifier = TestCase $ assertEqual "parse ID" expected actual
  where
    expected = Right "foo_bar1234"
    actual = execParser identifierP "foo_bar1234"

identifierInvalidChars
  = TestCase $ assertEqual "ID with unallowed chars" True actual
  where
    actual = parserFails identifierP "foo$bar"

identifierReservedWord
  = TestCase $ assertEqual "ID with reserved word" True actual
  where
    actual = parserFails identifierP "with"

idWithInvalidFirstChar
  = TestCase $ assertEqual "ID with initial number" True actual
  where
    actual = parserFails identifierP "9baz"

units = TestCase $ assertEqual "parse units" expected actual
  where
    expected = Right $ u "abc"
    actual = execParser unitsP "abc"

unitFraction
  = TestCase $ assertEqual "parse units with fraction" expected actual
  where
    expected = Right $ u "abc" `divide` u "def"
    actual = execParser unitsP "abc / def"

unitsWithExps
  = TestCase $ assertEqual "parse units with exponents" expected actual
  where
    expected = Right $ num `divide` den
    actual = execParser unitsP "abc def ghi^2 / lmn opq^3"
    num = u "abc" `multiply` u "def" `multiply` u "ghi" `multiply` u "ghi"
    den = u "lmn" `multiply` u "opq" `multiply` u "opq" `multiply` u "opq"

unitsWithIndices
  = TestCase $ assertEqual "parse units with indices" expected actual
  where
    expected = Right $ num `divide` den
    actual = execParser unitsP "A B(2) C(3)^2 / B(1) C(3)"
    num = u "A" `multiply` ui "B" 2 `multiply` ui "C" 3 `multiply` ui "C" 3
    den = ui "B" 1 `multiply` ui "C" 3

unitsWithoutNum
  = TestCase $ assertEqual "parse units fails without denominator" True actual
  where
    actual = parserFails unitsP "/ foo bar"

referenceExpression
  = TestCase $ assertEqual "parse reference expression" expected actual
  where
    expected = Right $ Reference "foo"
    actual = execParser expressionP "foo"

parenReferenceExpression
  = TestCase
  $ assertEqual "parse reference expression with parens" expected actual
  where
    expected = Right $ Reference "foo"
    actual = execParser expressionP "(  foo)"

scalarExpression
  = TestCase $ assertEqual "parse scalar expression" expected actual
  where
    expectedUnits = u "diameter" `divide` u "radius"
    expected = Right $ Literal $ Scalar 3.14159 expectedUnits
    actual = execParser expressionP "3.14159 diameter / radius"

infixOpExpression
  = TestCase $ assertEqual "parse infix operator expression" expected actual
  where
    expected
      = Right $ BinaryOperatorApply Multiply (Reference "foo") (Reference "bar")
    actual = execParser expressionP "foo * bar"

prefixOpExpression
  = TestCase $ assertEqual "parse prefix operator expression" expected actual
  where
    expected
      = Right
      $ UnaryOperatorApply Negative
      $ BinaryOperatorApply Divide (Reference "foo") (Reference "bar")
    actual = execParser expressionP "-(foo / bar)"

mixedExpression
  = TestCase $ assertEqual "parse complex expression" expected actual
  where
    expected
      = Right
      $ BinaryOperatorApply Divide
          (BinaryOperatorApply Multiply
            (Literal $ Scalar 4 $ u "mile")
            (Reference "foo"))
          (Literal $ Scalar 16 $ u "kilogram")
    actual = execParser expressionP "4 mile * foo / 16 kilogram"

operatorPrecedence
  = TestCase $ assertEqual "parse expression with precedence" expected actual
  where
    expected
      = Right
      $ BinaryOperatorApply
        Subtract
        (BinaryOperatorApply
          Add
          (BinaryOperatorApply Multiply (Reference "a") (Reference "b"))
          (Reference "c"))
        (BinaryOperatorApply
          Divide
          (BinaryOperatorApply Multiply (Reference "d") (Reference "e"))
          (Reference "f"))
    actual = execParser expressionP "a * b + c - d * e / f"

functionExpression
  = TestCase $ assertEqual "parse simple function application" expected actual
  where
    expected
      = Right
      $ FunctionApply "foo" (Literal $ Scalar 42 $ u "inch")
    actual = execParser expressionP "foo(42 inch)"

unitsExpression
  = TestCase $ assertEqual "parse units expression" expected actual
  where
    expected
      = Right
      $ BinaryOperatorApply Divide
        (Reference $ u "meter")
        (BinaryOperatorApply Subtract (Literal 12.0) (Reference $ u "second"))
    actual = execParser unitsExpressionP "meter / (12 - second)"

unitsExpressionNegation
  = TestCase
  $ assertEqual "parse units expression with negation" expected actual
  where
    expected = Right $ UnaryOperatorApply Negative $ Reference $ u "meter"
    actual = execParser unitsExpressionP "-meter"

postfixExponentUnitsExpression
  = TestCase $ assertEqual "parse exponent" expected actual
  where
    expected = Right $ UnaryOperatorApply (Exponent 2) (Reference $ u "meter")
    actual = execParser unitsExpressionP "meter^2"

unitTests = [
    identifier,
    identifierInvalidChars,
    idWithInvalidFirstChar,
    identifierReservedWord,
    units,
    unitFraction,
    unitsWithExps,
    unitsWithIndices,
    unitsWithoutNum,
    referenceExpression,
    parenReferenceExpression,
    scalarExpression,
    infixOpExpression,
    prefixOpExpression,
    mixedExpression,
    operatorPrecedence,
    functionExpression,
    unitsExpression,
    unitsExpressionNegation,
    postfixExponentUnitsExpression
  ]

main = do
  count <- runTestTT (TestList unitTests)
  when (failures count > 0) exitFailure
