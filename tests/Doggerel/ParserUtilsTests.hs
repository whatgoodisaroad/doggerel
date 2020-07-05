module Main where

import Data.Set (Set, empty, fromList)
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.ParserUtils
import System.Exit (exitFailure)
import Test.HUnit
import Text.ParserCombinators.Parsec

u :: String -> Units
u = toMap . BaseUnit

-- Given a parser, convert it to a parser that consumes all input.
withEof :: GenParser Char () a -> GenParser Char () a
withEof p = p >>= \a -> eof >> return a

execParser :: GenParser Char () a -> String -> Either ParseError a
execParser p i = parse (withEof p) "fail" i

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
    actual = execParser unitsP "ghi^2 abc def / lmn opq^3"
    num = u "abc" `multiply` u "def" `multiply` u "ghi" `multiply` u "ghi"
    den = u "lmn" `multiply` u "opq" `multiply` u "opq" `multiply` u "opq"

unitsWithoutNum
  = TestCase $ assertEqual "parse units fails without denominator" True actual
  where
    actual = parserFails unitsP "/ foo bar"

unitTests = [
    identifier,
    identifierInvalidChars,
    idWithInvalidFirstChar,
    units,
    unitFraction,
    unitsWithExps,
    unitsWithoutNum
  ]

main = do
  count <- runTestTT (TestList unitTests)
  if failures count > 0 then exitFailure else return ()
