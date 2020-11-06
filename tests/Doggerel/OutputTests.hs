module Main where

import Control.Monad (when)
import Data.Set (Set, empty, fromList)
import Doggerel.Ast;
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.Output
import System.Exit (exitFailure)
import Test.HUnit

u :: String -> Units
u = toMap . mkBaseUnit

simpleOneline = TestCase $ assertEqual "simple one line print" expected actual
  where
    expected = ["foo = {5.0 bar·baz⁻¹}"]
    actual = prettyPrint empty (Reference "foo")
      $ scalarToVector
      $ Scalar 5.0
      $ u "bar" `divide` u "baz"

simpleMultiline = TestCase $ assertEqual "simple multi line print" expected actual
  where
    expected = [
        "      ⎧ 5.0 bar ⎫",
        "foo = ⎨ ─────── ⎬",
        "      ⎩     baz ⎭"
      ]
    actual = prettyPrint (fromList [MultiLineFractions]) (Reference "foo")
      $ scalarToVector
      $ Scalar 5.0
      $ u "bar" `divide` u "baz"

multilineWithoutFraction
  = TestCase $ assertEqual "mnultiline but no fraction" expected actual
  where
    expected = ["foo = {5.0 bar·baz}"]
    actual = prettyPrint (fromList [MultiLineFractions]) (Reference "foo")
      $ scalarToVector
      $ Scalar 5.0
      $ u "bar" `multiply` u "baz"

unitTests = [
    simpleOneline,
    simpleMultiline,
    multilineWithoutFraction
  ]

main = do
  count <- runTestTT (TestList unitTests)
  when (failures count > 0) exitFailure
