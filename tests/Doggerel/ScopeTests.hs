module Main where

import Control.Monad (when)
import Data.List (sort)
import Data.Set (empty)
import Doggerel.Core
import Doggerel.DegreeMap (toMap)
import Doggerel.Scope
import System.Exit (exitFailure)
import Test.HUnit

u :: String -> Units
u = toMap . mkBaseUnit

withPlainDimension :: ScopeFrame -> String -> ScopeFrame
withPlainDimension f d = f `withDimension` (d, empty)

parentDimensionShaodwTest = TestCase
  $ assertEqual "parent dimensions are shadowed" expected actual
  where
    expected = [("a", empty), ("b", empty)]
    actual
      = sort
      $ getDimensions
      $ pushScope (initFrame `withPlainDimension` "a")
        `withPlainDimension` "a"
        `withPlainDimension` "b"

parentUnitShaodwTest = TestCase
  $ assertEqual "parent units are shadowed" expected actual
  where
    expected = [("b", Nothing), ("bool", Nothing)]
    actual
      = getUnits
      $ pushScope (initFrame
          `withPlainDimension` "a"
          `withUnit` ("b", Just $ toMap $ Dimension "a"))
        `withUnit` ("b", Nothing)

overwriteAssignmentTest = TestCase
  $ assertEqual "replace assignment alters correct assignment" expected actual
  where
    vec n = scalarToVector $ Scalar n $ u "bar"
    level0 = initFrame `withAssignment` ("foo", vec 1)
    level1 = pushScope level0 `withAssignment` ("foo", vec 2)
    level2 = pushScope level1
    expected = Just ("foo", vec 3)
    actual
      = getAssignmentById (replaceAssignment level2 ("foo", vec 3)) "foo"

replaceInputTest = TestCase
  $ assertEqual "replace input alters in correct scope" expected actual
  where
    sca n = Scalar n $ u "baz"
    level0 = initFrame `withInput` ("foo", Left $ toMap $ Dimension "bar")
    level1 = pushScope level0
      `withInput` ("foo", Left $ toMap $ Dimension "baz")
    level2 = pushScope level1
    expected = Just ("foo", Right $ sca 3)
    actual = getInputById (replaceInput level2 ("foo", Right $ sca 3)) "foo"


unitTests = [
    parentDimensionShaodwTest,
    parentUnitShaodwTest,
    overwriteAssignmentTest,
    replaceInputTest
  ]

main = do
  count <- runTestTT (TestList unitTests)
  when (failures count > 0) exitFailure
