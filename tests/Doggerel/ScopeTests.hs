module Main where

import Control.Monad (when)
import Data.List (sort)
import Data.Set (empty, fromList)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.DegreeMap (toMap)
import Doggerel.Scope
import System.Exit (exitFailure)
import Test.HUnit

u :: String -> Units
u = toMap . mkBaseUnit

withPlainDimension :: ScopeFrame -> String -> ScopeFrame
withPlainDimension f d = f `withDimension` (d, empty)

parentDimensionShadowTest = TestCase
  $ assertEqual "parent dimensions are shadowed" expected actual
  where
    expected = [("a", empty), ("b", empty)]
    actual
      = sort
      $ getDimensions
      $ pushScope (initFrame `withPlainDimension` "a")
        `withPlainDimension` "a"
        `withPlainDimension` "b"

parentUnitShadowTest = TestCase
  $ assertEqual "parent units are shadowed" expected actual
  where
    expected = [("b", empty)]
    actual
      = getUnits
      $ pushScope (emptyFrame
          `withPlainDimension` "a"
          `withUnit` ("b", fromList [UnitDim $ toMap $ mkDimension "a"]))
        `withUnit` ("b", empty)

overwriteAssignmentTest = TestCase
  $ assertEqual "replace assignment alters correct assignment" expected actual
  where
    vec n = scalarToVector $ Scalar n $ u "bar"
    dims = DSTerm $ DSTermDim "bar" Nothing 1
    level0 = initFrame `withAssignment` ("foo", vec 1, DSTerm $ DSTermDim "foo" Nothing 1)
    level1 = pushScope level0 `withAssignment` ("foo", vec 2, dims)
    level2 = pushScope level1
    expected = Just ("foo", vec 3, DSTerm $ DSTermDim "bar" Nothing 1)
    actual
      = getAssignmentById (replaceAssignment level2 ("foo", vec 3, dims)) "foo"

replaceInputTest = TestCase
  $ assertEqual "replace input alters in correct scope" expected actual
  where
    sca n = Scalar n $ u "baz"
    level0 = initFrame `withInput` ("foo", Left $ toMap $ mkDimension "bar")
    level1 = pushScope level0
      `withInput` ("foo", Left $ toMap $ mkDimension "baz")
    level2 = pushScope level1
    expected = Just ("foo", Right $ sca 3)
    actual = getInputById (replaceInput level2 ("foo", Right $ sca 3)) "foo"

unitTests = [
    parentDimensionShadowTest,
    parentUnitShadowTest,
    overwriteAssignmentTest,
    replaceInputTest
  ]

main = do
  count <- runTestTT (TestList unitTests)
  when (failures count > 0) exitFailure
