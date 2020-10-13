module Main where

import Control.Monad (when)
import Data.List (sort)
import Doggerel.Core
import Doggerel.DegreeMap (toMap)
import Doggerel.Scope
import System.Exit (exitFailure)
import Test.HUnit

u :: String -> Units
u = toMap . BaseUnit

parenDimensionShaodwTest = TestCase
  $ assertEqual "parent dimensions are shadowed" expected actual
  where
    expected = ["a", "b"]
    actual
      = sort
      $ getDimensions
      $ pushScope (initFrame `withDimension` "a")
        `withDimension` "a"
        `withDimension` "b"

parenUnitShaodwTest = TestCase
  $ assertEqual "parent units are shadowed" expected actual
  where
    expected = [("b", Nothing), ("bool", Nothing)]
    actual
      = getUnits
      $ pushScope (initFrame
          `withDimension` "a"
          `withUnit` ("b", Just $ toMap $ Dimension "a"))
        `withUnit` ("b", Nothing)

overwriteAssignmentTest = TestCase
  $ assertEqual "replace assignment alters correct assignment" expected actual
  where
    vec n = scalarToVector $ Scalar n $ u "bar"
    level0 = initFrame `withAssignment` ("foo", vec 1)
    level1 = (pushScope level0) `withAssignment` ("foo", vec 2)
    level2 = pushScope level1
    expected = Just ("foo", vec 3)
    actual
      = getAssignmentById (replaceAssignment level2 ("foo", vec 3)) "foo"

unitTests = [
    parenDimensionShaodwTest,
    parenUnitShaodwTest,
    overwriteAssignmentTest
  ]

main = do
  count <- runTestTT (TestList unitTests)
  when (failures count > 0) exitFailure
