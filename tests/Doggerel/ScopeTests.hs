module Main where

import Control.Monad (when)
import Data.List (sort)
import Data.Map.Strict as Map
import Doggerel.Core
import Doggerel.DegreeMap (toMap)
import Doggerel.Scope
import System.Exit (exitFailure)
import Test.HUnit

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

unitTests = [
    parenDimensionShaodwTest,
    parenUnitShaodwTest
  ]

main = do
  count <- runTestTT (TestList unitTests)
  when (failures count > 0) exitFailure
