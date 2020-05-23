module Main where

import System.Exit (exitFailure)
import Test.HUnit

import Doggerel.DegreeMap

equalSimpleMaps = TestCase
  $ assertEqual "simple maps are equal" True
  $ toMap "foo" == toMap "foo"
unequalSimpleMaps = TestCase
  $ assertEqual "simple maps are not equal" False
  $ toMap "foo" == toMap "bar"

compareSimpleEqualMaps = TestCase
  $ assertEqual "simple maps are equal" EQ
  $ toMap "foo" `compare` toMap "foo"
compareInequalSimpleMaps = TestCase
  $ assertEqual "simple maps are inequal" LT
  $ toMap 1 `compare` toMap 2
squareGreaterThanSingle = TestCase
  $ assertEqual "square greater than single" GT
  $ a `compare` b
  where
    a = toMap 1 `multiply` toMap 1
    b = toMap 2

showSimpleMap = TestCase
  $ assertEqual "a simple map shows to its value" "'x'"
  $ show $ toMap 'x'
showSimpleCubeMap = TestCase
  $ assertEqual "simple cube map shows to its value cubed" "'x'³"
  $ show $ toMap 'x' `multiply` toMap 'x' `multiply` toMap 'x'
positivesBeforeNegatives = TestCase
  $ assertEqual "list positives before negatives" "1³·2²·3·4⁻¹·5⁻²"
  $ show $ numerator `divide` deniminator
  where
    pow :: Ord a => a -> Int -> DegreeMap a
    pow a 1 = toMap a
    pow a n = toMap a `multiply` pow a (pred n)
    numerator = pow 1 3 `multiply` pow 2 2 `multiply` toMap 3
    deniminator = toMap 4 `multiply` pow 5 2
showNullIsDiemnsionless = TestCase
  $ assertEqual "null is dimensionless" "!dimensionless"
  $ show $ toMap 1 `divide` toMap 1



unitTests = [
    -- Eq
    equalSimpleMaps
  , unequalSimpleMaps

    -- Ord
  , compareSimpleEqualMaps
  , compareInequalSimpleMaps
  , squareGreaterThanSingle

    -- Show
  , showSimpleMap
  , showSimpleCubeMap
  , positivesBeforeNegatives
  , showNullIsDiemnsionless
  ]



main = do
  count <- runTestTT (TestList unitTests)
  if failures count > 0 then exitFailure else return ()
