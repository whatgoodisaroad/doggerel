module Main where

import Control.Monad (when)
import Data.Map.Strict as Map
import Doggerel.DegreeMap
import System.Exit (exitFailure)
import Test.HUnit

-- Eq
equalSimpleMaps = TestCase
  $ assertEqual "simple maps are equal" True
  $ toMap "foo" == toMap "foo"
unequalSimpleMaps = TestCase
  $ assertEqual "simple maps are not equal" False
  $ toMap "foo" == toMap "bar"

-- Ord
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

-- Show
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

-- fromMap
fromMapRemovesZeros = TestCase
  $ assertEqual "fromMap removes values mapped to zero" expected
  $ fromMap $ Map.fromList [('a', 1), ('b', 0)]
  where
    expected = fromMap $ singleton 'a' 1

-- invert
inversionFindsReciprocal = TestCase
  $ assertEqual "invert finds reciprocal" expected
  $ invert $ fromMap $ Map.fromList [('a', 2), ('b', 1), ('c', -1), ('d', -2)]
  where
    expected = fromMap $ Map.fromList [('a', -2), ('b', -1), ('c', 1), ('d', 2)]

-- multiply
multiplySumsNonZeroDegrees = TestCase
  $ assertEqual "multiply sums nonzero degrees" expected
  $ left `multiply` right
  where
    left = fromMap $ Map.fromList [('a', 5), ('b', 2), ('c', 1)]
    right = fromMap $ Map.fromList [('b', 3), ('c', -1), ('d', -4)]
    expected = fromMap $ Map.fromList [('a', 5), ('b', 5), ('d', -4)]

normalizeInverseTest = TestCase
  $ assertEqual "normalize inverse makes inverses identical" True
  $ normalizeInverse a == normalizeInverse b
  where
    a = fromMap $ Map.fromList [('b', 3), ('c', -1), ('d', -4)]
    b = invert a

expTest = TestCase
  $ assertEqual "exponents" expected
  $ expDM (fromMap $ Map.fromList [('e', 12), ('f', -4)]) 0.25
  where
    expected = Just $ fromMap $ Map.fromList [('e', 3), ('f', -1)]

intExpTest = TestCase
  $ assertEqual "exponents" expected
  $ intExpDM (fromMap $ Map.fromList [('a', 3), ('b', 1), ('c', -2)]) (-2)
  where
    expected = fromMap $ Map.fromList [('a', -6), ('b', -2), ('c', 4)]

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
    -- fromMap
  , fromMapRemovesZeros
    -- invert
  , inversionFindsReciprocal
  -- multiply
  , multiplySumsNonZeroDegrees
  -- normalizeInverse
  , normalizeInverseTest
  -- expDM
  , expTest
  , intExpTest
  ]

main = do
  count <- runTestTT (TestList unitTests)
  when (failures count > 0) exitFailure
