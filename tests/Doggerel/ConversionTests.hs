module Main where

import Control.Monad (when)
import Data.Map.Strict as Map
import Doggerel.Conversion
import Doggerel.Core
import Doggerel.DegreeMap
import System.Exit (exitFailure)
import Test.HUnit

u = toMap . BaseUnit

-- findConversions
findConversionsSimple = TestCase
  $ assertEqual "simple conversion finds way" expected
  $ findConversions cdb goal start
  where
    expected = Just [LinearTransform 1000]
    cdb = [Conversion (LinearTransform 1000) (u "kilometer") (u "meter")]
    goal = u "meter"
    start = u "kilometer"

findConversionsCompound = TestCase
  $ assertEqual "compound conversion finds way" expected
  $ findConversions cdb goal start
  where
    expected = Just [LinearTransform 3]
    cdb = [
        Conversion (LinearTransform 3) (u "foo" `multiply` u "foo") (u "bar")
      ]
    goal = u "bar"
    start = u "foo" `multiply` u "foo"

findConversionsInverse = TestCase
  $ assertEqual "inverse direct conversion" expected
  $ findConversions cdb goal start
  where
    expected = Just [LinearTransform 1000]
    cdb = [Conversion (LinearTransform 1000) (u "kilometer") (u "meter")]
    goal = fromMap $ fromList
      [(BaseUnit "meter", 1), (BaseUnit "second", -1)]
    start = fromMap $ fromList
      [(BaseUnit "kilometer", 1), (BaseUnit "second", -1)]

findConversionsIndirect = TestCase
  $ assertEqual "inverse direct conversion" expected
  $ findConversions cdb goal start
  where
    expected = Just [
        InverseOf $ LinearTransform 1000,
        InverseOf $ LinearTransform 60,
        InverseOf $ LinearTransform 60
      ]
    cdb = [
        Conversion (LinearTransform 1000) (u "kilometer") (u "meter"),
        Conversion (LinearTransform 60) (u "hour") (u "minute"),
        Conversion (LinearTransform 60) (u "minute") (u "second")
      ]
    goal = fromMap $ fromList
      [(BaseUnit "kilometer", 1), (BaseUnit "second", -1)]
    start = fromMap $ fromList
      [(BaseUnit "meter", 1), (BaseUnit "hour", -1)]

findConversionsFailsWithoutPath = TestCase
  $ assertEqual "fails to convert when path is not available" Nothing
  $ findConversions [] (toMap $ BaseUnit "meter") (toMap $ BaseUnit "mile")

unitTests = [
    findConversionsSimple
  , findConversionsCompound
  , findConversionsInverse
  , findConversionsIndirect
  , findConversionsFailsWithoutPath
  ]

main = do
  count <- runTestTT (TestList unitTests)
  when (failures count > 0) exitFailure
