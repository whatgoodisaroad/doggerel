module Main where

import Data.Map.Strict as Map
import System.Exit (exitFailure)
import Test.HUnit

import Doggerel.DegreeMap
import Doggerel.Core
import Doggerel.Conversion

-- findConversions
findConversionsSimple = TestCase
  $ assertEqual "simple conversion finds way" expected
  $ findConversions cdb goal start
  where
    expected = Just $ [LinearTransform "" 1000]
    cdb = [
        Conversion
          (LinearTransform "" 1000)
          (BaseUnit "kilometer")
          (BaseUnit "meter")
      ]
    goal = toMap $ BaseUnit "meter"
    start = toMap $ BaseUnit "kilometer"

findConversionsInverse = TestCase
  $ assertEqual "inverse direct conversion" expected
  $ findConversions cdb goal start
  where
    expected = Just $ [LinearTransform "" 1000]
    cdb = [
          Conversion
            (LinearTransform "" 1000)
            (BaseUnit "kilometer")
            (BaseUnit "meter")
        ]
    goal = fromMap $ fromList
      [(BaseUnit "meter", 1), (BaseUnit "second", -1)]
    start = fromMap $ fromList
      [(BaseUnit "kilometer", 1), (BaseUnit "second", -1)]

findConversionsIndirect = TestCase
  $ assertEqual "inverse direct conversion" expected
  $ findConversions cdb goal start
  where
    expected = Just $ [
        InverseOf $ LinearTransform "" 1000,
        InverseOf $ LinearTransform "" 60,
        InverseOf $ LinearTransform "" 60
      ]
    cdb = [
        Conversion
          (LinearTransform "" 1000)
          (BaseUnit "kilometer")
          (BaseUnit "meter"),
        Conversion
          (LinearTransform "" 60)
          (BaseUnit "hour")
          (BaseUnit "minute"),
        Conversion
          (LinearTransform "" 60)
          (BaseUnit "minute")
          (BaseUnit "second")
      ]
    goal = fromMap $ fromList
      [(BaseUnit "kilometer", 1), (BaseUnit "second", -1)]
    start = fromMap $ fromList
      [(BaseUnit "meter", 1), (BaseUnit "hour", -1)]

unitTests = [
    findConversionsSimple
  , findConversionsInverse
  , findConversionsIndirect
  ]

main = do
  count <- runTestTT (TestList unitTests)
  if failures count > 0 then exitFailure else return ()
