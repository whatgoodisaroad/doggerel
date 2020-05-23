module Main where

import Test.HUnit

import Doggerel.CoreTests as CoreTests

main = runTestTT $ TestList $ CoreTests.unitTests