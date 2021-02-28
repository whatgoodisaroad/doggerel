module Main where

import Control.Monad (when)
import Data.List (sort)
import Data.Set (empty, fromList)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.DegreeMap (divide, multiply, toMap)
import Doggerel.Scope
import Doggerel.Validation (matchByTermsOnly)
import System.Exit (exitFailure)
import Test.HUnit

d :: Identifier -> Dimensionality
d = toMap . flip Dimension Nothing

di :: Identifier -> Int -> Dimensionality
di id idx = toMap $ Dimension id $ Just idx

matchByTermsOnlySimpleTest = TestCase $ assertBool "simple match" actual
  where
    actual = matchByTermsOnly ds vd
    vd = VecDims $ fromList [
        d "foo" `multiply` d "foo" `divide` d "bar",
        di "baz" 4
      ]
    ds = DSSum [
        DSProduct [
            DSTerm $ DSTermDim "foo" Nothing 2,
            DSTerm $ DSTermDim "bar" Nothing (-1)
          ],
        DSTerm $ DSTermDim "baz" (Just 4) 1
      ]

matchByTermsOnlyProductTest = TestCase $ assertBool "product match" actual
  where
    actual = matchByTermsOnly ds vd
    vd = VecDims $ fromList [
        d "a" `multiply` d "c",
        d "a" `multiply` d "d",
        d "b" `multiply` d "c",
        d "b" `multiply` d "d"
      ]
    ds = DSProduct [
        DSSum [
            DSTerm $ DSTermDim "a" Nothing 1,
            DSTerm $ DSTermDim "b" Nothing 1
          ],
        DSSum [
            DSTerm $ DSTermDim "c" Nothing 1,
            DSTerm $ DSTermDim "d" Nothing 1
          ]
      ]

matchByTermsOnlyBoundedRangeTest = TestCase $ assertBool "bounded ranges" actual
  where
    actual = matchByTermsOnly ds vd
    vd = VecDims $ fromList [
        di "a" 0 `multiply` di "b" 0,
        di "a" 0 `multiply` di "b" 1,
        di "a" 1 `multiply` di "b" 0,
        di "a" 1 `multiply` di "b" 1,
        di "a" 2 `multiply` di "b" 0,
        di "a" 2 `multiply` di "b" 1
      ]
    ds = DSProduct [
        DSTerm $ DSTermRange "a" (Just 0) (Just 2) 1,
        DSTerm $ DSTermRange "b" (Just 0) (Just 1) 1
      ]

unitTests = [
    matchByTermsOnlySimpleTest,
    matchByTermsOnlyProductTest,
    matchByTermsOnlyBoundedRangeTest
  ]

main = do
  count <- runTestTT (TestList unitTests)
  when (failures count > 0) exitFailure
