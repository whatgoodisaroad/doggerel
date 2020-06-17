module Main where

import Control.Monad.Writer (runWriter)
import Doggerel.Ast
import Doggerel.Conversion
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.Eval
import Doggerel.Exec
import System.Exit (exitFailure)
import Test.HUnit

u :: String -> Units
u = toMap . BaseUnit

declareDim = TestCase $ assertEqual "declare a dim" expected actual
  where
    expected = (Right $ Frame ["myDim"] [] [] [], [])
    actual = runWriter result

    result :: WriterIO (Either ExecFail ScopeFrame)
    result = execute [DeclareDimension "myDim"]

printSimpleScalar = TestCase $ assertEqual "print simple scalar" expected actual
  where
    expected = (
        Right $ Frame [] [("mile", Nothing)] [] [],
        ["500.0 mile = {500.0 mile}"]
      )
    actual = runWriter result

    result :: WriterIO (Either ExecFail ScopeFrame)
    result = execute [
        DeclareUnit "mile" Nothing,
        Print (ScalarLiteral (Scalar 500 (u "mile"))) Nothing
      ]

unitTests = [
    declareDim,
    printSimpleScalar
  ]

main = do
  count <- runTestTT (TestList unitTests)
  if failures count > 0 then exitFailure else return ()
