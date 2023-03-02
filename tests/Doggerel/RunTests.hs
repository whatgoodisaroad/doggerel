module Main where

import Control.Monad (when)
import Control.Monad.State.Lazy (runState)
import Doggerel.Exec (TestIO)
import Doggerel.Run (executeSource, loadedStandardFrame)
import Doggerel.Scope
import System.Exit (exitFailure)
import System.IO
import Test.HUnit

-- Given a string of Doggerel source code, returns the output of that program.
sourceToOutput :: String -> [String]
sourceToOutput = sourceToOutput' initFrame []

sourceToOutput' :: ScopeFrame -> [String] -> String -> [String]
sourceToOutput' frame inputs source =
  let
    t = executeSource frame source :: TestIO ()
    (_, (_, _, _, iout)) = runState t ([], inputs, [], [])
  in
    iout

simpleProgram = TestCase $ assertEqual "simple program" expected actual
  where
    expected = [
        "x = {4.5 mile}"
      ]
    actual = sourceToOutput $ unlines [
        "dim length;",
        "unit mile of length;",
        "let x = 4.5 mile;",
        "print x;"
      ]

example1Test src = TestCase $ assertEqual "example 1" expected actual
  where
    expected = [
        "velocity × duration = {33540.0 meter}",
        "velocity = {1.3e-2 kilometer·second⁻¹}"
      ]
    actual = sourceToOutput src

example2Test src = TestCase $ assertEqual "example 2" expected actual
  where
    expected = [
        "speedAtImpact = {92.16647818360322 mile·hour⁻¹}"
      ]
    actual = sourceToOutput src

example3Test src = TestCase $ assertEqual "example 3" expected actual
  where
    expected = [
        "volumeOfTank × densityOfPetrol = {149.78 kilogram}"
      ]
    actual = sourceToOutput src

bmiForPairExampleTest stdlib src = TestCase $ assertEqual "BMI example (for pair)" expected actual
  where
    expected = [
        "Enter a scalar of dimensionality {bool}",
        "Enter a scalar of dimensionality {length}",
        "Enter a scalar of dimensionality {mass}",
        "bmi_formula(height + weight) = {24.412259534362946 bmi}"
      ]
    actual = sourceToOutput' stdlib [
        "1 bool",
        "6 foot",
        "180 pound"
      ] src

bmiThresholdsExampleTest stdlib src = TestCase $ assertEqual "BMI example (for height)" expected actual
  where
    expected = [
        "Enter a scalar of dimensionality {bool}",
        "Enter a scalar of dimensionality {length}",
        "very_severely_underweight = {48.6 kilogram}",
        "severely_underweight = {51.84 kilogram}",
        "underweight = {59.940000000000005 kilogram}",
        "normal_healthy = {81.0 kilogram}",
        "overweight = {97.2 kilogram}",
        "moderately_obese = {113.4 kilogram}",
        "severely_obese = {129.60000000000002 kilogram}"
      ]
    actual = sourceToOutput' stdlib [
        "0 bool",
        "180 centimeter"
      ] src

noxConcentrationExampleTest src = TestCase $ assertEqual "Nox concentration example" expected actual
  where
    expected = [
        "                 ⎧ 24.62746497724636 NOx·gram ⎫",
        "mass_flow_rate = ⎨ ────────────────────────── ⎬",
        "                 ⎩                       hour ⎭"
      ]
    actual = sourceToOutput src

relationExampleTest src = TestCase $ assertEqual "Relation example" expected actual
  where
    expected = [
        "It shouldn't break of course"
      ]
    actual = sourceToOutput src

main = do
  -- Load local source files.
  stdlib <- loadedStandardFrame initFrame False
  example1src <- readFile "examples/example.dog"
  example2src <- readFile "examples/example2.dog"
  example3src <- readFile "examples/example3.dog"
  bmiExampleSrc <- readFile "examples/bmi.dog"
  noxConcentrationExampleSrc <- readFile "examples/nox_concentration.dog"
  relationExampleSrc <- readFile "examples/relation.dog"

  count <- runTestTT $ TestList [
      simpleProgram,
      example1Test example1src,
      example2Test example2src,
      example3Test example3src,
      bmiForPairExampleTest stdlib bmiExampleSrc,
      bmiThresholdsExampleTest stdlib bmiExampleSrc,
      noxConcentrationExampleTest noxConcentrationExampleSrc,
      relationExampleTest relationExampleSrc
    ]
  when (failures count > 0) exitFailure
