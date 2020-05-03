module Test where

import Data.List (sortBy)

import Core
import Ast

executeTransform :: Transformation -> Quantity -> Quantity
executeTransform Inversion x = 1 / x
executeTransform (LinearTransform _ f) x = f * x
executeTransform (AffineTransForm _ m b) x = m * x + b
executeTransform (InverseOf Inversion) x = x
executeTransform (InverseOf (LinearTransform _ f)) x = x / f
executeTransform (InverseOf (AffineTransForm _ m b)) x = (x - b) / m

executeConversion :: [Transformation] -> Quantity -> Quantity
executeConversion = flip $ foldr executeTransform


-- Length
inch = BaseUnit "inch"
foot = BaseUnit "foot"
mile = BaseUnit "mile"
meter = BaseUnit "meter"
kilometer = BaseUnit "kilometer"

-- Temperature
fahrenheit = BaseUnit "fahrenheit"
celsius = BaseUnit "celsius"
kelvin = BaseUnit "kelvin"

-- Time
second = BaseUnit "second"
minute = BaseUnit "minute"
hour = BaseUnit "hour"
day = BaseUnit "day"
year = BaseUnit "year"

convertDb :: [Conversion]
convertDb = [
    Conversion (LinearTransform "foot->inch" 12) foot inch,
    Conversion (LinearTransform "mile->foot" 5280) mile foot,
    Conversion (LinearTransform "mile->kilometer" 1.60934) mile kilometer,
    Conversion (LinearTransform "kilometer->meter" 1000) kilometer meter,
    Conversion (AffineTransForm "celsius->kelvin" 1 273.15) celsius kelvin,
    Conversion (AffineTransForm "celsius->fahrenheit" (9/5) 32) celsius fahrenheit,
    Conversion (LinearTransform "minute->second" 60) minute second,
    Conversion (LinearTransform "hour->minute" 60) hour minute,
    Conversion (LinearTransform "day->hour" 24) day hour,
    Conversion (LinearTransform "year->day" 365) year day
  ]

convertTo :: [Conversion] -> Scalar -> DegreeMap BaseUnit -> Maybe Scalar
convertTo cdb scalar dest
  = (fmap fst)
  $ convertWithAnnotations cdb scalar dest

convertWithAnnotations ::
     [Conversion]
  -> Scalar
  -> DegreeMap BaseUnit
  -> Maybe (Scalar, [String])
convertWithAnnotations cdb (Scalar magnitude source) dest = do
  sequence <- bestSequence $ findConversions cdb dest source
  let magnitude' = executeConversion sequence magnitude
  return $ (Scalar magnitude' dest, map transformToAnnotation sequence)

transformToAnnotation :: Transformation -> String
transformToAnnotation Inversion = "Inverted"
transformToAnnotation (LinearTransform label factor) =
     "Conversion for " ++ label
  ++ ", multiply by " ++ show factor
transformToAnnotation (AffineTransForm label m b) =
     "Conversion for " ++ label ++ ", "
  ++ (if m == 1 then "" else "multiply by " ++ show m ++ " and ")
  ++ "add " ++ show b
transformToAnnotation (InverseOf (LinearTransform label factor)) =
     "Inverse conversion for " ++ label
  ++ ", divide by " ++ show factor
transformToAnnotation (InverseOf (AffineTransForm label m b)) =
     "Inverse conversion for " ++ label
  ++ ", subtract by " ++ show b
  ++ if m == 1 then "" else " and divide by " ++ show m

-- For testing

u :: String -> DegreeMap BaseUnit
u = toMap . BaseUnit

(#*), (#/) :: DegreeMap BaseUnit -> DegreeMap BaseUnit -> DegreeMap BaseUnit
(#*) = multiply
(#/) = divide

(#) :: Quantity -> String -> Scalar
m # bu = Scalar m $ toMap $ BaseUnit bu

(##) :: Quantity -> DegreeMap BaseUnit -> Scalar
(##) = Scalar

_convert :: Scalar -> DegreeMap BaseUnit -> IO ()
_convert s d = do
  putStrLn $ "Converting " ++ show s ++ " to " ++ show d ++ ":"
  case convertWithAnnotations convertDb s d of
    Nothing -> putStrLn "    Failed to find conversion"
    Just (result, steps) -> do
      putStrLn $ "    result = " ++ show result
      putStrLn "Steps:"
      mapM_ putStrLn $ map ("    "++) steps

mps = u "meter" #/ u "second"
mips = u "mile" #/ u "second"
mipm = u "mile" #/ u "minute"
miph = u "mile" #/ u "hour"

example :: IO ()
example = 3 ## mps `_convert` miph



bestSequence :: [[Transformation]] -> Maybe [Transformation]
bestSequence [] = Nothing
bestSequence ts
  = Just
  $ head
  $ sortBy (\a b -> length a `compare` length b)
  $ reverse
  $ ts

