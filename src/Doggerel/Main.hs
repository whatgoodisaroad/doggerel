module Main where

import Doggerel.DegreeMap
import Doggerel.Core
import Doggerel.Conversion
import Doggerel.Ast
import Doggerel.Eval
import Doggerel.Exec

-- -- Length
-- inch = BaseUnit "inch"
-- foot = BaseUnit "foot"
-- mile = BaseUnit "mile"
-- meter = BaseUnit "meter"
-- kilometer = BaseUnit "kilometer"

-- -- Temperature
-- fahrenheit = BaseUnit "fahrenheit"
-- celsius = BaseUnit "celsius"
-- kelvin = BaseUnit "kelvin"

-- -- Time
-- second = BaseUnit "second"
-- minute = BaseUnit "minute"
-- hour = BaseUnit "hour"
-- day = BaseUnit "day"
-- year = BaseUnit "year"

-- convertDb :: [Conversion]
-- convertDb = [
--     Conversion (LinearTransform 12) foot inch,
--     Conversion (LinearTransform 5280) mile foot,
--     Conversion (LinearTransform 1.60934) mile kilometer,
--     Conversion (LinearTransform 1000) kilometer meter,
--     Conversion (AffineTransForm 1 273.15) celsius kelvin,
--     Conversion (AffineTransForm (9/5) 32) celsius fahrenheit,
--     Conversion (LinearTransform 60) minute second,
--     Conversion (LinearTransform 60) hour minute,
--     Conversion (LinearTransform 24) day hour,
--     Conversion (LinearTransform 365) year day
--   ]

u :: String -> DegreeMap BaseUnit
u = toMap . BaseUnit

testProgram :: Program
testProgram = [
    DeclareDimension "length",
    DeclareDimension "time",

    DeclareUnit "second" (Just "time"),
    DeclareUnit "minute" (Just "time"),
    DeclareUnit "hour" (Just "time"),
    DeclareUnit "meter" (Just "length"),
    DeclareUnit "kilometer" (Just "length"),
    DeclareUnit "mile" (Just "length"),

    DeclareConversion "kilometer" "meter" (LinearTransform 1000),
    DeclareConversion "hour" "minute" (LinearTransform 60),
    DeclareConversion "minute" "second" (LinearTransform 60),
    DeclareConversion "mile" "kilometer" (LinearTransform 1.60934),

    Assignment "x" (ScalarLiteral (Scalar 1 (u "meter" `divide` u "second"))),
    Assignment "y" (ScalarLiteral (Scalar 1 (u "kilometer" `divide` u "hour"))),
    Assignment "z" (ScalarLiteral (Scalar 10 (u "minute"))),
    Assignment "w" (ScalarLiteral (Scalar 0.5 (u "mile"))),

    Print (Reference "x") Nothing,
    Print (Reference "z") Nothing,
    Print (Reference "w") Nothing,
    Print (BinaryOperatorApply Add (Reference "w") (BinaryOperatorApply Multiply (Reference "x") (Reference "z"))) Nothing,

    Print (Reference "doesNotExist") Nothing


    -- Print (Reference "x") Nothing,
    -- Print (Reference "y") Nothing,
    -- Print (BinaryOperatorApply Add (Reference "x") (Reference "y")) Nothing
  ]

main = execute testProgram >>= (\_ -> return ())