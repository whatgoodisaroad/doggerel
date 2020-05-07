module Test where

import Control.Monad.State
import Data.List (find, sortBy)

import Core
import Ast
import Eval

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



isExistingIdentifier :: Identifier -> ScopeFrame -> Bool
isExistingIdentifier id (Frame dims units _ assignments)
  =  id `elem` dims
  || id `elem` (map fst units)
  || id `elem` (map fst assignments)

execute :: Program -> IO ScopeFrame
execute p
  = fmap snd
  $ flip runStateT initFrame
  $ mapM executeStatement p

executeStatement :: Statement -> (StateT ScopeFrame IO) ()
executeStatement (DeclareDimension dimensionId) = do
  f@(Frame ds us cs as) <- get
  if isExistingIdentifier dimensionId f
    then fail $ "Identifier '" ++ dimensionId ++ "' is already defined."
    else put $ Frame (dimensionId:ds) us cs as

executeStatement (DeclareUnit id maybeDim) = do
  f@(Frame ds us cs as) <- get

  -- Is the unit ID unused?
  if isExistingIdentifier id f
    then fail $ "Identifier '" ++ id ++ "' is already defined."
    else return ()

  -- If there is a dimension spec, does it refer to an existing dimension?
  case maybeDim of
    Nothing -> return ()
    (Just dim) -> if dim `elem` ds
      then return ()
      else fail $ "Reference to undeclared dimension '" ++ dim ++ "'"

  put $ Frame ds ((id, maybeDim):us) cs as

executeStatement (DeclareConversion from to transform) = do
  f@(Frame ds us cs as) <- get

  if from == to
    then fail $ "Cannot declare conversion from a unit to itself"
    else return ()

  let fromUnits = find ((==from).fst) us
  let toUnits = find ((==to).fst) us

  let noUnitMsg = \u -> "Conversion refers to unkown unit '" ++ u ++ "'"
  let partialUnitlessMsg = \u -> "Cannot convert dimensionless unit '" ++ u ++ "'"

  case (fromUnits, toUnits) of
    -- Fail if either side of conversion is unknown units.
    (Nothing, _) -> fail $ noUnitMsg from
    (_, Nothing) -> fail $ noUnitMsg to

    -- Fail if units are of different dimensionalities
    (Just (_, Just fromDim), Just (_, Just toDim)) ->
      if fromDim == toDim
        then return ()
        else fail $ "Cannot declare conversion between units of different "
          ++ "dimensions: from '" ++ fromDim ++ "' to '" ++ toDim ++ "'"
    (Just (_, Nothing), _) ->
      fail $ partialUnitlessMsg from
    (_, Just (_, Nothing)) ->
      fail $ partialUnitlessMsg to

  put $ Frame ds us ((from, to, transform):cs) as

executeStatement (Assignment id expr) = do
  f@(Frame ds us cs as) <- get
  if isExistingIdentifier id f
    then fail $ "Identifier '" ++ id ++ "' is already defined"
    else return ()

  flip mapM_ (referencesOfExpr expr) $ \ref ->
    if not $ isExistingIdentifier ref f
      then fail $ "Expression refers to unknown identifier: '" ++ ref ++ "'"
      else return ()

  put $ Frame ds us cs ((id, expr):as)

executeStatement (Print expr _) = do
  f <- get
  case evaluate f expr of
    Left err -> fail $ show err
    Right v -> lift $ putStrLn $ show $ unifyVector f v


testProgram :: Program
testProgram = [
    DeclareDimension "length",
    DeclareDimension "time",
    DeclareUnit "second" (Just "time"),
    DeclareUnit "minute" (Just "time"),
    DeclareUnit "hour" (Just "time"),
    DeclareUnit "meter" (Just "length"),
    DeclareUnit "kilometer" (Just "length"),
    DeclareConversion "kilometer" "meter" (LinearTransform "" 1000),
    DeclareConversion "hour" "minute" (LinearTransform "" 60),
    DeclareConversion "minute" "second" (LinearTransform "" 60),
    Assignment "x" (ScalarLiteral (Scalar 3.5 (u "meter" `divide` u "second"))),
    Assignment "y" (ScalarLiteral (Scalar 72 (u "kilometer" `divide` u "hour"))),
    Print (BinaryOperatorApply Add (Reference "x") (Reference "y")) Nothing
  ]