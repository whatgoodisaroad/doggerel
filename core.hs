import Control.Monad (guard)
import Data.List (find, intersperse, nub, sort, sortBy)
import Data.Map.Strict as Map (
    Map,
    alter,
    filter,
    keys,
    lookup,
    null,
    singleton,
    toList,
    unionWith,
    update
  )
import Data.Set (Set, member)
import Data.Containers.ListUtils (nubOrdOn)

data DegreeMap a = DegreeMap (Map a Int)

instance Eq a => Eq (DegreeMap a) where
  (DegreeMap m1) == (DegreeMap m2) = m1 == m2

instance Ord a => Ord (DegreeMap a) where
  a `compare` b
    = (maximum $ keys $ getMap a) `compare` (maximum $ keys $ getMap b)

instance (Ord a, Show a) => Show (DegreeMap a) where
  show (DegreeMap m)
    | Map.null m = "{dimensionless}"
    | otherwise
        = concat
        $ intersperse "·"
        $ flip map (sortBy comapreDegree $ toList m)
        $ \(a, o) -> show a ++ if o == 1 then "" else intToSuperscript o
        where
          comapreDegree (_, a) (_, b) = if a > 0 && b > 0
            then a `compare` b else b `compare` a

intToSuperscript :: Int -> String
intToSuperscript n
  | n == 0 = (:[]) $ superscriptDigits !! 0
  | n < 0 = superscriptMinus : (intToSuperscript $ 0 - n)
  | otherwise = reverse $ s n
  where
    superscriptDigits = "⁰¹²³⁴⁵⁶⁷⁸⁹"
    superscriptMinus = '⁻'
    s :: Int -> String
    s 0 = ""
    s n = (superscriptDigits !! (n `mod` 10)) : (s $ n `div` 10)

getMap :: DegreeMap a -> Map a Int
getMap (DegreeMap m) = m

removeNull :: Map k Int -> Map k Int
removeNull = Map.filter (/= 0)

invert :: DegreeMap a -> DegreeMap a
invert = DegreeMap . fmap (\d -> 0 - d) . getMap

multiply1 :: Ord a => a -> Int -> DegreeMap a -> DegreeMap a
multiply1 a d1
  = DegreeMap
  . removeNull
  . update (\d2 -> Just $ d1 + d2) a
  . getMap

multiply :: Ord a => DegreeMap a -> DegreeMap a -> DegreeMap a
multiply (DegreeMap m1) (DegreeMap m2)
  = DegreeMap
  $ removeNull
  $ unionWith (+) m1 m2

divide :: Ord a => DegreeMap a -> DegreeMap a -> DegreeMap a
divide dm1 dm2 = multiply dm1 $ invert dm2

getOrder :: Ord a => DegreeMap a -> a -> Maybe Int
getOrder (DegreeMap m) a = a `Map.lookup` m

hasNumerator, hasDenominator :: Ord a => DegreeMap a -> a -> Bool
hasNumerator m = any (> 0) . getOrder m
hasDenominator m = any (< 0) . getOrder m

toMap :: a -> DegreeMap a
toMap = DegreeMap . flip singleton 1

-- Concrete types

data BaseUnit = BaseUnit String deriving (Eq, Ord)

instance Show BaseUnit where show (BaseUnit s) = s

data BaseDimensionality = BaseDimensionality String

-- Conversion database

type Quantity = Double

data Conversion
  = Conversion Transformation BaseUnit BaseUnit -- transform source dest
  deriving Show

sourceOfConversion, destOfConversion :: Conversion -> BaseUnit
sourceOfConversion (Conversion _ s _) = s
destOfConversion (Conversion _ _ d) = d

getTransform :: Conversion -> Transformation
getTransform (Conversion t _ _) = t

unitFactor :: Conversion -> DegreeMap BaseUnit
unitFactor (Conversion _ source dest) = toMap dest `divide` toMap source

data Transformation
  = Inversion
  | LinearTransform String Quantity
  | AffineTransForm String Quantity Quantity -- m b
  | InverseOf Transformation
  deriving Show

invertConversion :: Conversion -> Conversion
invertConversion (Conversion (InverseOf t) source dest) = Conversion t dest source
invertConversion (Conversion t source dest) = Conversion (InverseOf t) dest source

resultingUnits :: DegreeMap BaseUnit -> Conversion -> DegreeMap BaseUnit
resultingUnits u (Conversion _ source dest) =
  u `multiply` (toMap dest) `divide` (toMap source)

findConversions ::
     [Conversion]
  -> DegreeMap BaseUnit
  -> DegreeMap BaseUnit
  -> [[Transformation]]
findConversions cdb goal current =
  findConversionDijk 1000 cdb [current] goal [([], current)]

-- findConversions cdb goal current = findConversions' [current] cdb goal current
-- findConversions cdb goal current
--   = map ((map getTransform).fst)
--   $ searchConversionGraph' 10 cdb [] goal [] current

-- findConversions' ::
--      [DegreeMap BaseUnit]
--   -> [Conversion]
--   -> DegreeMap BaseUnit
--   -> DegreeMap BaseUnit
--   -> [[Transformation]]
-- findConversions' visited cdb goal current
--   | goal == current = return [] -- identity
--   | goal == invert current = return [Inversion]
--   | otherwise = do
--       let candidates = directlyApplicable current cdb
--       c@(Conversion transform _ _) <- candidates
--       let current' = resultingUnits current c
--       let visited' = current':visited
--       guard $ not $ current' `elem` visited
--       fmap (transform:) $ findConversions' visited' cdb goal current'

directlyApplicable :: DegreeMap BaseUnit -> [Conversion] -> [Conversion]
directlyApplicable source = Prelude.concatMap applies
  where
    applies c =
          (if source `canReduce` c then [c] else [])
      ++  (if source `canReduce` inverseC then [inverseC] else [])
      where
        inverseC = invertConversion c

type ConversionSearchFrontier = [([Transformation], DegreeMap BaseUnit)]

nextFrontier ::
     [Conversion]             -- Conversion DB
  -> [DegreeMap BaseUnit]     -- Visited
  -> ConversionSearchFrontier -- Current frontier,
                              --    assume sorted by transforms length ascending.
  -> ConversionSearchFrontier -- Next frontier
nextFrontier cdb visited [] = []
nextFrontier cdb visited ((ts, us):fs) = next
  where
    nextCurrent = map insertPrefix $ expandConversionGraph1 cdb us

    next :: ConversionSearchFrontier
    next = foldr (\f fs' -> insertSorted f fs') [] nextCurrent

    insertPrefix ::
         (Conversion, DegreeMap BaseUnit)
      -> ([Transformation], DegreeMap BaseUnit)
    insertPrefix (c, u) = (getTransform c:ts, u)

insertSorted ::
     ([Transformation], DegreeMap BaseUnit)
  -> ConversionSearchFrontier
  -> ConversionSearchFrontier
insertSorted x [] = [x]
insertSorted (ts, us) (f@(fts, _):fs) = if length fts <= length ts
  then (ts, us):f:fs
  else f:(insertSorted (ts, us) fs)

expandConversionGraph1 ::
     [Conversion]
  -> DegreeMap BaseUnit
  -> [(Conversion, DegreeMap BaseUnit)]
expandConversionGraph1 cdb start =
    flip map (directlyApplicable start cdb) $ \conv ->
      (conv, resultingUnits start conv)

findConversionDijk ::
     Int                      -- Depth
  -> [Conversion]             -- Conversion DB
  -> [DegreeMap BaseUnit]     -- Visited
  -> DegreeMap BaseUnit       -- Goal
  -> ConversionSearchFrontier -- Frontier
  -> [[Transformation]]       -- Conversions
findConversionDijk 0 _ _ _ _ = []
findConversionDijk _ _ _ _ [] = []
findConversionDijk depth cdb visited goal frontier@((_, c):_) =
  case maybeFound of
    Just ts -> [ts]
    Nothing -> next
    where
      maybeFound :: Maybe [Transformation]
      maybeFound = fmap fst $ find ((==goal).snd) frontier

      visited' :: [DegreeMap BaseUnit]
      visited' = c:visited

      frontier' :: ConversionSearchFrontier
      frontier' = nextFrontier cdb visited' frontier

      next :: [[Transformation]]
      next = findConversionDijk (pred depth) cdb visited' goal frontier'











canReduce, canNumReduce, canDenReduce :: DegreeMap BaseUnit -> Conversion -> Bool
canReduce u c = canNumReduce u c || canDenReduce u c
canNumReduce u = hasNumerator u . sourceOfConversion
canDenReduce u = hasDenominator u . destOfConversion


bestSequence :: [[Transformation]] -> Maybe [Transformation]
bestSequence [] = Nothing
bestSequence ts
  = Just
  $ head
  $ sortBy (\a b -> length a `compare` length b)
  $ reverse
  $ ts

executeTransform :: Transformation -> Quantity -> Quantity
executeTransform Inversion x = 1 / x
executeTransform (LinearTransform _ f) x = f * x
executeTransform (AffineTransForm _ m b) x = m * x + b
executeTransform (InverseOf Inversion) x = x
executeTransform (InverseOf (LinearTransform _ f)) x = x / f
executeTransform (InverseOf (AffineTransForm _ m b)) x = (x - b) / m

executeConversion :: [Transformation] -> Quantity -> Quantity
executeConversion = flip $ foldr executeTransform

-- Values

data Scalar = Scalar Quantity (DegreeMap BaseUnit)

instance Show Scalar where
  show (Scalar magnitude units) = show magnitude ++ " " ++ show units

-- Runtime

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

da = flip directlyApplicable convertDb
step :: [DegreeMap BaseUnit] -> [DegreeMap BaseUnit]
step = Prelude.concatMap (\u -> map (resultingUnits u) $ da u)
