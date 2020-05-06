module Core (
    BaseUnit(BaseUnit),
    Conversion(Conversion),
    DegreeMap,
    Quantity,
    Transformation(
      Inversion,
      LinearTransform,
      AffineTransForm,
      InverseOf
    ),
    Units,
    divide,
    findConversions,
    fromMap,
    getMap,
    invert,
    multiply,
    toMap
  ) where

import Control.Monad (guard)
import Data.List (find, intersperse, nub, sort, sortBy)
import Data.Map.Strict as Map (
    Map,
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
  (DegreeMap a) `compare` (DegreeMap b)
    = comapreDegreeLists (ascDegreeList a) (ascDegreeList b)
      where
        comapreDegreeLists [] [] = EQ
        comapreDegreeLists _ [] = GT
        comapreDegreeLists [] _ = LT
        comapreDegreeLists ((av, ad):as) ((bv, bd):bs) = case ad `compare` bd of
          GT -> GT
          LT -> LT
          EQ -> case av `compare` bv of
            GT -> GT
            LT -> LT
            EQ -> comapreDegreeLists as bs

instance (Ord a, Show a) => Show (DegreeMap a) where
  show (DegreeMap m)
    | Map.null m = "{dimensionless}"
    | otherwise
        = concat
        $ intersperse "·"
        $ flip map (ascDegreeList m)
        $ \(a, o) -> show a ++ if o == 1 then "" else intToSuperscript o

ascDegreeList :: Map a Int -> [(a, Int)]
ascDegreeList m = sortBy comapreDegree $ toList m
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

fromMap :: Map a Int -> DegreeMap a
fromMap = DegreeMap

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

type Units = DegreeMap BaseUnit

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
  findConversionDijk 1000 cdb goal ([current], [([], current)])

directlyApplicable :: DegreeMap BaseUnit -> [Conversion] -> [Conversion]
directlyApplicable source = Prelude.concatMap applies
  where
    applies c =
          (if source `canReduce` c then [c] else [])
      ++  (if source `canReduce` inverseC then [inverseC] else [])
      where
        inverseC = invertConversion c

type ConversionSearchFrontier = [([Transformation], DegreeMap BaseUnit)]

insertSorted ::
     ([Transformation], DegreeMap BaseUnit)
  -> ConversionSearchFrontier
  -> ConversionSearchFrontier
insertSorted x [] = [x]
insertSorted (ts, us) (f@(fts, _):fs) = if length fts <= length ts
  then (ts, us):f:fs
  else f:(insertSorted (ts, us) fs)

type FindConversionState = (
    [DegreeMap BaseUnit],     -- Visited
    ConversionSearchFrontier  -- Frontier
  )

initialSearchState :: DegreeMap BaseUnit -> FindConversionState
initialSearchState u = ([], [([], u)])

nextConversionState ::
     [Conversion]
  -> FindConversionState
  -> FindConversionState
nextConversionState cdb (visited, ((ts, us):frontier')) = (us:visited, nextF)
  where
    applicable :: [Conversion]
    applicable = directlyApplicable us cdb

    converted :: ConversionSearchFrontier
    converted = flip map applicable
              $ \c -> ((getTransform c):ts, resultingUnits us c)

    unvisited :: ConversionSearchFrontier
    unvisited = Prelude.filter (not . (flip elem visited) . snd) converted

    nextF :: ConversionSearchFrontier
    nextF = foldr (\f fs -> insertSorted f fs) frontier' unvisited

findConversionDijk ::
     Int                      -- Depth
  -> [Conversion]             -- Conversion DB
  -> DegreeMap BaseUnit       -- Goal
  -> FindConversionState
  -> [[Transformation]]       -- Conversions
findConversionDijk 0 _ _ _ = []
findConversionDijk _ _ _ (_ ,[]) = []
findConversionDijk depth cdb goal (visited, frontier@((_, c):_)) =
  case maybeFound of
    Just ts -> [reverse ts]
    Nothing -> next
    where
      maybeFound :: Maybe [Transformation]
      maybeFound = fmap fst $ find ((==goal).snd) frontier

      (visited', frontier') = nextConversionState cdb (visited, frontier)

      next :: [[Transformation]]
      next = findConversionDijk (pred depth) cdb goal (visited', frontier')

canReduce, canNumReduce, canDenReduce :: DegreeMap BaseUnit -> Conversion -> Bool
canReduce u c = canNumReduce u c || canDenReduce u c
canNumReduce u = hasNumerator u . sourceOfConversion
canDenReduce u = hasDenominator u . destOfConversion

