module Doggerel.DegreeMap (
    DegreeMap,
    allKeys,
    divide,
    emptyMap,
    expDM,
    fromMap,
    getMap,
    fmapDM,
    intExpDM,
    isEmpty,
    getFractionPair,
    hasDenominator,
    hasNumerator,
    invert,
    multiply,
    normalizeInverse,
    toMap
  ) where

import Data.List (intersperse, sort, sortBy)
import Data.Map.Strict as Map (
    Map,
    assocs,
    filter,
    fromList,
    keys,
    lookup,
    mapKeys,
    null,
    singleton,
    toList,
    unionWith
  )

-- DegreeMap is a generic representation of a compound fraction with components
-- of type a. For example, if a is Int, the fraction 3/4 would map those values
-- to their exponent (AKA their degree): {3 -> 1, 4 -> -1}. Likewise, the using
-- String for a, we can express an acceleration dimension meters/second^2 as:
-- {"meters" -> 1, "second" -> -2}.
--
-- Any value of a that has no mapping in a DegreeMap a is implicitly of degree
-- zero, so any values that actually map to zero should be removed from the
-- internal data structure.
data DegreeMap a = DegreeMap (Map a Int)

instance Eq a => Eq (DegreeMap a) where
  (DegreeMap m1) == (DegreeMap m2) = m1 == m2

-- DegreeMaps are ordered under the following scheme:
-- 1) The map with the greatest degree value is greater than the other.
-- 2) If the greatest degree of both maps are the same, then the comparison
--    is that of the values under that degree.
-- 3) An empty map is less than any non-empty map.
instance Ord a => Ord (DegreeMap a) where
  (DegreeMap a) `compare` (DegreeMap b)
    = comapreDegreeLists (descDegreeList a) (descDegreeList b)
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

-- Degree maps can be represented in a string as a product of its domain values
-- raised to the powers of their respective degrees.
--
-- For example: "a³·b²·c·d⁻¹·e⁻²"
instance (Ord a, Show a) => Show (DegreeMap a) where
  show (DegreeMap m)
    | Map.null m = "!dimensionless"
    | otherwise
        = concat
        $ intersperse "·"
        $ flip map (descDegreeList m)
        $ \(a, o) -> show a ++ if o == 1 then "" else intToSuperscript o

-- Here's a degree map version of fmap. We do not instance functor here because
-- we need the ord constraint on the key.
fmapDM :: Ord b => (a -> b) -> DegreeMap a -> DegreeMap b
fmapDM f (DegreeMap m) = DegreeMap $ mapKeys f m

-- Get the given DegreeMap as a list of pairs that are sorted by the degree in
-- descending order.
descDegreeList :: Map a Int -> [(a, Int)]
descDegreeList m = sortBy comapreDegree $ toList m
  where
    comapreDegree (_, a) (_, b) = b `compare` a

-- Convert an Int value to the corresponding decimal representation in the
-- Unicode superscript characters.
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

-- Get the underlying Map data structure in a degree Map.
getMap :: DegreeMap a -> Map a Int
getMap (DegreeMap m) = m

-- Get a degree map to represent the given map.
fromMap :: Map a Int -> DegreeMap a
fromMap = DegreeMap . removeNull

-- Given a map, remove the values that map to zero.
removeNull :: Map k Int -> Map k Int
removeNull = Map.filter (/= 0)

emptyMap :: Ord a => DegreeMap a
emptyMap = DegreeMap $ fromList []

isEmpty :: DegreeMap a -> Bool
isEmpty = Map.null . getMap

-- Exponent for a degree map. Given a degree map and an exponent, find the
-- degree map with every degre emultiplied by that exponent. If any degree
-- product is non-whole, then the result is nothing.
expDM :: Ord a => DegreeMap a -> Double -> Maybe (DegreeMap a)
expDM dm e = case intProduct dm of
  Just pairs -> Just $ fromMap $ fromList pairs
  Nothing -> Nothing
  where
    intProduct :: DegreeMap a -> Maybe [(a, Int)]
    intProduct = sequence . map roundPair . doubleProduct

    roundPair :: (a, Double) -> Maybe (a, Int)
    roundPair (a, d) = if (fromInteger $ round d) == d
      then Just (a, round d)
      else Nothing

    doubleProduct :: DegreeMap a -> [(a, Double)]
    doubleProduct = map multDeg . assocs . getMap

    multDeg :: (a, Int) -> (a, Double)
    multDeg (a, d) = (a, e * fromIntegral d)

-- We can expect the exponent to alays be defined when the radix is an int.
intExpDM :: Ord a => DegreeMap a -> Int -> DegreeMap a
intExpDM dm e = case (expDM dm $ fromIntegral e) of Just dm' -> dm'

-- Compute the reciprocal of the given DegreeMap. Effectively this means
-- reversing the sign of each mapped degree.
invert :: Ord a => DegreeMap a -> DegreeMap a
invert = flip intExpDM (-1)

-- Compute the product of two DegreeMaps. Effectively, this means computing the
-- sum of the degrees of matching a values, along with the unmatched mappings.
--
-- For example {a -> 3, b -> 4, c -> -2} multiplied by {b -> 2, c -> 2, d -> 1}
-- will be {a -> 3, b -> 6, d -> 1}. Note that c is dropped from the product
-- because its degree sum was zero.
multiply :: Ord a => DegreeMap a -> DegreeMap a -> DegreeMap a
multiply (DegreeMap m1) (DegreeMap m2)
  = DegreeMap
  $ removeNull
  $ unionWith (+) m1 m2

-- Division is shorthand for multiplying the first operand by the reciprocal of
-- the second operand.
divide :: Ord a => DegreeMap a -> DegreeMap a -> DegreeMap a
divide dm1 dm2 = multiply dm1 $ invert dm2

-- Find the degree of the given a value in the given DegreeMap a if present, or
-- nothing if it is not.
lookupDegree :: Ord a => DegreeMap a -> a -> Maybe Int
lookupDegree (DegreeMap m) a = a `Map.lookup` m

-- Find whether the given a value is in the numerator or the denominator
-- respectively of the fraction represented by the given DegreeMap a.
hasNumerator, hasDenominator :: Ord a => DegreeMap a -> a -> Bool
hasNumerator m = any (> 0) . lookupDegree m
hasDenominator m = any (< 0) . lookupDegree m

getFractionPair :: Ord a => DegreeMap a -> (DegreeMap a, DegreeMap a)
getFractionPair (DegreeMap m) = (DegreeMap num, DegreeMap den)
  where
    num = fromList
      $ Prelude.filter ((> 0) . snd)
      $ assocs m
    den = fromList
      $ map (\(a, d) -> (a, 0 - d))
      $ Prelude.filter ((< 0) . snd)
      $ assocs m

-- Get a DegreeMap mapping the given value to the degree of one.
toMap :: a -> DegreeMap a
toMap = DegreeMap . flip singleton 1

-- Normalize the degree map through inversion. If two degree maps are inverses
-- of each other, then, if they are both normalized by this function, they will
-- become identical by ensuring that the lexically-first key is in the
-- numerator.
normalizeInverse :: Ord a => DegreeMap a -> DegreeMap a
normalizeInverse dm@(DegreeMap m) =
  case dm `lookupDegree` (head $ sort $ keys m) of
    Just deg -> if deg > 0 then dm else invert dm

-- Do all the keys of the given degree map satisfy the given predicate?
allKeys :: (a -> Bool) -> DegreeMap a -> Bool
allKeys f (DegreeMap m) = all f $ keys m
