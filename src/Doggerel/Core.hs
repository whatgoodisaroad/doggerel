module Doggerel.Core (
    BaseUnit(BaseUnit),
    Dimension(..),
    Dimensionality,
    Quantity,
    Scalar(Scalar),
    Units,
    Vector(Vector),
    VectorDimensionality(..),
    booleanDims,
    dimsToVecDims,
    getComponent,
    getScalarUnits,
    logicalFalse,
    logicalTrue,
    mkBaseUnit,
    mkDimension,
    scalarToVector,
    orElse,
    unitMagnitude,
    vecDimsCartesianProduct,
    vecDimsInvert,
    vecDimsUnion
  ) where

import Data.List (find, intersperse, intercalate)
import Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set as Set (
    Set,
    cartesianProduct,
    fromList,
    singleton,
    toList,
    union
  )
import Doggerel.Charset
import Doggerel.DegreeMap

-- BaseUnit represents a base unit value identified by a string.
data BaseUnit = BaseUnit String (Maybe Int) deriving (Eq, Ord)

instance Show BaseUnit
  where
    show (BaseUnit s Nothing) = s
    show (BaseUnit s (Just i)) = s ++ "(" ++ show i ++ ")"
instance ShowForCharset BaseUnit where showForCharset _ = show

mkBaseUnit :: String -> BaseUnit
mkBaseUnit = flip BaseUnit Nothing

-- The Units type alias represents a compound units expression.
type Units = DegreeMap BaseUnit

data Dimension = Dimension String (Maybe Int) deriving (Eq, Ord)

instance Show Dimension
  where
    show (Dimension s Nothing) = s
    show (Dimension s (Just i)) = s ++ "(" ++ show i ++ ")"
instance ShowForCharset Dimension where showForCharset _ = show

mkDimension :: String -> Dimension
mkDimension = flip Dimension Nothing

type Dimensionality = DegreeMap Dimension

newtype VectorDimensionality = VecDims (Set Dimensionality) deriving (Eq, Ord)

instance ShowForCharset VectorDimensionality where
  showForCharset charset (VecDims dimSet) = "{ " ++ comps ++ " }"
    where
      comps
        = intercalate ", "
        $ Prelude.map (showForCharset charset)
        $ Set.toList dimSet
instance Show VectorDimensionality where show = showForCharset UnicodeCharset

-- Type alias for the underlying dimensionless floating point representation.
type Quantity = Double

data Scalar = Scalar Quantity Units deriving Eq

getScalarUnits :: Scalar -> Units
getScalarUnits (Scalar _ u) = u

instance ShowForCharset Scalar where
  showForCharset charset (Scalar magnitude units)
    = show magnitude ++ " " ++ showForCharset charset units
instance Show Scalar where show = showForCharset UnicodeCharset

newtype Vector = Vector (Map Units Quantity)

instance ShowForCharset Vector where
  showForCharset charset (Vector m)
    = if Map.null m then [emptySymbol] else "{" ++ vals ++ "}"
    where
      vals
        = intercalate ", "
        $ Prelude.map
          (\(u, q) -> showForCharset charset $ Scalar q u)
          (assocs m)
      emptySymbol = "Ø0" !! (if charset == UnicodeCharset then 0 else 1)
instance Show Vector where show = showForCharset UnicodeCharset

instance Eq Vector where
  (Vector v1) == (Vector v2) = v1 == v2

scalarToVector :: Scalar -> Vector
scalarToVector (Scalar q u) = Vector $ insert u q empty

-- Helper to coalesce a maybe to an alternative value if it is not present.
orElse :: Maybe a -> a -> a
orElse (Just a) _ = a
orElse _ a = a

getComponent :: Vector -> Units -> Quantity
getComponent (Vector m) u = fromMaybe 0 $ Map.lookup u m

-- Encode scalar dimensionality as a vector dimensionality.
dimsToVecDims :: Dimensionality -> VectorDimensionality
dimsToVecDims = VecDims . Set.singleton

-- Union of vector dimensionalities. Doesn't take inverses into account.
vecDimsUnion ::
     VectorDimensionality
  -> VectorDimensionality
  -> VectorDimensionality
(VecDims s1) `vecDimsUnion` (VecDims s2) = VecDims $ s1 `Set.union` s2

-- The cartesian product of the given two vector dimensionalities. The product
-- pairs are combined with the degree map product.
vecDimsCartesianProduct ::
     VectorDimensionality
  -> VectorDimensionality
  -> VectorDimensionality
(VecDims s1) `vecDimsCartesianProduct` (VecDims s2)
  = VecDims
  $ Set.fromList
  $ Prelude.map (uncurry multiply)
  $ Set.toList
  $ s1 `cartesianProduct` s2

-- The inverse of the given vector dimensionality.
vecDimsInvert :: VectorDimensionality -> VectorDimensionality
vecDimsInvert (VecDims s)
  = VecDims
  $ Set.fromList
  $ Prelude.map invert
  $ Set.toList s

booleanDims :: VectorDimensionality
booleanDims = dimsToVecDims (toMap $ Dimension "bool" Nothing)

logicalFalse, logicalTrue :: Vector
logicalFalse  = scalarToVector $ Scalar 0 $ toMap $ mkBaseUnit "bool"
logicalTrue   = scalarToVector $ Scalar 1 $ toMap $ mkBaseUnit "bool"

-- Find the magnitude of the given vector in terms of its current set of units.
unitMagnitude :: Vector -> Quantity
unitMagnitude (Vector m) = sqrt $ sum $ Prelude.map (^^2) $ elems m
