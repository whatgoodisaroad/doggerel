module Doggerel.Core (
    BaseUnit(BaseUnit),
    Dimension(..),
    Dimensionality,
    Quantity,
    Scalar(Scalar),
    Units,
    Vector(Vector),
    VectorDimensionality(..),
    dimsToVecDims,
    getComponent,
    getScalarUnits,
    scalarToVector,
    orElse,
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
import Doggerel.DegreeMap

-- BaseUnit represents a base unit value identified by a string.
newtype BaseUnit = BaseUnit String deriving (Eq, Ord)

instance Show BaseUnit where show (BaseUnit s) = s

-- The Units type alias represents a compound units expression.
type Units = DegreeMap BaseUnit

newtype Dimension = Dimension String deriving (Eq, Ord)

instance Show Dimension where
  show (Dimension s) = s

type Dimensionality = DegreeMap Dimension

newtype VectorDimensionality = VecDims (Set Dimensionality) deriving (Eq, Ord)

instance Show VectorDimensionality where
  show (VecDims dimSet) = "{ " ++ comps ++ " }"
    where
      comps = intercalate ", " (Prelude.map show $ Set.toList dimSet)

-- Type alias for the underlying dimensionless floating point representation.
type Quantity = Double

data Scalar = Scalar Quantity Units
  deriving Eq

getScalarUnits :: Scalar -> Units
getScalarUnits (Scalar _ u) = u

instance Show Scalar where
  show (Scalar magnitude units) = show magnitude ++ " " ++ show units

newtype Vector = Vector (Map Units Quantity)

instance Show Vector where
  show (Vector m) = if Map.null m then "0" else "{" ++ vals ++ "}"
    where
      vals
        = intercalate ", "
        $ Prelude.map (\(u, q) -> show $ Scalar q u) (assocs m)

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
