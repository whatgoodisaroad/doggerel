module Doggerel.Core (
    BaseUnit(BaseUnit),
    Dimension(..),
    Dimensionality,
    Quantity,
    Scalar(Scalar),
    Units,
    Vector(Vector),
    VectorDimensionality(..),
    getComponent,
    getScalarUnits,
    scalarToVector,
    orElse
  ) where

import Data.List (find, intersperse)
import Data.Map.Strict as Map
import Data.Set as Set (Set, toList)
import Doggerel.DegreeMap

-- BaseUnit represents a base unit value identified by a string.
data BaseUnit = BaseUnit String deriving (Eq, Ord)

instance Show BaseUnit where show (BaseUnit s) = s

-- The Units type alias represents a compound units expression.
type Units = DegreeMap BaseUnit

data Dimension = Dimension String
  deriving (Eq, Ord)

instance Show Dimension where
  show (Dimension s) = s

type Dimensionality = DegreeMap Dimension

data VectorDimensionality = VecDims (Set Dimensionality)
  deriving (Eq, Ord)

instance Show VectorDimensionality where
  show (VecDims dimSet) = "{ " ++ comps ++ " }"
    where
      comps = concat $ intersperse ", " $ Prelude.map show $ Set.toList dimSet

-- Type alias for the underlying dimensionless floating point representation.
type Quantity = Double

data Scalar = Scalar Quantity Units
  deriving Eq

getScalarUnits :: Scalar -> Units
getScalarUnits (Scalar _ u) = u

instance Show Scalar where
  show (Scalar magnitude units) = show magnitude ++ " " ++ show units

data Vector = Vector (Map Units Quantity)

instance Show Vector where
  show (Vector m) = if Map.null m then "Ø" else "{" ++ vals ++ "}"
    where
      vals
        = concat
        $ intersperse ", "
        $ Prelude.map (\(u, q) -> (show $ Scalar q u))
        $ assocs m

instance Eq Vector where
  (Vector v1) == (Vector v2) = v1 == v2

scalarToVector :: Scalar -> Vector
scalarToVector (Scalar q u) = Vector $ insert u q empty

-- Helper to coalesce a maybe to an alternative value if it is not present.
orElse :: Maybe a -> a -> a
orElse (Just a) _ = a
orElse _ a = a

getComponent :: Vector -> Units -> Quantity
getComponent (Vector m) u = case Map.lookup u m of
  Nothing -> 0
  Just q -> q
