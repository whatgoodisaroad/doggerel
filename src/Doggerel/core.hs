module Doggerel.Core (
    BaseUnit(BaseUnit),
    Quantity,
    Scalar(Scalar),
    Units,
    Vector(Vector),
    getScalarUnits,
    scalarToVector
  ) where

import Data.List (find, intersperse)
import Data.Map.Strict as Map
import Doggerel.DegreeMap

-- BaseUnit represents a base unit value identified by a string.
data BaseUnit = BaseUnit String deriving (Eq, Ord)

instance Show BaseUnit where show (BaseUnit s) = s

-- The Units type alias represents a compound units expression.
type Units = DegreeMap BaseUnit

-- Type alias for the underlying dimensionless floating point representation.
type Quantity = Double

data Scalar = Scalar Quantity Units

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
