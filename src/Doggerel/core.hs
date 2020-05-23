module Doggerel.Core (
    BaseUnit(BaseUnit),
    Quantity,
    Units
  ) where

import Data.List (find)
import Doggerel.DegreeMap

-- BaseUnit represents a base unit value identified by a string.
data BaseUnit = BaseUnit String deriving (Eq, Ord)

instance Show BaseUnit where show (BaseUnit s) = s

-- The Units type alias represents a compound units expression.
type Units = DegreeMap BaseUnit

-- Type alias for the underlying dimensionless floating point representation.
type Quantity = Double

