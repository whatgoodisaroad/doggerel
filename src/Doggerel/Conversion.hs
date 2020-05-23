module Doggerel.Conversion (
    Conversion(Conversion),
    Transformation(
      Inversion,
      LinearTransform,
      AffineTransForm,
      InverseOf
    ),
    findConversions
  ) where

import Data.List (find)
import Doggerel.DegreeMap
import Doggerel.Core

-- A conversion represents an edge in the conversion graph whereby the source
-- BaseUnit can be converted to the destination BaseUnit via the given
-- transformation.
data Conversion
  = Conversion Transformation BaseUnit BaseUnit -- transform source dest
  deriving Show

sourceOfConversion, destOfConversion :: Conversion -> BaseUnit
sourceOfConversion (Conversion _ s _) = s
destOfConversion (Conversion _ _ d) = d

getTransform :: Conversion -> Transformation
getTransform (Conversion t _ _) = t

-- A transformation represents an operation on a scalar quantity or the inverse
-- operation of another transformation.
data Transformation
  = Inversion
  | LinearTransform Quantity
  | AffineTransForm Quantity Quantity -- m b
  | InverseOf Transformation
  deriving (Show, Eq)

-- Given a conversion value, produce the inverse, whereby the transformation is
-- inverted, and the source and destination units are swapped.
invertConversion :: Conversion -> Conversion
invertConversion (Conversion (InverseOf t) source dest)
  = Conversion t dest source
invertConversion (Conversion t source dest)
  = Conversion (InverseOf t) dest source

-- Find the resulting units if the given conversion were applied to the givens
-- units expression.
resultingUnits :: Units -> Conversion -> Units
resultingUnits u (Conversion _ source dest) =
  u `multiply` (toMap dest) `divide` (toMap source)

-- Given a conversion database, starting units and a target final units, search
-- for a set of transformations to apply to a scalar of the starting units in
-- order to achieve a scalar of the destination units.
--
-- This sets up a Dijkstra search across the conversion graph with limited
-- depth, and may fail if no conversion were found, or the depth limit reached.
findConversions :: [Conversion] -> Units -> Units -> Maybe [Transformation]
findConversions cdb goal current =
  findConversionDijk 1000 cdb goal ([current], [([], current)])

-- Given a conversion database, filter the database to only those which are
-- in any way applicable to the given Units.
directlyApplicable :: Units -> [Conversion] -> [Conversion]
directlyApplicable source = Prelude.concatMap applies
  where
    applies c =
          (if source `canReduce` c then [c] else [])
      ++  (if source `canReduce` inverseC then [inverseC] else [])
      where
        inverseC = invertConversion c

-- Gives whether the given conversion, when applied to the given units
-- expression cancels any components of the fraction.
canReduce :: Units -> Conversion -> Bool
canReduce u c = num || den
  where
    num = hasNumerator u $ sourceOfConversion c
    den = hasDenominator u $ destOfConversion c

-- A frontier represents an intermediate Dijkstra-search frontier across a
-- conversion graph. It's a list of pairs linking a units expression with the
-- sequence of transformations (in reverse order) by which it can be reached
-- from the original units;
type ConversionSearchFrontier = [([Transformation], Units)]

-- Insert a new node into the given search frontier in order of transformation
-- list length. If the detination units are already represented in the frontier,
-- the node with the shorter transformation list is used and the other is
-- dropepd.
--
-- In effect, the frontier functions as a priority-queue whereby nodes with
-- shorter transformation lists have priority.
insertSorted ::
     ([Transformation], Units)
  -> ConversionSearchFrontier
  -> ConversionSearchFrontier
insertSorted x [] = [x]
insertSorted (ts, us) (f@(fts, _):fs) = if length fts <= length ts
  then (ts, us):f:fs
  else f:(insertSorted (ts, us) fs)

-- The intermediate state of a Dijkstra search.
type FindConversionState = (
    [Units],                  -- Visited
    ConversionSearchFrontier  -- Frontier
  )

-- Get an initial FindConversionState where the given starting units are the
-- only visited node.
initialSearchState :: Units -> FindConversionState
initialSearchState u = ([], [([], u)])

-- Expand the given state by one step by selecting the next frontier node (the
-- one with the shortest transformation list) finding the set of conversions
-- that apply to its units, find the resulting units from eaxch of those
-- conversions and inserting new frontier nodes for each of those which were not
-- already visited.
--
-- The selected frontier units is added to the visited list.
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

-- Search the conversion graph for a set of transformations to achieve the given -- goal units based on the given frontier. This executes a depth-limited
-- Dijkstra search using the given conversion database as a connected graph.
findConversionDijk ::
     Int                      -- Depth
  -> [Conversion]             -- Conversion DB
  -> Units                    -- Goal
  -> FindConversionState      -- State
  -> Maybe [Transformation]       -- List of resulting transformation lists
findConversionDijk 0 _ _ _ = Nothing
findConversionDijk _ _ _ (_ ,[]) = Nothing
findConversionDijk depth cdb goal (visited, frontier@((_, c):_)) =
  case maybeFound of
    Nothing -> next
    Just ts -> Just $ reverse ts
    where
      maybeFound :: Maybe [Transformation]
      maybeFound = fmap fst $ find ((==goal).snd) frontier

      (visited', frontier') = nextConversionState cdb (visited, frontier)

      next :: Maybe [Transformation]
      next = findConversionDijk (pred depth) cdb goal (visited', frontier')

