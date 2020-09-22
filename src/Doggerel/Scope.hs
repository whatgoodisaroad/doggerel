module Doggerel.Scope (
    Pragma(..),
    ScopeFrame,
    initFrame,
    getAssignmentById,
    getAssignmentId,
    getAssignments,
    getConversions,
    getDimensions,
    getInputById,
    getInputId,
    getInputs,
    getRelations,
    getRelationById,
    getRelationId,
    getUnitDimensionById,
    getUnits,
    hasParentScope,
    hasPragma,
    pushScope,
    popScope,
    replaceInput,
    withAssignment,
    withConversion,
    withDimension,
    withInput,
    withPragma,
    withRelation,
    withUnit
  )
  where

import Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Data.List (find, nub)
import Data.Set as Set
import Doggerel.Ast
import Doggerel.Core
import Doggerel.Conversion
import Doggerel.DegreeMap

type UnitDef = (Identifier, Maybe Dimensionality)
type Assignment = (Identifier, Expr)
type Input = (Identifier, Either Dimensionality Scalar)
type Rel = (Identifier, Map (Set Units) (Units, ValueExpression Units Quantity))

data Pragma = AsciiOutput
  deriving (Eq, Ord, Show)

newtype ClosureId = AnonClosure Int
  deriving (Eq, Ord, Show)

data ScopeFrame = ScopeFrame ClosureId Int (Map ClosureId Closure)
  deriving (Eq, Show)

-- TODO: this should be a record.
data Closure = Closure
  [Identifier]                      -- Dimensions
  [UnitDef]                         -- Units
  [(Units, Units, Transformation)]  -- Conversions
  [Assignment]                      -- Assignments
  [Input]                           -- Inputs
  [Rel]                             -- Relations
  (Set Pragma)                      -- Pragmas
  (Maybe ClosureId)                 -- Parent scope
  deriving (Eq, Show)

getLocalClosure :: ScopeFrame -> Closure
getLocalClosure (ScopeFrame id _ m) = fromJust $ id `Map.lookup` m

getParent :: Closure -> Maybe ClosureId
getParent (Closure _ _ _ _ _ _ _ mp) = mp

mask :: Closure -> Closure -> Closure
mask c1 c2 = Closure ds us cs as is rs ps mp2
  where
    (Closure ds1 us1 cs1 as1 is1 rs1 ps1 _) = c1
    (Closure ds2 us2 cs2 as2 is2 rs2 ps2 mp2) = c2
    noConflict = flip notElem (closureLocalIdentifiers c1)
    ds = ds1 ++ Prelude.filter noConflict ds2
    us = us1 ++ Prelude.filter (noConflict.fst) us2
    cs = cs1 ++ cs2
    as = as1 ++ Prelude.filter (noConflict.getAssignmentId) as2
    is = is1 ++ Prelude.filter (noConflict.getInputId) is2
    rs = rs1 ++ Prelude.filter (noConflict.getRelationId) rs2
    ps = ps1 `Set.union` ps2

getEffectiveScope :: ScopeFrame -> Closure
getEffectiveScope s@(ScopeFrame id ni m) = case getParent local of
  Just p -> local `mask` getEffectiveScope (ScopeFrame p ni m)
  Nothing -> local
  where
    local = getLocalClosure s

-- Get the list of local identifiers in the frame. This is the set of units,
-- dimensions, assignments, inputs, etc. which are defined in the given scope,
-- but not coming from an enclosing frame.
localIdentifiers :: ScopeFrame -> [Identifier]
localIdentifiers s = closureLocalIdentifiers $ getEffectiveScope s

closureLocalIdentifiers :: Closure -> [Identifier]
closureLocalIdentifiers (Closure ds us _ as is rs ps _)
    =   nub
    $   ds
    ++  Prelude.map fst us
    ++  Prelude.map getAssignmentId as
    ++  Prelude.map getInputId is
    ++  Prelude.map getRelationId rs

isNonLocal :: ScopeFrame -> Identifier -> Bool
isNonLocal s i = notElem i $ localIdentifiers s

emptyClosure :: Maybe ClosureId -> Closure
emptyClosure mp = Closure [] [] [] [] [] [] Set.empty mp

-- A frame with noting defined.
emptyFrame :: ScopeFrame
emptyFrame
  = ScopeFrame (AnonClosure 0) 1
  $ Map.insert (AnonClosure 0) (emptyClosure Nothing) Map.empty

-- An initial frame with built-in symbols defined.
initFrame :: ScopeFrame
initFrame = emptyFrame `withUnit` ("bool", Nothing)

-- Get the list of defined dimensions (with shadowing).
getDimensions :: ScopeFrame -> [Identifier]
getDimensions s = case getEffectiveScope s of (Closure ds _ _ _ _ _ _ _) -> ds

-- Get the list of defined units (with shadowing).
getUnits :: ScopeFrame -> [UnitDef]
getUnits s = case getEffectiveScope s of (Closure _ us _ _ _ _ _ _) -> us

-- Get the dimensionality of the unit with the given ID. If the unit has no
-- dimension, or if it is not defined, the result is Nothing.
getUnitDimensionById :: ScopeFrame -> Identifier -> Maybe Dimensionality
getUnitDimensionById f id = case find ((==id).fst) $ getUnits f of
  Just (_, md) -> md
  Nothing -> Nothing

-- Get the list of defined conversions.
getConversions :: ScopeFrame -> [(Units, Units, Transformation)]
getConversions s = case getEffectiveScope s of (Closure _ _ cs _ _ _ _ _) -> cs

-- Get the list of assignments (with shadowing).
getAssignments :: ScopeFrame -> [Assignment]
getAssignments s = case getEffectiveScope s of (Closure _ _ _ as _ _ _ mp) -> as

-- Extract the ID from the assignment structure.
getAssignmentId :: Assignment -> Identifier
getAssignmentId (id, _) = id

-- Find the assignment with the given ID.
getAssignmentById :: ScopeFrame -> Identifier -> Maybe Assignment
getAssignmentById f id = find ((==id).getAssignmentId) $ getAssignments f

-- Get the list of defined inputs (with shadowing).
getInputs :: ScopeFrame -> [Input]
getInputs s = case getEffectiveScope s of (Closure _ _ _ _ is _ _ _) -> is

-- Get the ID of the given input structure.
getInputId :: Input -> Identifier
getInputId (id, _) = id

-- Find the input with the given ID.
getInputById :: ScopeFrame -> Identifier -> Maybe Input
getInputById f id = find ((==id).getInputId) $ getInputs f

-- Get the list of defined relations (with shadowing).
getRelations :: ScopeFrame -> [Rel]
getRelations s = case getEffectiveScope s of (Closure _ _ _ _ _ rs _ _) -> rs

-- Get the ID of the given relation structure.
getRelationId :: Rel -> Identifier
getRelationId (id, _) = id

-- Find the relation with the given ID.
getRelationById :: ScopeFrame -> Identifier -> Maybe Rel
getRelationById f id = find ((==id).getRelationId) $ getRelations f

-- Is the given pragma defined within scope.
hasPragma :: ScopeFrame -> Pragma -> Bool
hasPragma s p = case getEffectiveScope s of
  (Closure _ _ _ _ _ _ ps _) -> p `Set.member` ps

withDimension :: ScopeFrame -> Identifier -> ScopeFrame
withDimension s@(ScopeFrame id ni m) d
  = ScopeFrame id ni $ Map.insert id local' m
  where
    (Closure ds us cs as is rs ps mp) = getLocalClosure s
    local' = Closure (d:ds) us cs as is rs ps mp

withUnit :: ScopeFrame -> UnitDef -> ScopeFrame
withUnit s@(ScopeFrame id ni m) u = ScopeFrame id ni $ Map.insert id local' m
  where
    (Closure ds us cs as is rs ps mp) = getLocalClosure s
    local' = Closure ds (u:us) cs as is rs ps mp

withConversion :: ScopeFrame -> (Units, Units, Transformation) -> ScopeFrame
withConversion s@(ScopeFrame id ni m) c
  = ScopeFrame id ni $ Map.insert id local' m
  where
    (Closure ds us cs as is rs ps mp) = getLocalClosure s
    local' = Closure ds us (c:cs) as is rs ps mp

withAssignment :: ScopeFrame -> Assignment -> ScopeFrame
withAssignment s@(ScopeFrame id ni m) a
  = ScopeFrame id ni $ Map.insert id local' m
  where
    (Closure ds us cs as is rs ps mp) = getLocalClosure s
    local' = Closure ds us cs (a:as) is rs ps mp

withInput :: ScopeFrame -> Input -> ScopeFrame
withInput s@(ScopeFrame id ni m) i = ScopeFrame id ni $ Map.insert id local' m
  where
    (Closure ds us cs as is rs ps mp) = getLocalClosure s
    local' = Closure ds us cs as (i:is) rs ps mp

replaceInput :: ScopeFrame -> Input -> ScopeFrame
replaceInput s@(ScopeFrame id ni m) i
  = ScopeFrame id ni $ Map.insert id local' m
  where
    (Closure ds us cs as is rs ps mp) = getLocalClosure s
    local' = Closure ds us cs as (i:is') rs ps mp
    inputId = getInputId i
    is' = Prelude.filter ((/=inputId).fst) is

withRelation :: ScopeFrame -> Rel -> ScopeFrame
withRelation s@(ScopeFrame id ni m) r
  = ScopeFrame id ni $ Map.insert id local' m
  where
    (Closure ds us cs as is rs ps mp) = getLocalClosure s
    local' = Closure ds us cs as is (r:rs) ps mp

withPragma :: ScopeFrame -> Pragma -> ScopeFrame
withPragma s@(ScopeFrame id ni m) p = ScopeFrame id ni $ Map.insert id local' m
  where
    (Closure ds us cs as is rs ps mp) = getLocalClosure s
    local' = Closure ds us cs as is rs (p `Set.insert` ps) mp

pushScope :: ScopeFrame -> ScopeFrame
pushScope (ScopeFrame id ni m)
  = ScopeFrame (AnonClosure ni) (succ ni)
  $ Map.insert (AnonClosure ni) (emptyClosure $ Just id) m

popScope :: ScopeFrame -> ScopeFrame
popScope s@(ScopeFrame id ni m) = case getLocalClosure s of
  (Closure _ _ _ _ _ _ _ (Just id')) -> ScopeFrame id' ni m

hasParentScope :: ScopeFrame -> Bool
hasParentScope s = case getEffectiveScope s of
  (Closure _ _ _ _ _ _ _ mp) -> isJust mp
