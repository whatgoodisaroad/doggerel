module Doggerel.Scope (
    Pragma(..),
    ScopeFrame,
    initFrame,
    garbageCollect,
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
    hasPragma,
    isLocalIdentifier,
    pushScope,
    popScope,
    replaceAssignment,
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

import Data.Map.Strict as Map (Map, adjust, empty, insert, lookup, restrictKeys)
import Data.Maybe (fromJust, isJust)
import Data.List (find, nub)
import Data.Set as Set (Set, empty, insert, member, union)
import Doggerel.Ast ( Units, Identifier, ValueExpression )
import Doggerel.Core (Dimensionality, Quantity, Scalar, Units, Vector)
import Doggerel.Conversion (Transformation)

type UnitDef = (Identifier, Maybe Dimensionality)
type Assignment = (Identifier, Vector)
type Input = (Identifier, Either Dimensionality Scalar)
type Rel = (Identifier, Map (Set Units) (Units, ValueExpression Units Quantity))

data Pragma = AsciiOutput
  deriving (Eq, Ord, Show)

newtype ClosureId = AnonClosure Int
  deriving (Eq, Ord, Show)

-- A scope frame is an identifier of the innermost closure, an index for the
-- next unused closure ID and a map of defined closures.
data ScopeFrame = ScopeFrame ClosureId Int (Map ClosureId Closure)
  deriving (Eq, Show)

data Closure = Closure
  [Identifier]                      -- Dimensions
  [UnitDef]                         -- Units
  [(Units, Units, Transformation)]  -- Conversions
  [Assignment]                      -- Assignments
  [Input]                           -- Inputs
  [Rel]                             -- Relations
  (Set Pragma)                      -- Pragmas
  (Maybe ClosureId)                 -- Parent closure
  deriving (Eq, Show)

-- Get innermost closure of the given scope.
getLocalClosure :: ScopeFrame -> Closure
getLocalClosure (ScopeFrame id _ m) = fromJust $ id `Map.lookup` m

-- Get the ID of the closure wrapping the innermost, if any.
getParent :: Closure -> Maybe ClosureId
getParent (Closure _ _ _ _ _ _ _ mp) = mp

-- Given an inner closure and an outer closure, produce a synthetic closure
-- that combines the two, but where any identifier in the outer will be omitted
-- if it corresponds to an idenrifier of any type from the inner closure.
--
-- For example, if foo is defined as a unit in the inner closure, but foo is
-- defined as an assignment in the outer closure, the unit will appear in the
-- synthetic closure and the assignment will not.
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

-- Collapse what is defined and addressible in the given scope frame as a single
-- synthetic closure.
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
localIdentifiers s = closureLocalIdentifiers $ getLocalClosure s

-- Get the set of identiifers that are defined in the given closure, this
-- disregards parent closures.
closureLocalIdentifiers :: Closure -> [Identifier]
closureLocalIdentifiers (Closure ds us _ as is rs ps _)
    =   nub
    $   ds
    ++  Prelude.map fst us
    ++  Prelude.map getAssignmentId as
    ++  Prelude.map getInputId is
    ++  Prelude.map getRelationId rs

isLocalIdentifier :: Identifier -> ScopeFrame -> Bool
isLocalIdentifier id = elem id . localIdentifiers

-- Traverse closures looking for a closure with some definition and give its ID
-- if it's found. If a closure is found that masks the ID for the search, then
-- give nothing.
getClosureOfDefinition ::
     ScopeFrame
  -> Identifier
  -> (Closure -> Bool)
  -> (Closure -> Bool)
  -> Maybe ClosureId
getClosureOfDefinition (ScopeFrame ci ni m) id isHere isMaskedHere =
  case ci `Map.lookup` m of
    Nothing -> Nothing
    Just c -> if isHere c
      -- It's defined in the local closure, so give this ID.
      then Just ci
      -- If the ID is otherwsise defined in the local closure, then it will mask
      -- any definition in a parent closure, so give nothing.
      else if isMaskedHere c
      then Nothing
      -- Otherwise we need to search parent closures.
      else case getParent c of
        -- Give nothing if there is no parent closure to search.
        Nothing -> Nothing
        -- Otherwise, recurse by creaing a new frame with the parent as the
        -- current ID.
        Just pi ->
          getClosureOfDefinition (ScopeFrame pi ni m) id isHere isMaskedHere

emptyClosure :: Maybe ClosureId -> Closure
emptyClosure = Closure [] [] [] [] [] [] Set.empty

-- A frame with noting defined.
emptyFrame :: ScopeFrame
emptyFrame
  = ScopeFrame (AnonClosure 0) 1
  $ Map.insert (AnonClosure 0) (emptyClosure Nothing) Map.empty

-- An initial frame with built-in language symbols defined.
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
getAssignments s = case getEffectiveScope s of (Closure _ _ _ as _ _ _ _) -> as

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

-- Given a scope frame, and an identifier, give the identifier for the closure
-- where an assignment with that ID is defined, or nothing if no such assignment
-- is defined.
closureOfAssignment :: ScopeFrame -> Identifier -> Maybe ClosureId
closureOfAssignment f id = getClosureOfDefinition f id isHere isMaskedHere
  where
    isHere (Closure _ _ _ as _ _ _ _) = any ((==id).fst) as
    isMaskedHere c = id `elem` closureLocalIdentifiers c

-- Rewrite an assignment as its defined in the given scope. If no such
-- assignment is defined and reachable, the scope is unchanged.
replaceAssignment :: ScopeFrame -> Assignment -> ScopeFrame
replaceAssignment f@(ScopeFrame ci ni m) a@(id, _) =
  case closureOfAssignment f id of
    Just aci -> ScopeFrame ci ni $ adjust replaceInClosure aci m
    Nothing -> f
  where
    replaceInClosure :: Closure -> Closure
    replaceInClosure (Closure ds us cs as is rs ps mp) =
      Closure ds us cs as' is rs ps mp
      where
        as' = a : Prelude.filter ((/=id).fst) as

withInput :: ScopeFrame -> Input -> ScopeFrame
withInput s@(ScopeFrame id ni m) i = ScopeFrame id ni $ Map.insert id local' m
  where
    (Closure ds us cs as is rs ps mp) = getLocalClosure s
    local' = Closure ds us cs as (i:is) rs ps mp

closureOfInput :: ScopeFrame -> Identifier -> Maybe ClosureId
closureOfInput f id = getClosureOfDefinition f id isHere isMaskedHere
  where
    isHere (Closure _ _ _ _ is _ _ _) = any ((==id).fst) is
    isMaskedHere c = id `elem` closureLocalIdentifiers c

replaceInput :: ScopeFrame -> Input -> ScopeFrame
replaceInput f@(ScopeFrame ci ni m) i@(id, _) =
  case closureOfInput f id of
    Just ici -> ScopeFrame ci ni $ adjust replaceInClosure ici m
    Nothing -> f
  where
    replaceInClosure :: Closure -> Closure
    replaceInClosure (Closure ds us cs as is rs ps mp) =
      Closure ds us cs as is' rs ps mp
      where
        is' = i : Prelude.filter ((/=id).fst) is

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

-- Given a scope frame, create an empty closure nested inside the current one.
pushScope :: ScopeFrame -> ScopeFrame
pushScope (ScopeFrame oldPointer ni m)
  = ScopeFrame newPointer newNext
  $ Map.insert newPointer (emptyClosure $ Just oldPointer) m
  where
    newPointer = AnonClosure ni
    newNext = succ ni

-- Given a scope frame, pop outwards to the enclosing closure (or give nothing
-- if we're at the top-level).
popScope :: ScopeFrame -> Maybe ScopeFrame
popScope s@(ScopeFrame id ni m) = do
  parentId <- getParent $ getLocalClosure s
  return $ ScopeFrame parentId ni m

-- Garbage collect unreferenced closures.
garbageCollect :: ScopeFrame -> ScopeFrame
garbageCollect f@(ScopeFrame ci ni m) =
  ScopeFrame ci ni $ restrictKeys m $ referencedClosures f

-- The list of closure identifiers that are reachable from the current closure
-- (i.e. they cannot be garbage collected).
referencedClosures :: ScopeFrame -> Set ClosureId
referencedClosures f@(ScopeFrame ci ni m) = Set.insert ci rc
  where
    rc = case getParent $ getLocalClosure f of
      Just pi -> referencedClosures $ ScopeFrame pi ni m
      Nothing -> Set.empty
