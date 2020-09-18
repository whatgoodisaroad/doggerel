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
import Data.Maybe (isJust)
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

-- Represents a lexical scope for runtime.
data ScopeFrame
  = Frame
      [Identifier]                      -- Dimensions
      [UnitDef]                         -- Units
      [(Units, Units, Transformation)]  -- Conversions
      [Assignment]                      -- Assignments
      [Input]                           -- Inputs
      [Rel]                             -- Relations
      (Set Pragma)                      -- Pragmas
      (Maybe ScopeFrame)                -- Parent scope
  deriving (Eq, Show)

-- Get the list of local identifiers in the frame. This is the set of units,
-- dimensions, assignments, inputs, etc. which are defined in the given scope,
-- but not coming from an enclosing frame.
localIdentifiers :: ScopeFrame -> [Identifier]
localIdentifiers (Frame ds us _ as is rs _ _)
  = nub 
  $   ds
  ++  Prelude.map fst us
  ++  Prelude.map getAssignmentId as
  ++  Prelude.map getInputId is
  ++  Prelude.map getRelationId rs 

isNonLocal :: ScopeFrame -> Identifier -> Bool
isNonLocal s i = notElem i $ localIdentifiers s

-- Utility for getting a list out of an optional parent frame.
callThroughParent :: (ScopeFrame -> [a]) -> Maybe ScopeFrame -> [a]
callThroughParent _ Nothing = []
callThroughParent f (Just s) = f s

-- A frame with noting defined.
emptyFrame :: ScopeFrame
emptyFrame = Frame [] [] [] [] [] [] Set.empty Nothing

-- An initial frame with built-in symbols defined.
initFrame :: ScopeFrame
initFrame = emptyFrame `withUnit` ("bool", Nothing)

-- Get the list of defined dimensions (with shadowing).
getDimensions :: ScopeFrame -> [Identifier]
getDimensions s@(Frame ds _ _ _ _ _ _ mp)
  =   ds
  ++  Prelude.filter (isNonLocal s) (callThroughParent getDimensions mp)

-- Get the list of defined units (with shadowing).
getUnits :: ScopeFrame -> [UnitDef]
getUnits s@(Frame _ us _ _ _ _ _ mp)
  =   us
  ++  Prelude.filter (isNonLocal s . fst) (callThroughParent getUnits mp)

-- Get the dimensionality of the unit with the given ID. If the unit has no
-- dimension, or if it is not defined, the result is Nothing.
getUnitDimensionById :: ScopeFrame -> Identifier -> Maybe Dimensionality
getUnitDimensionById f id = case find ((==id).fst) $ getUnits f of
  Just (_, md) -> md
  Nothing -> Nothing

-- Get the list of defined conversions.
getConversions :: ScopeFrame -> [(Units, Units, Transformation)]
getConversions (Frame _ _ cs _ _ _ _ mp)
  = cs ++ callThroughParent getConversions mp

-- Get the list of assignments (with shadowing).
getAssignments :: ScopeFrame -> [Assignment]
getAssignments s@(Frame _ _ _ as _ _ _ mp)
  =   as
  ++  Prelude.filter
    (isNonLocal s . getAssignmentId)
    (callThroughParent getAssignments mp)

-- Extract the ID from the assignment structure.
getAssignmentId :: Assignment -> Identifier
getAssignmentId (id, _) = id

-- Find the assignment with the given ID.
getAssignmentById :: ScopeFrame -> Identifier -> Maybe Assignment
getAssignmentById f id = find ((==id).getAssignmentId) $ getAssignments f

-- Get the list of defined inputs (with shadowing).
getInputs :: ScopeFrame -> [Input]
getInputs s@(Frame _ _ _ _ is _ _ mp)
  =   is
  ++  Prelude.filter
    (isNonLocal s . getInputId)
    (callThroughParent getInputs mp)

-- Get the ID of the given input structure.
getInputId :: Input -> Identifier
getInputId (id, _) = id

-- Find the input with the given ID.
getInputById :: ScopeFrame -> Identifier -> Maybe Input
getInputById f id = find ((==id).getInputId) $ getInputs f

-- Get the list of defined relations (with shadowing).
getRelations :: ScopeFrame -> [Rel]
getRelations s@(Frame _ _ _ _ _ rs _ mp)
  =   rs
  ++  Prelude.filter 
    (isNonLocal s . getRelationId)
    (callThroughParent getRelations mp)

-- Get the ID of the given relation structure.
getRelationId :: Rel -> Identifier
getRelationId (id, _) = id

-- Find the relation with the given ID.
getRelationById :: ScopeFrame -> Identifier -> Maybe Rel
getRelationById f id = find ((==id).getRelationId) $ getRelations f

-- Is the given pragma defined within scope.
hasPragma :: ScopeFrame -> Pragma -> Bool
hasPragma (Frame _ _ _ _ _ _ ps mp) pragma = pragma `Set.member` ps || inParent
  where
    inParent = case mp of
      Nothing -> False
      Just parent -> parent `hasPragma` pragma

withDimension :: ScopeFrame -> Identifier -> ScopeFrame
withDimension (Frame ds us cs as is rs ps mp) d
  = Frame (d:ds) us cs as is rs ps mp

withUnit :: ScopeFrame -> UnitDef -> ScopeFrame
withUnit (Frame ds us cs as is rs ps mp) u = Frame ds (u:us) cs as is rs ps mp

replaceInput :: ScopeFrame -> Input -> ScopeFrame
replaceInput (Frame ds us cs as is rs ps mp) i@(id, _)
  = Frame ds us cs as (i : Prelude.filter ((/=id).fst) is) rs ps mp

withConversion :: ScopeFrame -> (Units, Units, Transformation) -> ScopeFrame
withConversion (Frame ds us cs as is rs ps mp) c
  = Frame ds us (c:cs) as is rs ps mp

withAssignment :: ScopeFrame -> Assignment -> ScopeFrame
withAssignment (Frame ds us cs as is rs ps mp) a
  = Frame ds us cs (a:as) is rs ps mp

withInput :: ScopeFrame -> Input -> ScopeFrame
withInput (Frame ds us cs as is rs ps mp) i
  = Frame ds us cs as (i:is) rs ps mp

withRelation :: ScopeFrame -> Rel -> ScopeFrame
withRelation (Frame ds us cs as is rs ps mp) r
  = Frame ds us cs as is (r:rs) ps mp

withPragma :: ScopeFrame -> Pragma -> ScopeFrame
withPragma (Frame ds us cs as is rs ps mp) p
  = Frame ds us cs as is rs (p `Set.insert` ps) mp

pushScope :: ScopeFrame -> ScopeFrame
pushScope f = Frame [] [] [] [] [] [] Set.empty $ Just f

popScope :: ScopeFrame -> ScopeFrame
popScope (Frame _ _ _ _ _ _ _ (Just f)) = f

hasParentScope :: ScopeFrame -> Bool
hasParentScope (Frame _ _ _ _ _ _ _ mp) = isJust mp
