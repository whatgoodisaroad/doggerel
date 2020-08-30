module Doggerel.Scope (
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
    getUnits,
    replaceInput,
    withAssignment,
    withConversion,
    withDimension,
    withInput,
    withRelation,
    withUnit
  )
  where

import Data.Map.Strict as Map
import Data.List (find)
import Data.Set as Set
import Doggerel.Ast
import Doggerel.Core
import Doggerel.Conversion
import Doggerel.DegreeMap

type UnitDef = (Identifier, Maybe Dimensionality)
type Assignment = (Identifier, Expr)
type Input = (Identifier, Either Dimensionality Scalar)
type Rel = (Identifier, Map (Set Units) (Units, ValueExpression Units Quantity))

-- Represents a lexical scope for runtime.
data ScopeFrame
  = Frame
      [Identifier]                                  -- Dimensions
      [UnitDef]                                     -- Units
      [(Identifier, Identifier, Transformation)]    -- Conversions
      [Assignment]                                  -- Assignments
      [Input]                                       -- Inputs
      [Rel]                                         -- Relations
  deriving (Eq, Show)

-- An empty scope frame.
initFrame :: ScopeFrame
initFrame = Frame [] [] [] [] [] []

getDimensions :: ScopeFrame -> [Identifier]
getDimensions (Frame ds _ _ _ _ _) = ds

getUnits :: ScopeFrame -> [UnitDef]
getUnits (Frame _ us _ _ _ _) = us

getConversions :: ScopeFrame -> [(Identifier, Identifier, Transformation)]
getConversions (Frame _ _ cs _ _ _) = cs

getAssignments :: ScopeFrame -> [Assignment]
getAssignments (Frame _ _ _ as _ _) = as

getAssignmentId :: Assignment -> Identifier
getAssignmentId (id, _) = id

getAssignmentById :: ScopeFrame -> Identifier -> Maybe Assignment
getAssignmentById f id = find ((==id).getAssignmentId) $ getAssignments f

getInputs :: ScopeFrame -> [Input]
getInputs (Frame _ _ _ _ is _) = is

getInputId :: Input -> Identifier
getInputId (id, _) = id

getInputById :: ScopeFrame -> Identifier -> Maybe Input
getInputById f id = find ((==id).getInputId) $ getInputs f

getRelations :: ScopeFrame -> [Rel]
getRelations (Frame _ _ _ _ _ rs) = rs

getRelationId :: Rel -> Identifier
getRelationId (id, _) = id

getRelationById :: ScopeFrame -> Identifier -> Maybe Rel
getRelationById f id = find ((==id).getRelationId) $ getRelations f

withDimension :: ScopeFrame -> Identifier -> ScopeFrame
withDimension (Frame ds us cs as is rs) d = Frame (d:ds) us cs as is rs

withUnit :: ScopeFrame -> UnitDef -> ScopeFrame
withUnit (Frame ds us cs as is rs) u = Frame ds (u:us) cs as is rs

replaceInput :: ScopeFrame -> Input -> ScopeFrame
replaceInput (Frame ds us cs as is rs) i@(id, _)
  = Frame ds us cs as (i:(Prelude.filter ((/=id).fst) is)) rs

withConversion ::
     ScopeFrame
  -> (Identifier, Identifier, Transformation)
  -> ScopeFrame
withConversion (Frame ds us cs as is rs) c = Frame ds us (c:cs) as is rs

withAssignment :: ScopeFrame -> Assignment -> ScopeFrame
withAssignment (Frame ds us cs as is rs) a = Frame ds us cs (a:as) is rs

withInput :: ScopeFrame -> Input -> ScopeFrame
withInput (Frame ds us cs as is rs) i = Frame ds us cs as (i:is) rs

withRelation :: ScopeFrame -> Rel -> ScopeFrame
withRelation (Frame ds us cs as is rs) r = Frame ds us cs as is (r:rs)
