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
    getUnits,
    hasPragma,
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

data Pragma = AsciiOutput
  deriving (Eq, Ord, Show)

-- Represents a lexical scope for runtime.
data ScopeFrame
  = Frame
      [Identifier]                                  -- Dimensions
      [UnitDef]                                     -- Units
      [(Identifier, Identifier, Transformation)]    -- Conversions
      [Assignment]                                  -- Assignments
      [Input]                                       -- Inputs
      [Rel]                                         -- Relations
      (Set Pragma)
  deriving (Eq, Show)

-- An empty scope frame.
initFrame :: ScopeFrame
initFrame = Frame [] [] [] [] [] [] Set.empty

getDimensions :: ScopeFrame -> [Identifier]
getDimensions (Frame ds _ _ _ _ _ _) = ds

getUnits :: ScopeFrame -> [UnitDef]
getUnits (Frame _ us _ _ _ _ _) = us

getConversions :: ScopeFrame -> [(Identifier, Identifier, Transformation)]
getConversions (Frame _ _ cs _ _ _ _) = cs

getAssignments :: ScopeFrame -> [Assignment]
getAssignments (Frame _ _ _ as _ _ _) = as

getAssignmentId :: Assignment -> Identifier
getAssignmentId (id, _) = id

getAssignmentById :: ScopeFrame -> Identifier -> Maybe Assignment
getAssignmentById f id = find ((==id).getAssignmentId) $ getAssignments f

getInputs :: ScopeFrame -> [Input]
getInputs (Frame _ _ _ _ is _ _) = is

getInputId :: Input -> Identifier
getInputId (id, _) = id

getInputById :: ScopeFrame -> Identifier -> Maybe Input
getInputById f id = find ((==id).getInputId) $ getInputs f

getRelations :: ScopeFrame -> [Rel]
getRelations (Frame _ _ _ _ _ rs _) = rs

getRelationId :: Rel -> Identifier
getRelationId (id, _) = id

getRelationById :: ScopeFrame -> Identifier -> Maybe Rel
getRelationById f id = find ((==id).getRelationId) $ getRelations f

hasPragma :: ScopeFrame -> Pragma -> Bool
hasPragma (Frame _ _ _ _ _ _ ps) p = p `Set.member` ps

withDimension :: ScopeFrame -> Identifier -> ScopeFrame
withDimension (Frame ds us cs as is rs ps) d = Frame (d:ds) us cs as is rs ps

withUnit :: ScopeFrame -> UnitDef -> ScopeFrame
withUnit (Frame ds us cs as is rs ps) u = Frame ds (u:us) cs as is rs ps

replaceInput :: ScopeFrame -> Input -> ScopeFrame
replaceInput (Frame ds us cs as is rs ps) i@(id, _)
  = Frame ds us cs as (i:(Prelude.filter ((/=id).fst) is)) rs ps

withConversion ::
     ScopeFrame
  -> (Identifier, Identifier, Transformation)
  -> ScopeFrame
withConversion (Frame ds us cs as is rs ps) c = Frame ds us (c:cs) as is rs ps

withAssignment :: ScopeFrame -> Assignment -> ScopeFrame
withAssignment (Frame ds us cs as is rs ps) a = Frame ds us cs (a:as) is rs ps

withInput :: ScopeFrame -> Input -> ScopeFrame
withInput (Frame ds us cs as is rs ps) i = Frame ds us cs as (i:is) rs ps

withRelation :: ScopeFrame -> Rel -> ScopeFrame
withRelation (Frame ds us cs as is rs ps) r = Frame ds us cs as is (r:rs) ps

withPragma :: ScopeFrame -> Pragma -> ScopeFrame
withPragma (Frame ds us cs as is rs ps) p = Frame ds us cs as is rs (p `Set.insert` ps)
