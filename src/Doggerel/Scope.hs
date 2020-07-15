module Doggerel.Scope (
    ScopeFrame,
    initFrame,
    getAssignmentId,
    getAssignments,
    getConversions,
    getDimensions,
    getInputId,
    getInputs,
    getRelations,
    getRelationId,
    getUnits,
    withAssignment,
    withConversion,
    withDimension,
    withInput,
    withRelation,
    withUnit
  )
  where

import Doggerel.Ast
import Doggerel.Core
import Doggerel.Conversion

type Assignment = (Identifier, Expr, Vector)
type Input = (Identifier, Either Dimensionality Scalar)
type Rel = (
    Identifier,
    ValueExpression Units Quantity,
    ValueExpression Units Quantity
  )

-- Represents a lexical scope for runtime.
data ScopeFrame
  = Frame
      [Identifier]                                -- Dimensions
      [(Identifier, Maybe Identifier)]            -- Units
      [(Identifier, Identifier, Transformation)]  -- Conversions
      [Assignment]                                -- Assignments
      [Input]                                     -- Inputs
      [Rel]                                       -- Relations
  deriving (Eq, Show)

-- An empty scope frame.
initFrame :: ScopeFrame
initFrame = Frame [] [] [] [] [] []

getDimensions :: ScopeFrame -> [Identifier]
getDimensions (Frame ds _ _ _ _ _) = ds

getUnits :: ScopeFrame -> [(Identifier, Maybe Identifier)]
getUnits (Frame _ us _ _ _ _) = us

getConversions :: ScopeFrame -> [(Identifier, Identifier, Transformation)]
getConversions (Frame _ _ cs _ _ _) = cs

getAssignments :: ScopeFrame -> [Assignment]
getAssignments (Frame _ _ _ as _ _) = as

getAssignmentId :: Assignment -> Identifier
getAssignmentId (id, _, _) = id

getInputs :: ScopeFrame -> [Input]
getInputs (Frame _ _ _ _ is _) = is

getInputId :: Input -> Identifier
getInputId (id, _) = id

getRelations :: ScopeFrame -> [Rel]
getRelations (Frame _ _ _ _ _ rs) = rs

getRelationId :: Rel -> Identifier
getRelationId (id, _, _) = id

withDimension :: ScopeFrame -> Identifier -> ScopeFrame
withDimension (Frame ds us cs as is rs) d = Frame (d:ds) us cs as is rs

withUnit :: ScopeFrame -> (Identifier, Maybe Identifier) -> ScopeFrame
withUnit (Frame ds us cs as is rs) u = Frame ds (u:us) cs as is rs

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
