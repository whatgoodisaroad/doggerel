module Doggerel.Scope (
    ScopeFrame,
    initFrame,
    getAssignmentId,
    getAssignments,
    getConversions,
    getDimensions,
    getInputId,
    getInputs,
    getUnits,
    withAssignment,
    withConversion,
    withDimension,
    withInput,
    withUnit
  )
  where

import Doggerel.Ast
import Doggerel.Core
import Doggerel.Conversion

type Assignment = (Identifier, ValueExpression, Vector)
type Input = (Identifier, Either Dimensionality Scalar)

-- Represents a lexical scope for runtime.
data ScopeFrame
  = Frame
      [Identifier]                                -- Dimensions
      [(Identifier, Maybe Identifier)]            -- Units
      [(Identifier, Identifier, Transformation)]  -- Conversions
      [Assignment]                                -- Assignments
      [Input]                                     -- Inputs
  deriving (Eq, Show)

-- An empty scope frame.
initFrame :: ScopeFrame
initFrame = Frame [] [] [] [] []

getDimensions :: ScopeFrame -> [Identifier]
getDimensions (Frame ds _ _ _ _) = ds

getUnits :: ScopeFrame -> [(Identifier, Maybe Identifier)]
getUnits (Frame _ us _ _ _) = us

getConversions :: ScopeFrame -> [(Identifier, Identifier, Transformation)]
getConversions (Frame _ _ cs _ _) = cs

getAssignments :: ScopeFrame -> [Assignment]
getAssignments (Frame _ _ _ as _) = as

getAssignmentId :: Assignment -> Identifier
getAssignmentId (id, _, _) = id

getInputs :: ScopeFrame -> [Input]
getInputs (Frame _ _ _ _ is) = is

getInputId :: Input -> Identifier
getInputId (id, _) = id

withDimension :: ScopeFrame -> Identifier -> ScopeFrame
withDimension (Frame ds us cs as is) d = Frame (d:ds) us cs as is

withUnit :: ScopeFrame -> (Identifier, Maybe Identifier) -> ScopeFrame
withUnit (Frame ds us cs as is) u = Frame ds (u:us) cs as is

withConversion ::
     ScopeFrame
  -> (Identifier, Identifier, Transformation)
  -> ScopeFrame
withConversion (Frame ds us cs as is) c = Frame ds us (c:cs) as is

withAssignment :: ScopeFrame -> Assignment -> ScopeFrame
withAssignment (Frame ds us cs as is) a = Frame ds us cs (a:as) is

withInput :: ScopeFrame -> Input -> ScopeFrame
withInput (Frame ds us cs as is) i = Frame ds us cs as (i:is)
