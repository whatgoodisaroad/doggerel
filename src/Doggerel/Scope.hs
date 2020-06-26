module Doggerel.Scope (
    ScopeFrame,
    initFrame,
    getAssignments,
    getConversions,
    getDimensions,
    getUnits,
    withAssignment,
    withConversion,
    withDimension,
    withUnit
  )
  where

import Doggerel.Ast
import Doggerel.Core
import Doggerel.Conversion

-- Represents a lexical scope for runtime.
data ScopeFrame
  = Frame
      [Identifier]                                -- Dimensions
      [(Identifier, Maybe Identifier)]            -- Units
      [(Identifier, Identifier, Transformation)]  -- Conversions
      [(Identifier, ValueExpression)]             -- Assignments
  deriving (Eq, Show)

-- An empty scope frame.
initFrame :: ScopeFrame
initFrame = Frame [] [] [] []

getDimensions :: ScopeFrame -> [Identifier]
getDimensions (Frame ds _ _ _) = ds

getUnits :: ScopeFrame -> [(Identifier, Maybe Identifier)]
getUnits (Frame _ us _ _) = us

getConversions :: ScopeFrame -> [(Identifier, Identifier, Transformation)]
getConversions (Frame _ _ cs _) = cs

getAssignments :: ScopeFrame -> [(Identifier, ValueExpression)]
getAssignments (Frame _ _ _ as) = as

withDimension :: ScopeFrame -> Identifier -> ScopeFrame
withDimension (Frame ds us cs as) d = Frame (d:ds) us cs as

withUnit :: ScopeFrame -> (Identifier, Maybe Identifier) -> ScopeFrame
withUnit (Frame ds us cs as) u = Frame ds (u:us) cs as

withConversion ::
     ScopeFrame
  -> (Identifier, Identifier, Transformation)
  -> ScopeFrame
withConversion (Frame ds us cs as) c = Frame ds us (c:cs) as

withAssignment :: ScopeFrame -> (Identifier, ValueExpression) -> ScopeFrame
withAssignment (Frame ds us cs as) a = Frame ds us cs (a:as)
