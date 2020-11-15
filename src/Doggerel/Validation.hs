module Doggerel.Validation (
    ExecFail(..),
    allDimensionsAreDefined,
    allReferencesAreDefined,
    allUnitsAreDimensional,
    containsExponent,
    failedAssignmentConstraints,
    isExistingIdentifier,
    isStaticIdentifier,
    invalidExprUnitsError,
    invalidUnitError,
    invalidUnitExpressionError
  ) where

import Data.List.Extra (firstJust)
import Data.Map.Strict (keys)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Set as Set (Set, empty, insert, fromList, member, toList)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.DegreeMap (allKeys, getMap)
import Doggerel.Eval
import Doggerel.Scope

data ExecFail
  = ExecEvalFail EvalFail
  | UnknownIdentifier String
  | RedefinedIdentifier String
  | InvalidConversion String
  | UnsatisfiableConstraint String
  | UnsatisfiedConstraint String
  | InvalidVectorExpression String
  | InternalExecError String
  | InvalidUnitSpec String
  deriving (Eq, Show)

-- Is the given identifier already defined in the given state as anything?
isExistingIdentifier :: Identifier -> ScopeFrame -> Bool
isExistingIdentifier id f
  =  id `elem` map fst (getDimensions f)
  || id `elem` map fst (getUnits f)
  || id `elem` map getAssignmentId (getAssignments f)
  || id `elem` map getInputId (getInputs f)
  || id `elem` map getRelationId (getRelations f)

isDefinedAsUnit :: Identifier -> ScopeFrame -> Bool
isDefinedAsUnit id f = id `elem` map fst (getUnits f)

isStaticIdentifier :: Identifier -> ScopeFrame -> Bool
isStaticIdentifier i f = i `member` getStaticIdentifiers f

isDefinedAsAssignment :: ScopeFrame -> Identifier -> Bool
isDefinedAsAssignment f id = id `elem` map getAssignmentId (getAssignments f)

isDefinedAsAssignmentRelationOrInput :: ScopeFrame -> Identifier -> Bool
isDefinedAsAssignmentRelationOrInput f id
  =  isDefinedAsAssignment f id
  || id `elem` map getInputId (getInputs f)
  || id `elem` map getRelationId (getRelations f)

allReferencesAreDefined :: ScopeFrame -> Expr -> Bool
allReferencesAreDefined f e
  = all (isDefinedAsAssignmentRelationOrInput f)
  $ referencesOfExpr e

invalidExprUnitsError :: ScopeFrame -> Expr -> Maybe ExecFail
invalidExprUnitsError f = firstJust (invalidBaseUnitError f) . unitsOfExpr

allDimensionsAreDefined :: ScopeFrame -> Dimensionality -> Bool
allDimensionsAreDefined f d = all exists dimIds
  where
    dimIds :: [String]
    dimIds = map (\(Dimension d) -> d) $ keys $ getMap d

    exists :: String -> Bool
    exists s = (s `elem` map fst (getDimensions f))
      || s `elem` dimensionlessUnits

    dimensionlessUnits :: [String]
    dimensionlessUnits
      = map fst
      $ filter (\(_, opts) -> isNothing $ unitOptsDimensionality opts)
      $ getUnits f

allNaturalUnitsAreIndexed :: ScopeFrame -> Units -> Bool
allNaturalUnitsAreIndexed f
  = all (\bu -> isIndexed bu == isNatural bu)
  . keys
  . getMap
  where
    isIndexed :: BaseUnit -> Bool
    isIndexed (BaseUnit _ Nothing) = False
    isIndexed (BaseUnit _ (Just _)) = True
    isNatural :: BaseUnit -> Bool
    isNatural (BaseUnit id _) = isUnitNaturalById f id

allUnitsAreDimensional :: ScopeFrame -> Units -> Bool
allUnitsAreDimensional f = allKeys isDefinedWithDim
  where
    isDefinedWithDim :: BaseUnit -> Bool
    -- If the unit has an index, it is not dimensional.
    isDefinedWithDim (BaseUnit id (Just _)) = False
    isDefinedWithDim (BaseUnit id Nothing) = isJust $ getUnitDimensionById f id

invalidUnitExpressionError ::
     ScopeFrame
  -> ValueExpression Units q
  -> Maybe ExecFail
invalidUnitExpressionError f = firstJust (invalidUnitError f) . referencesOfExpr

containsExponent :: ValueExpression ref lit -> Bool
containsExponent (UnaryOperatorApply (Exponent _) _) = True
containsExponent (UnaryOperatorApply _ e) = containsExponent e
containsExponent (BinaryOperatorApply _ e1 e2)
  = containsExponent e1 || containsExponent e2
containsExponent (FunctionApply _ e) = containsExponent e
containsExponent _ = False

invalidBaseUnitError :: ScopeFrame -> BaseUnit -> Maybe ExecFail
invalidBaseUnitError f bu@(BaseUnit id _)
  | not $ isDefinedAsUnit id f = Just $ UnknownIdentifier unknownMsg
  | isIndexed bu && (not $ isNatural bu) = Just $ InvalidUnitSpec overIndexedMsg
  | (not $ isIndexed bu) && isNatural bu = Just $ InvalidUnitSpec noIndexedMsg
  | otherwise = Nothing
  where
    isIndexed :: BaseUnit -> Bool
    isIndexed (BaseUnit _ Nothing) = False
    isIndexed (BaseUnit _ (Just _)) = True
    isNatural :: BaseUnit -> Bool
    isNatural (BaseUnit id _) = isUnitNaturalById f id
    unknownMsg = "Unknown units: " ++ show bu
    overIndexedMsg
      = "Invalid unit: " ++ show bu ++ " has index but is not natural"
    noIndexedMsg
      = "Invalid unit: " ++ show bu ++ " is natural, but is missing an index"

invalidUnitError :: ScopeFrame -> Units -> Maybe ExecFail
invalidUnitError f = firstJust (invalidBaseUnitError f) . keys . getMap

-- Given a set of assignment options, a scope frame and the resulting vector
-- value to be potentially recorded in the assignment, give a list of strings
-- that desceibe the ways in which constraints in those options were violated by
-- the vector. If the resulting list is empty, there are no violations
failedAssignmentConstraints ::
     Set AssignmentOption
  -> ScopeFrame
  -> VectorDimensionality
  -> [String]
failedAssignmentConstraints opts f dims
  = concat $ flip fmap (toList opts)
  $ \o -> case (o, dims) of
    (ConstrainedScalar, dims@(VecDims dimSet)) ->
      (["Constrained to scalar, but vector had multiple components:\n" ++
        "  actual: " ++ show dims
        | 1 /= length dimSet])
    (ConstrainedDimensionality target, dims) ->
      (["Vector does not match target dims:\n" ++
        "  target: " ++ show target ++ "\n" ++ "  actual: " ++ show dims
        | target /= dims])
