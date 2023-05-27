{-# LANGUAGE LambdaCase #-}

module Doggerel.Validation (
    ExecFail(..),
    allDimensionsAreDefined,
    allReferencesAreDefined,
    allUnitsAreDimensional,
    containsExponent,
    dimspecAsDimensionality,
    failedAssignmentConstraints,
    failedOperatorConstraints,
    isExistingIdentifier,
    invalidAssignmentConstraints,
    invalidDimspecError,
    invalidExprUnitsError,
    invalidRelationParamNames,
    invalidRelationParamUnits,
    invalidRelationRefError,
    invalidUnitError,
    invalidUnitExpressionError,
    isMatch,
    isStaticIdentifier,
    materializeDimspec,
    materializeDimspec',
    relationIdentifierReusedError
  ) where

import Data.List (find, intersect, nub, partition)
import Data.Map.Strict as Map (fromList, keys, singleton)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Set as Set (
    Set,
    empty,
    insert,
    fromList,
    member,
    null,
    singleton,
    toList,
    union
  )
import Doggerel.Ast
import Doggerel.Core
import Doggerel.DegreeMap (
    allKeys,
    anyKey,
    divide,
    emptyMap,
    fromMap,
    getMap,
    hasNumerator,
    lookupDegree,
    mapPairs,
    multiply,
    toMap
  )
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
  deriving Eq

instance Show ExecFail where
  show (ExecEvalFail e) = show e
  show (UnknownIdentifier m) = "[Unknown ID] " ++ m
  show (RedefinedIdentifier m) = "[Redefined ID] " ++ m
  show (InvalidConversion m) = "[Invalid conversion] " ++ m
  show (UnsatisfiableConstraint m) = "[Unsatisfiable] " ++ m
  show (UnsatisfiedConstraint m) = "[Unsatisfied] " ++ m
  show (InvalidVectorExpression m) = "[Invalid] " ++ m
  show (InternalExecError m) = "[Internal] " ++ m
  show (InvalidUnitSpec m) = "[Invalid] " ++ m

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

-- Expand any dimensions in the given dimspec that are aliases in the given
-- scope.
materializeDimspec :: ScopeFrame -> Dimspec -> Dimspec
materializeDimspec f ds = if ds == ds' then ds else materializeDimspec f ds'
  where
    ds' = materializeDimspec' f ds

-- Do one round of alias expansions
materializeDimspec' :: ScopeFrame -> Dimspec -> Dimspec
materializeDimspec' f ds@(DSTerm (DSTermDim id Nothing deg))
  = fromMaybe ds $ fmap ((flip dimspecExp deg).snd) $ getDimensionAliasById f id
materializeDimspec' f ds@(DSTerm _) = ds
materializeDimspec' f (DSSum dss) = DSSum $ map (materializeDimspec' f) dss
materializeDimspec' f (DSProduct dss)
  = DSProduct $ map (materializeDimspec' f) dss

invalidDimspecError :: ScopeFrame -> Dimspec -> Maybe ExecFail
invalidDimspecError f ds =
  if not $ Prelude.null undefinedIds
  then Just $ UnknownIdentifier $
    "Dimspec refers to unknown identifier: " ++ head undefinedIds
  else if not $ Prelude.null nonIndexedNaturalIds
  then Just $ InvalidUnitSpec $
    "Dimspec does not index a natural dimension " ++ head nonIndexedNaturalIds
  else if not $ Prelude.null indexedNonNaturalIds
  then Just $ InvalidUnitSpec $
    "Dimspec indexes a non-natural dimension: " ++ head indexedNonNaturalIds
  else Nothing
  where
    (nonIndexed, indexed) = getDimspecIdentifiers ds
    undefinedIds
      = filter (not . dimensionIsDefined f)
      $ toList
      $ nonIndexed `union` indexed
    nonIndexedNaturalIds = filter (isUnitNaturalById f) $ toList nonIndexed
    indexedNonNaturalIds = filter (not . isUnitNaturalById f) $ toList indexed

allDimensionsAreDefined :: ScopeFrame -> Dimensionality -> Bool
allDimensionsAreDefined f d
  = all (dimensionIsDefined f)
  $ map (\(Dimension d _) -> d)
  $ keys
  $ getMap d

-- Is the given identifier defined as a dimension (not as a dimension alias)?
dimensionIsDefined :: ScopeFrame -> Identifier -> Bool
dimensionIsDefined f id
  = elem id
  $   (map fst $ getDimensions f)
  ++  dimensionlessUnits
  ++  (map fst $ getDimensionAliases f)
  where
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

invalidRelationParamUnits :: ScopeFrame -> RelationParams -> Maybe ExecFail
invalidRelationParamUnits f = firstJust (invalidUnitError f) . (map snd)

invalidRelationParamNames :: RelationParams -> Maybe ExecFail
invalidRelationParamNames params = if (length ids) == (length $ nub ids)
  then Nothing
  else Just $ RedefinedIdentifier $ "Relation parameter name is repeated"
  where
    ids = map fst params

invalidRelationRefError ::
     RelationParams
  -> ValueExpression Identifier q
  -> Maybe ExecFail
invalidRelationRefError params e = do
  id <- find (\id -> not $ elem id $  map fst params) $ referencesOfExpr e
  return $ UnknownIdentifier $ "Unknown relation expression identifier: " ++ id

relationIdentifierReusedError ::
     ValueExpression Identifier q
  -> ValueExpression Identifier q
  -> Maybe ExecFail
relationIdentifierReusedError e1 e2 = if Prelude.null reused
  then Nothing
  else Just $ RedefinedIdentifier $
    "Identifier appears on both sides of a relation: " ++ head reused
  where
    reused = intersect rs1 rs2
    rs1 = referencesOfExpr e1
    rs2 = referencesOfExpr e2

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
  | isIndexed bu && not (isNatural bu) = Just $ InvalidUnitSpec overIndexedMsg
  | not (isIndexed bu) && isNatural bu = Just $ InvalidUnitSpec noIndexedMsg
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

invalidAssignmentConstraints ::
     Set AssignmentOption
  -> ScopeFrame
  -> Maybe ExecFail
invalidAssignmentConstraints opts f = do
  (ConstrainedDimensionality target) <- flip find opts $ \opt -> case opt of {
      (ConstrainedDimensionality _) -> True;
      _ -> False;
    }
  invalidDimspecError f target

-- Given a set of assignment options, a scope frame and the resulting vector
-- value to be potentially recorded in the assignment, give a list of strings
-- that describe the ways in which constraints in those options were violated by
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
      let mds = materializeDimspec f target in
      (["Vector does not match target dims:\n" ++
        "  target: " ++ show mds ++ "\n" ++ "  actual: " ++ show dims
        | not $ isMatch mds dims])

-- For every use of a boolean operator in the given expression, do all the
-- operands statically evaluate to vectors of boolean dimensionality.
failedOperatorConstraints :: ScopeFrame -> Expr -> Maybe String
failedOperatorConstraints f (UnaryOperatorApply op expr) = do
  let msub = failedOperatorConstraints f expr
  exprDims <- staticEval f expr
  if isJust msub
    then msub
    else failedUnaryOperatorConstraint op exprDims
failedOperatorConstraints f (BinaryOperatorApply op e1 e2) = do
  let msub1 = failedOperatorConstraints f e1
  let msub2 = failedOperatorConstraints f e2
  e1Dims <- staticEval f e1
  e2Dims <- staticEval f e2
  if isJust msub1
    then msub1
    else if isJust msub2
      then msub2
      else failedBinaryOperatorConstraint op e1Dims e2Dims
failedOperatorConstraints f (FunctionApply _ expr)
  = failedOperatorConstraints f expr
failedOperatorConstraints _ _ = Nothing

failedUnaryOperatorConstraint ::
     UnaryOperator
  -> VectorDimensionality
  -> Maybe String
failedUnaryOperatorConstraint LogicalNot dims = if dims == booleanDims
  then Nothing
  else Just
    $   "The logical not operator must be applied to a boolean vector, but was"
    ++  " applied to: " ++ show dims
failedUnaryOperatorConstraint _ _ = Nothing

-- Find the violated constraints for hypothetically applying the given binary
-- operator to a pair of vectors of the given dimensionalities. The result is
-- nothing when there are no violations.
failedBinaryOperatorConstraint ::
     BinaryOperator
  -> VectorDimensionality
  -> VectorDimensionality
  -> Maybe String
failedBinaryOperatorConstraint LogicalAnd d1 d2 =
  logicalBinOpConstraint "and" d1 d2
failedBinaryOperatorConstraint LogicalOr d1 d2 =
  logicalBinOpConstraint "or" d1 d2
failedBinaryOperatorConstraint LessThan d1 d2 =
  inequalityBinOpConstraint "less-than" d1 d2
failedBinaryOperatorConstraint GreaterThan d1 d2 =
  inequalityBinOpConstraint "greater-than" d1 d2
failedBinaryOperatorConstraint LessThanOrEqualTo d1 d2 =
  inequalityBinOpConstraint "less-than-or-equal-to" d1 d2
failedBinaryOperatorConstraint GreaterThanOrEqualTo d1 d2 =
  inequalityBinOpConstraint "greater-than-or-equal-to" d1 d2
failedBinaryOperatorConstraint _ _ _ = Nothing

-- Find the constraints for applying the given logical binary operator.
logicalBinOpConstraint ::
     String
  -> VectorDimensionality
  -> VectorDimensionality
  -> Maybe String
logicalBinOpConstraint name d1 d2 =
  if d1 == booleanDims && d2 == booleanDims
  then Nothing
  else Just
    $   "The logical " ++ name ++ " operator must be applied to a boolean "
    ++  "vector, but was applied to but was applied to: "
    ++  if d1 == booleanDims then show d2 else show d1

-- Find the constraints for applying the given inequality operator.
inequalityBinOpConstraint ::
     String
  -> VectorDimensionality
  -> VectorDimensionality
  -> Maybe String
inequalityBinOpConstraint name d1 d2 =
  if d1 == d2
  then Nothing
  else Just
    $   "The inequality " ++ name ++ " operator must be applied to vectors of "
    ++  "the same dimensionality, but was applied to: "
    ++  show d1 ++ " and " ++ show d2

-- Internal data structures to represent normalized dimspecs as a sum of
-- products.
data NormDimspecProd = NormDimspecProd {
    ndpDims :: [DimspecTerm],
    ndpRanges :: [DimspecTerm],
    ndpVars :: [DimspecTerm]
  }
  deriving Show
newtype NormDimspec = NormDimspec [NormDimspecProd] deriving Show

dsNormalize :: Dimspec -> NormDimspec
dsNormalize = NormDimspec . map dstNormalize . dsFlatten

-- Turn a product list of DimspecTerms into a normalized product
dstNormalize :: [DimspecTerm] -> NormDimspecProd
dstNormalize [] = NormDimspecProd { ndpDims = [], ndpRanges = [], ndpVars = []}
dstNormalize (t:ts) =
  let sub = dstNormalize ts
  in case t of
    (DSTermDim _ _ _) -> sub { ndpDims = t:(ndpDims sub) }
    (DSTermRange _ _ _ _) -> sub { ndpRanges = t:(ndpRanges sub) }
    (DSTermVar _ _) -> sub { ndpVars = t:(ndpVars sub) }

-- Turn the given dimspec into a list of lists of terms, where the outer list is
-- a sum and the inner lists are products.
dsFlatten :: Dimspec -> [[DimspecTerm]]
dsFlatten (DSTerm (DSTermRange id (Just low) (Just high) deg)) =
  flip map [low..high] $ \idx -> [DSTermDim id (Just idx) deg]
dsFlatten (DSTerm d) = [[d]]
dsFlatten (DSSum ts) = concatMap dsFlatten ts
dsFlatten (DSProduct [t]) = dsFlatten t
dsFlatten (DSProduct (t:ts)) = do
  t' <- dsFlatten t
  ts' <- dsFlatten (DSProduct ts)
  return $ t' ++ ts'

-- Does the given dimspec describe all the components of the given vector
-- dimensionality?
isMatch :: Dimspec -> VectorDimensionality -> Bool
isMatch ds vd = vd == projectDims ds vd

-- Filter the components of the given vector dimensionality to those that are
-- described by the given dimspec.
projectDims :: Dimspec -> VectorDimensionality -> VectorDimensionality
projectDims ds vd = fst $ foldr f (nullDims, vd) prods
  where
    NormDimspec prods = dsNormalize ds

    f :: NormDimspecProd
      -> (VectorDimensionality, VectorDimensionality)
      -> (VectorDimensionality, VectorDimensionality)
    f p (VecDims pos, neg) = (VecDims $ pos `union` pos', neg')
      where
        (VecDims pos', neg') = partitionByProd p neg

-- Given a single normalized dimspec product and a vector dimensionality,
-- partition the dimensionality by whether the terms match the product. The
-- first part of the pair are the matches, and the second part the mismatches.
partitionByProd ::
     NormDimspecProd
  -> VectorDimensionality
  -> (VectorDimensionality, VectorDimensionality)
partitionByProd (NormDimspecProd [] [] []) vd = (nullDims, vd)
partitionByProd p (VecDims vds) = (vd pos, vd neg)
  where
    lvds = Set.toList vds
    vd = VecDims . Set.fromList
    (pos, neg) = partition (isProdMatch p) lvds

-- Does the given normalized product match the given component dimensionality.
isProdMatch :: NormDimspecProd -> Dimensionality -> Bool
isProdMatch (NormDimspecProd [] [] []) dim = dim == emptyMap
isProdMatch p dim = case factorTerm t dim of
  Nothing -> False
  Just dim' -> isProdMatch p' dim'
  where
    (t, p') = case p of
      (NormDimspecProd ts (r:rs) vs) -> (r, NormDimspecProd ts rs vs)
      (NormDimspecProd (t:ts) [] vs) -> (t, NormDimspecProd ts [] vs)

-- If the given term is part of the component dimensionality, then slice it out.
-- If it's not a part, then give Nothing.
factorTerm :: DimspecTerm -> Dimensionality -> Maybe Dimensionality
factorTerm dst dims =
  if hasTerm dst dims
  then Just $ mapPairs f dims
  else Nothing
  where
    f :: Dimension -> Int -> (Dimension, Int)
    f (Dimension id' mi') deg' = case dst of
      (DSTermDim id mi deg) ->
        if id == id' && mi == mi'
        then (Dimension id' mi', deg' - deg)
        else (Dimension id' mi', deg')
      (DSTermRange id _ _ deg) ->
        if id == id' && mi' /= Nothing
        then (Dimension id mi', deg' - deg)
        else (Dimension id' mi', deg')

hasTerm :: DimspecTerm -> Dimensionality -> Bool

-- Unbounded natural terms match any dimensionality where that name is present
-- with any index and with greater or equal absolute degree.
hasTerm (DSTermRange id mlow mhigh deg) dim = flip anyKey dim $ \case
  (Dimension _ Nothing) -> False
  d@(Dimension id' (Just idx)) ->
    id == id' &&
    inbounds &&
    (nestedDegrees deg $ fromJust $ lookupDegree dim d)
      where
        inbounds = case (mlow, mhigh) of
          (Nothing, Nothing) -> True
          (Just low, Nothing) -> idx >= low
          (Nothing, Just high) -> idx <= high
          (Just low, Just high) -> idx >= low && idx <= high

-- A concrete term matches any dimensionality where that dimension is
-- identically indexed and mapped to a greater or equal absolute degree.
hasTerm (DSTermDim id mi deg) dim = case lookupDegree dim (Dimension id mi) of
  Just deg' ->  nestedDegrees deg deg'
  Nothing -> False

-- Is the first degree of the same sign and of less than or equal absolute value
-- than the second
nestedDegrees :: Int -> Int -> Bool
nestedDegrees d1 d2 = (d1 > 0) == (d2 > 0) && abs d1 <= abs d2

normdimspecToDimensionality :: NormDimspecProd -> Maybe Dimensionality
normdimspecToDimensionality (NormDimspecProd dims@(_:_) [] [])
  = Just
  $ fromMap
  $ Map.fromList
  $ flip map dims
  $ \(DSTermDim id mi deg) -> (Dimension id mi, deg)
normdimspecToDimensionality _ = Nothing

-- If the given dimspec is monomorphically scalar (contains no vars, ranges nor
-- sums) then express it as a scalar dimensionality.
dimspecAsDimensionality :: Dimspec -> Maybe Dimensionality
dimspecAsDimensionality ds =
  if 1 == length products
  then normdimspecToDimensionality $ head products
  else Nothing
  where
    NormDimspec products = dsNormalize ds

-- Get sets of dimension identifiers in a dimspec as a pair of sets: the first
-- set being unindexed, the second set indexed.
getDimspecIdentifiers :: Dimspec -> (Set Identifier, Set Identifier)
getDimspecIdentifiers (DSTerm (DSTermDim id Nothing _))
  = (Set.singleton id, empty)
getDimspecIdentifiers (DSTerm (DSTermDim id (Just _) _))
  = (empty, Set.singleton id)
getDimspecIdentifiers (DSTerm (DSTermRange id _ _ _))
  = (empty, Set.singleton id)
getDimspecIdentifiers (DSTerm (DSTermVar id _)) = (empty, empty)
getDimspecIdentifiers (DSSum dss) = foldr1 union' $ map getDimspecIdentifiers dss
getDimspecIdentifiers (DSProduct dss) = foldr1 union' $ map getDimspecIdentifiers dss

union' :: Ord a => (Set a, Set a) -> (Set a, Set a) -> (Set a, Set a)
(s1, s2) `union'` (s3, s4) = (s1 `union` s3, s2 `union` s4)

dimspecExp :: Dimspec -> Int -> Dimspec
dimspecExp ds 0 = ds
dimspecExp (DSTerm (DSTermDim id mi deg)) r = DSTerm $ DSTermDim id mi $ deg * r
dimspecExp (DSTerm (DSTermRange id mlow mhigh deg)) r
  = DSTerm $ DSTermRange id mlow mhigh $ deg * r
dimspecExp (DSTerm (DSTermVar id deg)) r = DSTerm $ DSTermVar id $ deg * r
dimspecExp (DSProduct dss) r = DSProduct $ map (flip dimspecExp r) dss
dimspecExp ds@(DSSum dss) r =
  if r < 1
  then dimspecExp (dimspecInv ds) (0 - r)
  else DSProduct $ concat $ replicate r dss

dimspecInv :: Dimspec -> Dimspec
dimspecInv (DSTerm (DSTermDim id mi deg)) = DSTerm $ DSTermDim id mi $ 0 - deg
dimspecInv (DSTerm (DSTermRange id mlow mhigh deg))
  = DSTerm $ DSTermRange id mlow mhigh $ 0 - deg
dimspecInv (DSTerm (DSTermVar id deg)) = DSTerm $ DSTermVar id $ 0 - deg
dimspecInv (DSProduct dss) = DSProduct $ map dimspecInv dss
dimspecInv (DSSum dss) = DSSum $ map dimspecInv dss
