module Doggerel.Eval (
    EvalFail(
      EvalFailCrossProduct,
      DivideByZero
    ),
    convertAsScalar,
    evaluate,
    getDimensionality,
    getVectorDimensionality
  )
  where

import Data.List (find, intersperse, sortBy)
import Data.Map.Strict as Map (
    Map,
    assocs,
    empty,
    fromList,
    insert,
    keys,
    lookup,
    mapKeys,
    null,
    size
  )
import Data.Set as Set (Set, fromList, toList)
import Data.Tuple (swap)
import Doggerel.Ast
import Doggerel.Conversion
import Doggerel.Core
import Doggerel.DegreeMap
import Doggerel.Scope

-- Utility to wrap a maybe in such a way that its easy to use in an either monad
-- context.
maybeToEither :: b -> Maybe a -> Either b a
maybeToEither b m = case m of
  Nothing -> Left b
  Just a -> Right a

-- Given an expression of just quantity values, evaluate it to a resulting
-- quantity. This disregards references or functions.
evaluateQuantityExpr :: ValueExpression () Quantity -> Quantity
evaluateQuantityExpr (Literal q) = q
evaluateQuantityExpr (UnaryOperatorApply Negative e) = -(evaluateQuantityExpr e)
evaluateQuantityExpr (BinaryOperatorApply op e1 e2)
  = (binOpToFn op) (evaluateQuantityExpr e1) (evaluateQuantityExpr e2)

-- Map BinaryOperator values to their respective math operator function.
binOpToFn :: BinaryOperator -> Quantity -> Quantity -> Quantity
binOpToFn Add = (+)
binOpToFn Subtract = (-)
binOpToFn Multiply = (*)
binOpToFn Divide = (/)

-- Given a vector and an expression with unit references and quantity literals,
-- substitute the vector components that match those units into the references.
-- The result is an expression with no references. If units are not represented
-- by any component of the vector, then zero is used.
substituteUnits ::
     Vector
  -> ValueExpression Units Quantity
  -> ValueExpression () Quantity
substituteUnits _ (Literal q) = Literal q
substituteUnits vec (Reference us) = Literal $ getComponent vec us
substituteUnits vec (UnaryOperatorApply op expr)
  = UnaryOperatorApply op $ substituteUnits vec expr
substituteUnits vec (BinaryOperatorApply op e1 e2)
  = BinaryOperatorApply op (substituteUnits vec e1) (substituteUnits vec e2)

-- Apply the given transformation to the given quantity.
executeTransform :: Transformation -> Quantity -> Quantity
executeTransform Inversion x = 1 / x
executeTransform (LinearTransform f) x = f * x
executeTransform (AffineTransForm m b) x = m * x + b
executeTransform (InverseOf Inversion) x = x
executeTransform (InverseOf (LinearTransform f)) x = x / f
executeTransform (InverseOf (AffineTransForm m b)) x = (x - b) / m

-- Execute the given list of transformations in order to the given quantity.
executeConversion :: [Transformation] -> Quantity -> Quantity
executeConversion = flip $ foldr executeTransform

-- Convert a scalar to the desired units if a conversion sequence can be found
-- in the given database of conversions.
convert :: [Conversion] -> Scalar -> Units -> Maybe Scalar
convert cdb (Scalar magnitude source) dest = do
  sequence <- findConversions cdb dest source
  let magnitude' = executeConversion sequence magnitude
  return $ Scalar magnitude' dest

-- Convert a scalar to the desired units if a conversion sequence can be found
-- in the given scope frame.
convertInScope :: ScopeFrame -> Scalar -> Units -> Maybe Scalar
convertInScope f s t = convert cdb s t
  where
    cdb
      = flip map (getConversions f) $ \(source, dest, trans) ->
        Conversion trans (BaseUnit source) (BaseUnit dest)

-- Get the dimensionality of the given base unit under the given scope.
getUnitDimensionality :: ScopeFrame -> BaseUnit -> Dimension
getUnitDimensionality f (BaseUnit u)
  = case find ((==u).fst) (getUnits f) of
    Nothing -> undefined                -- Was the unit undeclared in the scope
                                        -- frame?  Note: This should never
                                        -- happen. Maybe throw.
    Just (_, Nothing)  -> Dimension u   -- The unit is declared, but has no
                                        -- dimension.
    Just (_, (Just d)) -> Dimension d

-- Get a dimensionality expression represnted by the given units within scope.
getDimensionality :: ScopeFrame -> Units -> Dimensionality
getDimensionality f = fromMap . (mapKeys $ getUnitDimensionality f) . getMap

-- Get the list of dimensionalities for each component of the given vector.
getVectorDimensionality :: ScopeFrame -> Vector -> VectorDimensionality
getVectorDimensionality f (Vector v)
  = VecDims $ Set.fromList $ map (getDimensionality f) $ keys v

-- TODO: support expressions
-- TernaryOperatorApply,
-- UnaryOperatorApply

-- Add two vectors together. Matching units are combined. If conversions are
-- needed to make matching dimensionalities have matching units, convert before
-- using addV.
addV :: Vector -> Vector -> Vector
addV (Vector left) (Vector right)
  = Vector
  $ Map.fromList
  $ combineAll (assocs left) (assocs right)
  where
    combineAll ::
         [(Units, Quantity)]
      -> [(Units, Quantity)]
      -> [(Units, Quantity)]
    combineAll [] r = r
    combineAll l [] = l
    combineAll (l:ls) rs = combineAll ls $ addPair l rs

    addPair ::
         (Units, Quantity)
      -> [(Units, Quantity)]
      -> [(Units, Quantity)]
    addPair l [] = [l]
    addPair l@(lu, lq) (r@(ru, rq):rs)
      = if lu == ru
        then (lu, lq + rq):rs
        else if (lu == invert ru)
          then (lu, lq + (1/rq)):rs
          else r:(addPair l rs)

-- Find the dot product of a scalar with a vector.
dotProduct :: Scalar -> Vector -> Vector
dotProduct (Scalar lq lu) (Vector right)
  = Vector $ Map.fromList $ map add1 $ assocs right
  where
    add1 (ru, rq) = (multiply lu ru, lq * rq)

-- Reverse the sign of every component of the given vector.
negateV :: Vector -> Vector
negateV (Vector m) = Vector $ fmap (0-) m

-- Compute a vector where each component is the reciprocal of the given vector's
-- components.
invertV :: Vector -> Maybe Vector
invertV v@(Vector m) = if anyComponentZero v
  then Nothing
  else Just $ Vector $ Map.fromList $ map invert1 $ assocs m
  where
    invert1 (u, q) = (invert u, 1 / q)

anyComponentZero :: Vector -> Bool
anyComponentZero (Vector m) = any ((==0).snd) $ assocs m

-- Whether the given vector have exactly one component.
isSingleVector :: Vector -> Bool
isSingleVector (Vector m) = 1 == size m

-- If the given vector has one component, give its units, otherwise nothing.
getSingleUnits :: Vector -> Maybe Units
getSingleUnits = fmap getScalarUnits . vectorAsScalar

-- If the given vector has only one component, then give it as a scalar.
vectorAsScalar :: Vector -> Maybe Scalar
vectorAsScalar v@(Vector m) = if isSingleVector v
  then Just $ uncurry Scalar $ swap $ head $ assocs m
  else Nothing

convertAsScalar :: ScopeFrame -> Vector -> Units -> Maybe Vector
convertAsScalar f v u = do
  s <- vectorAsScalar v
  s' <- convertInScope f s u
  return $ scalarToVector s'

-- Find units for optimal cancellation in a binary operation.
--
-- Under the given scope and two units expressions for the left and right
-- operands of the operation respectively, find an alternative (and
-- theoretically convertable) units expression for the right operanwd that best
-- matches the left units.
--
-- For example if minute and second are both of dimension time, then with
-- left = meters/second and right = minutes/kilowatt, the result would be
-- seconds/kilowatt.
getCancellationTargetUnits :: ScopeFrame -> Units -> Units -> Units
getCancellationTargetUnits f l r = fromMap $ Map.fromList $ matchedRight
  where
    -- For a units expression, find the list of tuples representing:
    -- - the BaseUnit,
    -- - the degree of the BaseUnit in the expression, and
    -- - the dimensionality of that BaseUnit
    unitDimensions :: Units -> [(BaseUnit, Int, Dimension)]
    unitDimensions u
      = map (\(bu, deg) -> (bu, deg, getUnitDimensionality f bu))
      $ assocs
      $ getMap u
    lps = unitDimensions l
    rps = unitDimensions r

    -- Whether the given two tuples are in the same dimension.
    sameDim :: (BaseUnit, Int, Dimension) -> (BaseUnit, Int, Dimension) -> Bool
    sameDim (_, _, ldim) (_, _, rdim) = ldim == rdim

    -- A list of pairs representing the modified right units expression but with
    -- each unit swapped with the corresponding left unit of the same
    -- dimensionality.
    matchedRight :: [(BaseUnit, Int)]
    matchedRight
      = flip map rps
      $ \rp@(ru, rdeg, rdim) ->
        case find (sameDim rp) lps of
          Nothing         -> (ru, rdeg)
          Just (lu, _, _) -> (lu, rdeg)

-- Under the given scope, and with the given left hand operand units, attempt to
-- convert the given scalar to the cancellation units.
convertForCancellation :: ScopeFrame -> Units -> Scalar -> Maybe Scalar
convertForCancellation f lu rs@(Scalar _ ru)
  = convertInScope f rs $ getCancellationTargetUnits f lu ru

-- Given a scalar, decompose it to a tuple of its units and its quantity.
toScalarPair :: Scalar -> (Units, Quantity)
toScalarPair (Scalar q u) = (u, q)

-- For any two vector components to be added together, they must have identical
-- units expressions.
--
-- With convertRightOperandForSum given scope and a left hand vector and a right
-- hand vector, convert the right hand vector such that as many components have
-- identical units to some left hand component as possible.
convertRightOperandForSum :: ScopeFrame -> Vector -> Vector -> Vector
convertRightOperandForSum f (Vector ml) vr = convertToTargetUnits f (keys ml) vr

-- Best effort convert the vector to the given target units.
convertToTargetUnits :: ScopeFrame -> [Units] -> Vector -> Vector
convertToTargetUnits f leftUnits (Vector right)
  = Vector
  $ Map.fromList $ map convertIfMatching $ assocs right
  where
    leftDims :: [(Units, Dimensionality)]
    leftDims = map (\u -> (u, getDimensionality f u)) leftUnits

    -- Given a component of the right hand vector, if there is a compatible
    -- component of the left hand vector (where compatible means same
    -- dimensionality or reciprocal dimensionality), convert the left hand
    -- component. Leave it unchanged if no candidates are found.
    convertIfMatching :: (Units, Quantity) -> (Units, Quantity)
    convertIfMatching noChange@(us, q) = case (match, match') of
      (Just lus, _)       -> convertDirect  lus   `orElse` noChange
      (_, Just lus')      -> convertInverse lus'  `orElse` noChange
      (Nothing, Nothing)  -> noChange
      where
        -- The dimensionality and reciprocal dimensionality of the current right
        -- hand operand component.
        dims, dims' :: Dimensionality
        dims = getDimensionality f us
        dims' = invert dims

        -- Given the units of the current right hand component, the value of
        -- match represents the units of the left hand component with matching
        -- dimensionality if present.
        -- The value of match' is the same if the left hand units are inverted.
        match, match' :: Maybe Units
        match   = fmap fst $ find ((==dims).snd) leftDims
        match'  = fmap fst $ find ((==dims').snd) leftDims

        -- convertDirect attempts to convert the current component to the given
        -- target units. convertInverse does the same with the reciprocal of the
        -- current component.
        convertDirect, convertInverse :: Units -> Maybe (Units, Quantity)
        convertDirect target
          = fmap toScalarPair
          $ convertForCancellation f target
          $ Scalar q us
        convertInverse target
          = fmap toScalarPair
          $ convertForCancellation f target
          $ Scalar (1/q) (invert us)

-- Convert like, convertToTargetUnits, but with failure if the exact target is
-- not achieved.
convertToExactly :: ScopeFrame -> Set Units -> Vector -> Maybe Vector
convertToExactly f us vec = if convertedUnits == us
  then Just converted
  else Nothing
  where
    convertedUnits = Set.fromList $ keys convertedMap
    converted@(Vector convertedMap) = convertToTargetUnits f (Set.toList us) vec

-- For any two vector components to be multiplied together, their unit
-- expression components of the same dimensionality should be of the same base
-- unit. Such matching allows for degrees to accumulate or cancel properly.
--
-- This method only permits the left hand side to be scalar.
--
-- Given the units of the left hand component, convert the components of the
-- right hand vector such that matching base unit dimensionalities have matching
-- base units.
convertRightOperandForProduct :: ScopeFrame -> Units -> Vector -> Vector
convertRightOperandForProduct f target (Vector right)
  = Vector
  $ Map.fromList
  $ map (\p -> convert1 p `orElse` p)
  $ assocs right
  where
    convert1 :: (Units, Quantity) -> Maybe (Units, Quantity)
    convert1 (u, q)
      = fmap toScalarPair
      $ convertForCancellation f target
      $ Scalar q u

data EvalFail
  = EvalFailCrossProduct
  | DivideByZero
  | InternalError String
  | UnsatisfiableArgument String
  deriving (Eq, Show)

-- Find the product of two vectors in scope.
-- Only the dot-product is supported currently, so at least one operand must
-- have exactly one component, otherwise the evaluation will fail as a
-- cross-product.
evalVectorProduct :: ScopeFrame -> Vector -> Vector -> Either EvalFail Vector
evalVectorProduct f r1 r2 = case (vectorAsScalar r1, vectorAsScalar r2) of
  -- Construct a dot product based on which operand is scalar.
  (Just s1, _)  -> return
    $ dotProduct s1
    $ convertRightOperandForProduct f (getScalarUnits s1) r2
  (_, Just s2)  -> return
    $ dotProduct s2
    $ convertRightOperandForProduct f (getScalarUnits s2) r1
  -- If it's an unsupported cross product, fail.
  _ -> Left EvalFailCrossProduct

-- Given a set of units, map them to a set of dimensionality.
relationKeyToVectorDims :: ScopeFrame -> Set Units -> VectorDimensionality
relationKeyToVectorDims f
  = VecDims
  . Set.fromList
  . (fmap (getDimensionality f))
  . Set.toList

-- Given a scope frame, a map representation of a relation and the input vector
-- to the relation, evaluate the ressulting vector.
evalRelation ::
     ScopeFrame
  -> Map (Set Units) (Units, ValueExpression Units Quantity)
  -> Vector
  -> Either EvalFail Vector
evalRelation f relMap vec = do
  -- For every input set of the mapping, pair it with its dimensionality.
  let relMapKeyDims = map (\k -> (k, relationKeyToVectorDims f k)) $ keys relMap

  -- Find the dimensionality of the vector.
  let argDims = getVectorDimensionality f vec

  -- Find the units expression of the input set with dimensionality that matches
  -- the vector.
  (units, _) <- maybeToEither badDimMatch $ find ((==argDims).snd) relMapKeyDims

  -- Pull the resulting units and expression for this input set.
  let (Just (resultingUnits, expr)) = Map.lookup units relMap

  -- Convert the vector to those units.
  vec' <- maybeToEither failedConvert $ convertToExactly f units vec

  -- Insert the vector components into the expression and resolve it to a
  -- quantity.
  let q = evaluateQuantityExpr $ substituteUnits vec' expr

  return $ scalarToVector $ Scalar q resultingUnits
  where
    badDimMatch = UnsatisfiableArgument "Cannot match dims to arg"
    failedConvert = UnsatisfiableArgument "Cannot convert to arg units"

-- Evaluate the given value expression to either a resulting vector or to an
-- evaluation failure value.
evaluate :: ScopeFrame -> Expr -> Either EvalFail Vector
evaluate _ (Literal s) = return $ scalarToVector s
evaluate f (Reference id)
  = case find ((==id).getAssignmentId) (getAssignments f) of
    Just (_, _, value) -> Right value
    Nothing -> case find ((==id).getInputId) (getInputs f) of
      Just (_, Right s) -> Right $ scalarToVector s
      _ -> Left $ InternalError "Can't resolve ref. This shouldn't happen."
evaluate f (BinaryOperatorApply Add e1 e2) = do
  r1 <- evaluate f e1
  r2 <- evaluate f e2
  let r2' = convertRightOperandForSum f r1 r2
  return $ r1 `addV` r2'
evaluate f (BinaryOperatorApply Subtract e1 e2) = do
  r1 <- evaluate f e1
  r2 <- evaluate f e2
  let r2' = convertRightOperandForSum f r1 r2
  return $ r1 `addV` (negateV r2')
evaluate f (BinaryOperatorApply Multiply e1 e2) = do
  r1 <- evaluate f e1
  r2 <- evaluate f e2
  evalVectorProduct f r1 r2
evaluate f (BinaryOperatorApply Divide e1 e2) = do
  r1 <- evaluate f e1
  r2 <- evaluate f e2
  case invertV r2 of
    Just r2' -> evalVectorProduct f r1 r2'
    Nothing -> Left DivideByZero
evaluate f (FunctionApply id argExpr)
  = case find ((==id).getRelationId) (getRelations f) of
    Nothing -> Left $ InternalError "Can't resolve rel. This shouldn't happen."
    Just (_, relMap) -> evaluate f argExpr >>= evalRelation f relMap
