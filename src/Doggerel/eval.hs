module Doggerel.Eval (
    EvalFail,
    ScopeFrame(Frame),
    convertAsScalar,
    evaluate,
    initFrame,
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
    mapKeys,
    null,
    size
  )
import Data.Tuple (swap)

import Doggerel.DegreeMap
import Doggerel.Core
import Doggerel.Conversion
import Doggerel.Ast

type Dimensionality = DegreeMap String

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
    cdb = let (Frame _ _ cs _) = f
      in
        flip map cs $ \(source, dest, trans) ->
          Conversion trans (BaseUnit source) (BaseUnit dest)

-- Represents a lexical scope for runtime.
data ScopeFrame
  = Frame
      [Identifier]                                -- Dimensions
      [(Identifier, Maybe Identifier)]            -- Units
      [(Identifier, Identifier, Transformation)]  -- Conversions
      [(Identifier, ValueExpression)]             -- Assignments
  deriving Show

-- Get the dimensionality of the given base unit under the given scope.
getUnitDimensionality :: ScopeFrame -> BaseUnit -> Identifier
getUnitDimensionality (Frame _ us _ _) (BaseUnit u)
  = case find ((==u).fst) us of
    Nothing -> undefined    -- Was the unit undeclared in the scope frame?
                            -- Note: This should never happen. Maybe throw.
    Just (_, Nothing) -> u  -- The unit is declared, but has no dimension.
    Just (_, (Just d)) -> d

-- Get a dimensionality expression represnted by the given units within scope.
getDimensionality :: ScopeFrame -> Units -> Dimensionality
getDimensionality f = fromMap . (mapKeys $ getUnitDimensionality f) . getMap

-- Get the list of dimensionalities for each component of the given vector.
getVectorDimensionality :: ScopeFrame -> Vector -> [Dimensionality]
getVectorDimensionality f (Vector v) = map (getDimensionality f) $ keys v

-- An empty scope frame.
initFrame :: ScopeFrame
initFrame = Frame [] [] [] []

-- TODO: support expressions
-- TernaryOperatorApply,
-- UnaryOperatorApply

-- Add two vectors together. Matching units are combined. If conversions are
-- needed to make matching dimensionalities have matching units, convert before
-- using addV.
addV :: Vector -> Vector -> Vector
addV (Vector left) (Vector right)
  = Vector
  $ fromList
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

-- Find the dot product of two vectors. The first operand must be a single
-- component vector (a scalar in a vector type).
-- TODO: accept a scalar as the first operand.
dotProduct :: Vector -> Vector -> Vector
dotProduct (Vector left) (Vector right) = Vector $ fromList $ map add1 $ assocs right
  where
    [(lu, lq)] = assocs left
    add1 (ru, rq) = (multiply lu ru, lq * rq)

-- Reverse the sign of every component of the given vector.
negateV :: Vector -> Vector
negateV (Vector m) = Vector $ fmap (0-) m

-- Compute a vector where each component is the reciprocal of the given vector's
-- components.
invertV :: Vector -> Vector
invertV (Vector m) = Vector $ fromList $ map invert1 $ assocs m
  where
    invert1 (u, q) = (invert u, 1 / q)

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
getCancellationTargetUnits f l r = fromMap $ fromList $ matchedRight
  where
    -- For a units expression, find the list of tuples representing:
    -- - the BaseUnit,
    -- - the degree of the BaseUnit in the expression, and
    -- - the dimensionality of that BaseUnit
    unitDimensions :: Units -> [(BaseUnit, Int, String)]
    unitDimensions u
      = map (\(bu, deg) -> (bu, deg, getUnitDimensionality f bu))
      $ assocs
      $ getMap u
    lps = unitDimensions l
    rps = unitDimensions r

    -- Whether the given two tuples are in the same dimension.
    sameDim :: (BaseUnit, Int, String) -> (BaseUnit, Int, String) -> Bool
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
convertRightOperandForSum f (Vector left) (Vector right)
  = Vector
  $ fromList $ map convertIfMatching $ assocs right
  where
    leftDims :: [(Units, Dimensionality)]
    leftDims = map (\u -> (u, getDimensionality f u)) $ keys left

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
  $ fromList
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
  deriving (Eq, Show)

-- Evaluate the given value expression to either a resulting vector or to am
-- evaluation failure value.
evaluate :: ScopeFrame -> ValueExpression -> Either EvalFail Vector
evaluate _ (ScalarLiteral s) = return $ scalarToVector s
evaluate f@(Frame _ _ _ as) (Reference id) = case find ((==id).fst) as of
  Just (_, expr) -> evaluate f expr
evaluate f (BinaryOperatorApply Add e1 e2) = do
  r1 <- evaluate f e1
  r2 <- evaluate f e2
  let r2' = convertRightOperandForSum f r1 r2
  return $ r1 `addV` r2'
evaluate f (BinaryOperatorApply Subtract e1 e2) = do
  r1 <- evaluate f e1
  r2 <- evaluate f e2
  let r2' = convertRightOperandForSum f r1 r2
  return $ r1 `addV` (negateV r2)
evaluate f (BinaryOperatorApply Multiply e1 e2) = do
  r1 <- evaluate f e1
  r2 <- evaluate f e2

  -- If it's not a dot product, fail.
  case getSingleUnits r1 of
    Just u1 -> return $ dotProduct r1 $ convertRightOperandForProduct f u1 r2
    Nothing -> case getSingleUnits r2 of
      Just u2 -> return $ dotProduct r2 $ convertRightOperandForProduct f u2 r1
      Nothing -> Left EvalFailCrossProduct
