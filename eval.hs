module Eval (
    ScopeFrame(Frame),
    bestSequence,
    convertWithAnnotations,
    initFrame,
    evaluate,
    executeConversion,
    unifyVector,
    getVectorDimensionality,
    convertForCancellation
  )
  where

import Core
import Ast

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

data Vector = Vector (Map Units Quantity)

instance Show Vector where
  show (Vector m) = if Map.null m then "Ø" else "{" ++ vals ++ "}"
    where
      vals
        = concat
        $ intersperse ", "
        $ map (\(u, q) -> (show $ Scalar q u))
        $ assocs m

executeTransform :: Transformation -> Quantity -> Quantity
executeTransform Inversion x = 1 / x
executeTransform (LinearTransform _ f) x = f * x
executeTransform (AffineTransForm _ m b) x = m * x + b
executeTransform (InverseOf Inversion) x = x
executeTransform (InverseOf (LinearTransform _ f)) x = x / f
executeTransform (InverseOf (AffineTransForm _ m b)) x = (x - b) / m

executeConversion :: [Transformation] -> Quantity -> Quantity
executeConversion = flip $ foldr executeTransform

bestSequence :: [[Transformation]] -> Maybe [Transformation]
bestSequence [] = Nothing
bestSequence ts
  = Just
  $ head
  $ sortBy (\a b -> length a `compare` length b)
  $ reverse
  $ ts

convertTo :: [Conversion] -> Scalar -> DegreeMap BaseUnit -> Maybe Scalar
convertTo cdb scalar dest
  = (fmap fst)
  $ convertWithAnnotations cdb scalar dest

convertWithAnnotations ::
     [Conversion]
  -> Scalar
  -> Units
  -> Maybe (Scalar, [String])
convertWithAnnotations cdb (Scalar magnitude source) dest = do
  sequence <- bestSequence $ findConversions cdb dest source
  let magnitude' = executeConversion sequence magnitude
  return $ (Scalar magnitude' dest, map transformToAnnotation sequence)

convertInScope :: ScopeFrame -> Scalar -> Units -> Maybe Scalar
convertInScope f s t = fmap fst $ convertWithAnnotations cdb s t
  where
    cdb = let (Frame _ _ cs _) = f
      in
        flip map cs $ \(source, dest, trans) ->
          Conversion trans (BaseUnit source) (BaseUnit dest)


transformToAnnotation :: Transformation -> String
transformToAnnotation Inversion = "Inverted"
transformToAnnotation (LinearTransform label factor) =
     "Conversion for " ++ label
  ++ ", multiply by " ++ show factor
transformToAnnotation (AffineTransForm label m b) =
     "Conversion for " ++ label ++ ", "
  ++ (if m == 1 then "" else "multiply by " ++ show m ++ " and ")
  ++ "add " ++ show b
transformToAnnotation (InverseOf (LinearTransform label factor)) =
     "Inverse conversion for " ++ label
  ++ ", divide by " ++ show factor
transformToAnnotation (InverseOf (AffineTransForm label m b)) =
     "Inverse conversion for " ++ label
  ++ ", subtract by " ++ show b
  ++ if m == 1 then "" else " and divide by " ++ show m

type Dimensionality = DegreeMap String

scalarToVector :: Scalar -> Vector
scalarToVector (Scalar q u) = Vector $ insert u q empty

data ScopeFrame
  = Frame
      [Identifier]                                -- Dimensions
      [(Identifier, Maybe Identifier)]            -- Units
      [(Identifier, Identifier, Transformation)]  -- Conversions
      [(Identifier, ValueExpression)]             -- Assignments
  deriving Show

getUnitDimensionality :: ScopeFrame -> BaseUnit -> Identifier
getUnitDimensionality (Frame _ us _ _) (BaseUnit u)
  = case find ((==u).fst) us of
    Nothing -> u -- This should never happen
    Just (_, Nothing) -> u
    Just (_, (Just d)) -> d

getDimensionality :: ScopeFrame -> Units -> Dimensionality
getDimensionality f = fromMap . (mapKeys $ getUnitDimensionality f) . getMap

getVectorDimensionality :: ScopeFrame -> Vector -> [Dimensionality]
getVectorDimensionality f (Vector v) = map (getDimensionality f) $ keys v

initFrame :: ScopeFrame
initFrame = Frame [] [] [] []

-- BinaryOperatorApply,
-- ScalarLiteral,

-- Reference,
-- TernaryOperatorApply,
-- UnaryOperatorApply

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

dotProduct :: Vector -> Vector -> Vector
dotProduct (Vector left) (Vector right) = Vector $ fromList $ map add1 $ assocs right
  where
    [(lu, lq)] = assocs left
    add1 (ru, rq) = (multiply lu ru, lq * rq)


negateV :: Vector -> Vector
negateV (Vector m) = Vector $ fmap (0-) m

invertV :: Vector -> Vector
invertV (Vector m) = Vector $ fromList $ map invert1 $ assocs m
  where
    invert1 (u, q) = (invert u, 1 / q)

isSingleVector :: Vector -> Bool
isSingleVector (Vector m) = 1 == size m

-- If the given vector has one component, give its units, otherwise nothing.
getSingleUnits :: Vector -> Maybe Units
getSingleUnits = fmap getScalarUnits . vectorAsScalar

vectorAsScalar :: Vector -> Maybe Scalar
vectorAsScalar v@(Vector m) = if isSingleVector v
  then Just $ uncurry Scalar $ swap $ head $ assocs m
  else Nothing

-- Unify identical dimensionalities within a vector
unifyVector :: ScopeFrame -> Vector -> Vector
unifyVector f@(Frame _ _ cs _) (Vector v) = Vector $ fromList $ unifyA $ assocs v
  where
    unifyA :: [(Units, Quantity)] -> [(Units, Quantity)]
    unifyA [] = []
    unifyA (p:ps) = unify1 p $ unifyA ps

    unify1 :: (Units, Quantity) -> [(Units, Quantity)] -> [(Units, Quantity)]
    unify1 p1 [] = [p1]
    unify1 p1@(u1, q1) (p2@(u2, q2):ps) = if d1 /= d2 && d1 /= d2'
      then recurse
      else case convertTo cdb (Scalar q2 u2) u1 of
        Just (Scalar q2' _) -> unify1 (u1, q1 + q2') ps
        Nothing -> recurse
      where
        d1 = getDimensionality f u1
        d2 = getDimensionality f u2
        d2' = invert d2
        cdb = map (\(s, d, t) -> Conversion t (BaseUnit s) (BaseUnit d)) cs
        recurse = p2:(unify1 p1 ps)



getCancellationTargetUnits :: ScopeFrame -> Units -> Units -> Units
getCancellationTargetUnits f l r = fromMap $ fromList $ matchedRight
  where
    unitDimensions :: Units -> [(BaseUnit, Int, String)]
    unitDimensions u
      = map (\(bu, deg) -> (bu, deg, getUnitDimensionality f bu))
      $ assocs
      $ getMap u
    lps = unitDimensions l
    rps = unitDimensions r

    sameDim :: (BaseUnit, Int, String) -> (BaseUnit, Int, String) -> Bool
    sameDim (_, _, ldim) (_, _, rdim) = ldim == rdim

    matchedRight :: [(BaseUnit, Int)]
    matchedRight
      = flip map rps
      $ \rp@(ru, rdeg, rdim) ->
        case find (sameDim rp) lps of
          Nothing         -> (ru, rdeg)
          Just (lu, _, _) -> (lu, rdeg)

convertForCancellation :: ScopeFrame -> Units -> Scalar -> Maybe Scalar
convertForCancellation f lu rs@(Scalar _ ru)
  = convertInScope f rs $ getCancellationTargetUnits f lu ru

orElse :: Maybe a -> a -> a
orElse (Just a) _ = a
orElse _ a = a

toScalarPair :: Scalar -> (Units, Quantity)
toScalarPair (Scalar q u) = (u, q)

-- For each component of the second vector with dimensionality matching any
-- component of the first vector, convert the right component to be in units of
-- that left component.
convertRightOperandForSum :: ScopeFrame -> Vector -> Vector -> Vector
convertRightOperandForSum f (Vector left) (Vector right)
  = Vector
  $ fromList convertedRightAssocs
  where
    leftDims :: [(Units, Dimensionality)]
    leftDims = map (\u -> (u, getDimensionality f u)) $ keys left

    convertedRightAssocs :: [(Units, Quantity)]
    convertedRightAssocs = map convertIfMatching $ assocs right

    convertIfMatching :: (Units, Quantity) -> (Units, Quantity)
    convertIfMatching (us, q) = case (match, match') of
      (Just lus, _) -> convertDirect lus `orElse` noChange
      (_, Just lus') -> convertInverse lus' `orElse` noChange
      (Nothing, Nothing) -> noChange
      where
        dims, dims' :: Dimensionality
        dims = getDimensionality f us
        dims' = invert dims

        match, match' :: Maybe Units
        match   = fmap fst $ find ((==dims).snd) leftDims
        match'  = fmap fst $ find ((==dims').snd) leftDims

        noChange = (us, q)

        convertDirect, convertInverse :: Units -> Maybe (Units, Quantity)
        convertDirect target
          = fmap toScalarPair
          $ convertForCancellation f target
          $ Scalar q us
        convertInverse target
          = fmap toScalarPair
          $ convertForCancellation f target
          $ Scalar (1/q) (invert us)


convertRightOperandForProduct :: ScopeFrame -> Units -> Vector -> Vector
convertRightOperandForProduct f target (Vector right)
  = Vector
  $ fromList convertedRightAssocs
  where
    convertedRightAssocs :: [(Units, Quantity)]
    convertedRightAssocs
      = map (\p -> convert1 p `orElse` p)
      $ assocs right

    convert1 :: (Units, Quantity) -> Maybe (Units, Quantity)
    convert1 (u, q)
      = fmap toScalarPair
      $ convertForCancellation f target
      $ Scalar q u

data EvalFail
  = EvalFailCrossProduct
  deriving Show

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

-- For testing

u :: String -> DegreeMap BaseUnit
u = toMap . BaseUnit