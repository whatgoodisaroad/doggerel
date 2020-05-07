module Eval (
    ScopeFrame(Frame),
    bestSequence,
    convertWithAnnotations,
    initFrame,
    evaluate,
    executeConversion,
    unifyVector,
    getVectorDimensionality
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
  -> DegreeMap BaseUnit
  -> Maybe (Scalar, [String])
convertWithAnnotations cdb (Scalar magnitude source) dest = do
  sequence <- bestSequence $ findConversions cdb dest source
  let magnitude' = executeConversion sequence magnitude
  return $ (Scalar magnitude' dest, map transformToAnnotation sequence)

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
dotProduct = undefined


negateV :: Vector -> Vector
negateV (Vector m) = Vector $ fmap (0-) m

invertV :: Vector -> Vector
invertV (Vector m) = Vector $ fromList $ map invert1 $ assocs m
  where
    invert1 (u, q) = (invert u, 1 / q)

isSingleVector :: Vector -> Bool
isSingleVector (Vector m) = 1 == size m

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
  return $ r1 `addV` r2
evaluate f (BinaryOperatorApply Subtract e1 e2) = do
  r1 <- evaluate f e1
  r2 <- evaluate f e2
  return $ r1 `addV` (negateV r2)
evaluate f (BinaryOperatorApply Multiply e1 e2) = do
  r1 <- evaluate f e1
  r2 <- evaluate f e2
  let ur1 = unifyVector f r1
  let ur2 = unifyVector f r2
  if isSingleVector ur1
    then return $ dotProduct ur1 ur2
    else if isSingleVector ur2
      then return $ dotProduct ur2 ur1
      else Left EvalFailCrossProduct

-- For testing

u :: String -> DegreeMap BaseUnit
u = toMap . BaseUnit