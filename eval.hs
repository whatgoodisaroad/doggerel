module Eval (
    ScopeFrame(Frame),
    initFrame,
    evaluate
  )
  where

import Core
import Ast

import Data.List (find, intersperse)
import Data.Map.Strict as Map (
    Map,
    assocs,
    empty,
    fromList,
    insert,
    mapKeys,
    null
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

multV :: Vector -> Vector -> Vector
multV = undefined

negateV :: Vector -> Vector
negateV (Vector m) = Vector $ fmap (0-) m

data EvalFail = EvalFail
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
  return $ r1 `multV` r2


-- For testing

u :: String -> DegreeMap BaseUnit
u = toMap . BaseUnit