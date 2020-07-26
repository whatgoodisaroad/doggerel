module Doggerel.Relation (
    allRefsAreUnique,
    asVectorMap,
    solveFor
  ) where

import Data.List (nub)
import Data.Map.Strict as Map
import Data.Set as Set (Set, fromList)
import Doggerel.Ast
import Doggerel.Core

solveFor ::
     Eq ref
  => ValueExpression ref lit
  -> ValueExpression ref lit
  -> ref
  -> Maybe (ValueExpression ref lit)
solveFor e1 e2 id = case (idInE1, idInE2) of
  (True, False) -> solveForLeft e1 e2 id
  (False, True) -> solveForLeft e2 e1 id
  _ -> Nothing
  where
    idInE1 = id `elem` referencesOfExpr e1
    idInE2 = id `elem` referencesOfExpr e2

allRefsAreUnique ::
     Eq ref
  => ValueExpression ref lit
  -> ValueExpression ref lit
  -> Bool
allRefsAreUnique e1 e2 = length fullList == (length $ nub fullList)
  where
    fullList = referencesOfExpr e1 ++ referencesOfExpr e2

solveForLeft ::
     Eq ref
  => ValueExpression ref lit
  -> ValueExpression ref lit
  -> ref
  -> Maybe (ValueExpression ref lit)
solveForLeft (Reference id') e2 id = if id == id' then Just e2 else Nothing
solveForLeft (UnaryOperatorApply Negative e1) e2 id
  = solveForLeft e1 (UnaryOperatorApply Negative e2) id
solveForLeft (UnaryOperatorApply (Exponent radix) e1) e2 id
  = solveForLeft e1 (UnaryOperatorApply (Exponent (1/radix)) e2) id
solveForLeft (BinaryOperatorApply Add e1_1 e1_2) e2 id
  = case (idInE1_1, idInE1_2) of
    (True, False) -> solveForLeft e1_1 (BinaryOperatorApply Subtract e2 e1_2) id
    (False, True) -> solveForLeft e1_2 (BinaryOperatorApply Subtract e2 e1_1) id
    _ -> Nothing
    where
      idInE1_1 = id `elem` referencesOfExpr e1_1
      idInE1_2 = id `elem` referencesOfExpr e1_2
solveForLeft (BinaryOperatorApply Subtract e1_1 e1_2) e2 id
  = case (idInE1_1, idInE1_2) of
    (True, False) -> solveForLeft e1_1 (BinaryOperatorApply Add e2 e1_2) id
    (False, True) -> solveForLeft e1_2 (BinaryOperatorApply Subtract e1_1 e2) id
    _ -> Nothing
    where
      idInE1_1 = id `elem` referencesOfExpr e1_1
      idInE1_2 = id `elem` referencesOfExpr e1_2
solveForLeft (BinaryOperatorApply Multiply e1_1 e1_2) e2 id
  = case (idInE1_1, idInE1_2) of
    (True, False) -> solveForLeft e1_1 (BinaryOperatorApply Divide e2 e1_2) id
    (False, True) -> solveForLeft e1_2 (BinaryOperatorApply Divide e2 e1_1) id
    _ -> Nothing
    where
      idInE1_1 = id `elem` referencesOfExpr e1_1
      idInE1_2 = id `elem` referencesOfExpr e1_2
solveForLeft (BinaryOperatorApply Divide e1_1 e1_2) e2 id
  = case (idInE1_1, idInE1_2) of
    (True, False) -> solveForLeft e1_1 (BinaryOperatorApply Multiply e2 e1_2) id
    (False, True) -> solveForLeft e1_2 (BinaryOperatorApply Divide e1_1 e2) id
    _ -> Nothing
    where
      idInE1_1 = id `elem` referencesOfExpr e1_1
      idInE1_2 = id `elem` referencesOfExpr e1_2

-- Given a pair of expressions representing a relation which together span n
-- units expressions, give a map where, for each unit expression u, if it can be
-- isolated, the key is the n-1 other units, and the mapped value is the
-- expression across those units having isolated u.
asVectorMap ::
     (Eq ref, Ord ref)
  => ValueExpression ref lit
  -> ValueExpression ref lit
  -> Map (Set ref) (ref, ValueExpression ref lit)
asVectorMap e1 e2
  = Map.fromList
  $ Prelude.map (\(args, us, Just expr) -> (args, (us, expr)))
  $ Prelude.filter (\p -> case p of {(_, _, Nothing)  -> False; _ -> True; })
  $ Prelude.map (\us -> (excluding us, us, solveFor e1 e2 us)) allRefs
  where
    excluding us = Set.fromList $ Prelude.filter (/=us) allRefs
    allRefs = referencesOfExpr e1 ++ referencesOfExpr e2
