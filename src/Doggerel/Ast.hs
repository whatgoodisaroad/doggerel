{-# LANGUAGE FlexibleInstances #-}

module Doggerel.Ast (
    AssignmentOption(..),
    BinaryOperator(
      Add,
      Divide,
      Multiply,
      Subtract
    ),
    Expr,
    Identifier,
    PrintOption(..),
    Program,
    Statement(
      Assignment,
      DeclareDimension,
      DeclareConversion,
      DeclareUnit,
      Input,
      Print,
      Comment
    ),
    Units,
    UnaryOperator(
      Negative
    ),
    ValueExpression(
      BinaryOperatorApply,
      Reference,
      ScalarLiteral,
      UnaryOperatorApply
    ),
    referencesOfExpr,
    unitsOfExpr
  ) where

import Data.Map.Strict (keys)
import Doggerel.Core
import Doggerel.Conversion
import Doggerel.DegreeMap (getMap)
import Data.List (nub)
import Data.Set (Set)

type Identifier = String

data UnaryOperator = Negative
  deriving Eq

instance Show UnaryOperator where
  show Negative = "-"

data BinaryOperator = Add | Subtract | Multiply | Divide
  deriving Eq

instance Show BinaryOperator where
  show Add = " + "
  show Subtract = " - "
  show Multiply = " × "
  show Divide = " ÷ "

data TernaryOperator = Branch
  deriving Eq

data ValueExpression ref
  = ScalarLiteral
      Scalar
  | UnaryOperatorApply
      UnaryOperator
      (ValueExpression ref)
  | BinaryOperatorApply
      BinaryOperator
      (ValueExpression ref)
      (ValueExpression ref)
  | Reference
      ref
  deriving Eq

type Expr = ValueExpression String

class RefShow a where refshow :: a -> String
instance RefShow Identifier where refshow = id
instance RefShow Units where refshow = show

-- Whether the expression is complex enough to put parens around.
isSimpleExpr :: ValueExpression ref -> Bool
isSimpleExpr (ScalarLiteral _) = True
isSimpleExpr (UnaryOperatorApply _ _) = True
isSimpleExpr (BinaryOperatorApply _ _ _) = False
isSimpleExpr (Reference _) = True

maybeWrap :: RefShow ref => ValueExpression ref -> String
maybeWrap e = if isSimpleExpr e
  then show e
  else "(" ++ show e ++ ")"

instance RefShow ref => Show (ValueExpression ref) where
  show (ScalarLiteral s) = show s
  show (UnaryOperatorApply o e) = show o ++ maybeWrap e
  show (BinaryOperatorApply o e1 e2)
    = maybeWrap e1 ++ show o ++ maybeWrap e2
  show (Reference id) = refshow id

referencesOfExpr :: Eq ref => ValueExpression ref -> [ref]
referencesOfExpr (ScalarLiteral _) = []
referencesOfExpr (UnaryOperatorApply _ e) = referencesOfExpr e
referencesOfExpr (BinaryOperatorApply _ e1 e2)
  = nub $ concatMap referencesOfExpr [e1, e2]
referencesOfExpr (Reference id) = [id]

unitsOfExpr :: ValueExpression ref -> [BaseUnit]
unitsOfExpr (ScalarLiteral (Scalar _ us)) = keys $ getMap us
unitsOfExpr (UnaryOperatorApply _ e) = unitsOfExpr e
unitsOfExpr (BinaryOperatorApply _ e1 e2)
  = nub $ concatMap unitsOfExpr [e1, e2]
unitsOfExpr (Reference _) = []

data AssignmentOption
  = ConstrainedScalar
  | ConstrainedDimensionality VectorDimensionality
  deriving (Eq, Ord, Show)

data PrintOption
  = SingleLine
  | MultiLineFractions
  deriving (Eq, Ord, Show)

data Statement
  = Assignment Identifier Expr (Set AssignmentOption)
  | Print Expr (Maybe Units) (Set PrintOption)
  | DeclareDimension Identifier
  | DeclareUnit Identifier (Maybe Identifier)
  | DeclareConversion Identifier Identifier Transformation
  | Comment
  | Input Identifier Dimensionality
  | Relation Identifier (ValueExpression Units) (ValueExpression Units)
  deriving Show

type Program = [Statement]

