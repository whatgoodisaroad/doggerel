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
      Comment,
      Relation
    ),
    Units,
    UnaryOperator(
      Exponent,
      Negative
    ),
    ValueExpression(
      BinaryOperatorApply,
      FunctionApply,
      Literal,
      Reference,
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

data UnaryOperator
  = Negative
  | Exponent Quantity
  deriving (Eq, Ord)

instance Show UnaryOperator where
  show Negative = "-"

data BinaryOperator = Add | Subtract | Multiply | Divide
  deriving (Eq, Ord)

instance Show BinaryOperator where
  show Add = " + "
  show Subtract = " - "
  show Multiply = " × "
  show Divide = " ÷ "

data TernaryOperator = Branch
  deriving Eq

data ValueExpression ref lit
  = Literal lit
  | Reference ref
  | UnaryOperatorApply UnaryOperator (ValueExpression ref lit)
  | BinaryOperatorApply
      BinaryOperator
      (ValueExpression ref lit)
      (ValueExpression ref lit)
  | FunctionApply ref (ValueExpression ref lit)
  deriving (Eq, Ord)

type Expr = ValueExpression String Scalar

class RefShow a where refshow :: a -> String
instance RefShow Identifier where refshow = id
instance RefShow Units where refshow = show

-- Whether the expression is complex enough to put parens around.
isSimpleExpr :: ValueExpression ref lit -> Bool
isSimpleExpr (Literal _) = True
isSimpleExpr (UnaryOperatorApply _ _) = True
isSimpleExpr (BinaryOperatorApply _ _ _) = False
isSimpleExpr (Reference _) = True
isSimpleExpr (FunctionApply _ _ ) = True

maybeWrap :: (RefShow ref, Show lit) => ValueExpression ref lit -> String
maybeWrap e = if isSimpleExpr e
  then show e
  else "(" ++ show e ++ ")"

instance (RefShow ref, Show lit) => Show (ValueExpression ref lit) where
  show (Literal s) = show s
  show (UnaryOperatorApply (Exponent radix) e)
    = maybeWrap e ++ "^" ++ show radix
  show (UnaryOperatorApply o e) = show o ++ maybeWrap e
  show (BinaryOperatorApply o e1 e2)
    = maybeWrap e1 ++ show o ++ maybeWrap e2
  show (Reference id) = refshow id
  show (FunctionApply id expr) = refshow id ++ "(" ++ show expr ++ ")"

-- Find the reference identifiers referred to explicitly anywheere in the given
-- expression
referencesOfExpr :: Eq ref => ValueExpression ref lit -> [ref]
referencesOfExpr (Literal _) = []
referencesOfExpr (UnaryOperatorApply _ e) = referencesOfExpr e
referencesOfExpr (BinaryOperatorApply _ e1 e2)
  = nub $ concatMap referencesOfExpr [e1, e2]
referencesOfExpr (Reference id) = [id]
referencesOfExpr (FunctionApply id _) = [id]

-- Find the BaseUnit values  referred to explicitly anywhere in the expression.
unitsOfExpr :: ValueExpression ref Scalar -> [BaseUnit]
unitsOfExpr (Literal (Scalar _ us)) = keys $ getMap us
unitsOfExpr (UnaryOperatorApply _ e) = unitsOfExpr e
unitsOfExpr (BinaryOperatorApply _ e1 e2)
  = nub $ concatMap unitsOfExpr [e1, e2]
unitsOfExpr (Reference _) = []
unitsOfExpr (FunctionApply _ e) = unitsOfExpr e

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
  | Relation
      Identifier
      (ValueExpression Units Quantity)
      (ValueExpression Units Quantity)
  deriving (Eq, Show)

type Program = [Statement]

