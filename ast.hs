module Ast (
    Scalar(Scalar),
    Units
  ) where

import Core

data Scalar = Scalar Quantity (DegreeMap BaseUnit)

instance Show Scalar where
  show (Scalar magnitude units) = show magnitude ++ " " ++ show units

type Identifier = String

data UnaryOperator = Negative
data BinaryOperator = Add | Subtract | Multiply | Divide
data TernaryOperator = Branch

data ValueExpression
  = ScalarLiteral Scalar
  | UnaryOperatorApply UnaryOperator ValueExpression
  | BinaryOperatorApply BinaryOperator ValueExpression ValueExpression
  | TernaryOperatorApply TernaryOperator ValueExpression ValueExpression ValueExpression
  | Reference Identifier

data Statement
  = Assignment Identifier ValueExpression
  | Print ValueExpression
