module Ast (
    BinaryOperator(
      Add,
      Divide,
      Multiply,
      Subtract
    ),
    Identifier,
    Program,
    Scalar(Scalar),
    Statement(
      Assignment,
      DeclareDimension,
      DeclareConversion,
      DeclareUnit,
      Print
    ),
    Units,
    UnaryOperator(
      Negative
    ),
    ValueExpression(
      BinaryOperatorApply,
      Reference,
      ScalarLiteral,
      TernaryOperatorApply,
      UnaryOperatorApply
    ),
    referencesOfExpr
  ) where

import Core
import Data.List (nub)

data Scalar = Scalar Quantity Units

instance Show Scalar where
  show (Scalar magnitude units) = show magnitude ++ " " ++ show units

type Identifier = String

data UnaryOperator = Negative

instance Show UnaryOperator where
  show Negative = "-"

data BinaryOperator = Add | Subtract | Multiply | Divide

instance Show BinaryOperator where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"

data TernaryOperator = Branch

data ValueExpression
  = ScalarLiteral
      Scalar
  | UnaryOperatorApply
      UnaryOperator
      ValueExpression
  | BinaryOperatorApply
      BinaryOperator
      ValueExpression
      ValueExpression
  | TernaryOperatorApply
      TernaryOperator
      ValueExpression
      ValueExpression
      ValueExpression
  | Reference
      Identifier

instance Show ValueExpression where
  show (ScalarLiteral s) = show s
  show (UnaryOperatorApply o e) = show o ++ " " ++ show e
  show (BinaryOperatorApply o e1 e2)
    = show e1 ++ " " ++ show o ++ " " ++ show e2
  show (TernaryOperatorApply Branch e1 e2 e3)
    = show e1 ++ " ? " ++ show e2 ++ " : " ++ show e3
  show (Reference id) = id

referencesOfExpr :: ValueExpression -> [Identifier]
referencesOfExpr (ScalarLiteral _) = []
referencesOfExpr (UnaryOperatorApply _ e) = referencesOfExpr e
referencesOfExpr (BinaryOperatorApply _ e1 e2)
  = nub $ concatMap referencesOfExpr [e1, e2]
referencesOfExpr (TernaryOperatorApply _ e1 e2 e3)
  = nub $ concatMap referencesOfExpr [e1, e2, e3]
referencesOfExpr (Reference id) = [id]

data Statement
  = Assignment Identifier ValueExpression
  | Print ValueExpression (Maybe Units)
  | DeclareDimension Identifier
  | DeclareUnit Identifier (Maybe Identifier)
  | DeclareConversion Identifier Identifier Transformation

type Program = [Statement]