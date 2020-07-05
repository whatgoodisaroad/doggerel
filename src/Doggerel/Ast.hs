module Doggerel.Ast (
    AssignmentOption(..),
    BinaryOperator(
      Add,
      Divide,
      Multiply,
      Subtract
    ),
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
      TernaryOperatorApply,
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
  deriving Eq

-- Whether the expression is complex enough to put parens around.
isSimpleExpr :: ValueExpression -> Bool
isSimpleExpr (ScalarLiteral _) = True
isSimpleExpr (UnaryOperatorApply _ _) = True
isSimpleExpr (BinaryOperatorApply _ _ _) = False
isSimpleExpr (TernaryOperatorApply _ _ _ _) = False
isSimpleExpr (Reference _) = True

maybeWrap :: ValueExpression -> String
maybeWrap e = if isSimpleExpr e
  then show e
  else "(" ++ show e ++ ")"

instance Show ValueExpression where
  show (ScalarLiteral s) = show s
  show (UnaryOperatorApply o e) = show o ++ maybeWrap e
  show (BinaryOperatorApply o e1 e2)
    = maybeWrap e1 ++ show o ++ maybeWrap e2
  show (TernaryOperatorApply Branch e1 e2 e3)
    = maybeWrap e1 ++ "?" ++ maybeWrap e2 ++ ":" ++ maybeWrap e3
  show (Reference id) = id

referencesOfExpr :: ValueExpression -> [Identifier]
referencesOfExpr (ScalarLiteral _) = []
referencesOfExpr (UnaryOperatorApply _ e) = referencesOfExpr e
referencesOfExpr (BinaryOperatorApply _ e1 e2)
  = nub $ concatMap referencesOfExpr [e1, e2]
referencesOfExpr (TernaryOperatorApply _ e1 e2 e3)
  = nub $ concatMap referencesOfExpr [e1, e2, e3]
referencesOfExpr (Reference id) = [id]

unitsOfExpr :: ValueExpression -> [BaseUnit]
unitsOfExpr (ScalarLiteral (Scalar _ us)) = keys $ getMap us
unitsOfExpr (UnaryOperatorApply _ e) = unitsOfExpr e
unitsOfExpr (BinaryOperatorApply _ e1 e2)
  = nub $ concatMap unitsOfExpr [e1, e2]
unitsOfExpr (TernaryOperatorApply _ e1 e2 e3)
  = nub $ concatMap unitsOfExpr [e1, e2, e3]
unitsOfExpr (Reference id) = []

data AssignmentOption
  = ConstrainedScalar
  | ConstrainedDimensionality VectorDimensionality
  deriving (Eq, Ord, Show)

data PrintOption
  = SingleLine
  | MultiLineFractions
  deriving (Eq, Ord, Show)

data Statement
  = Assignment Identifier ValueExpression (Set AssignmentOption)
  | Print ValueExpression (Maybe Units) (Set PrintOption)
  | DeclareDimension Identifier
  | DeclareUnit Identifier (Maybe Identifier)
  | DeclareConversion Identifier Identifier Transformation
  | Comment
  | Input Identifier Dimensionality
  deriving Show

type Program = [Statement]

