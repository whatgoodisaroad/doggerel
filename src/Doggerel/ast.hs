module Doggerel.Ast (
    BinaryOperator(
      Add,
      Divide,
      Multiply,
      Subtract
    ),
    Identifier,
    Program,
    Statement(
      Assignment,
      DeclareDimension,
      DeclareConversion,
      DeclareUnit,
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

type Identifier = String

data UnaryOperator = Negative

instance Show UnaryOperator where
  show Negative = "-"

data BinaryOperator = Add | Subtract | Multiply | Divide

instance Show BinaryOperator where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "×"
  show Divide = "÷"

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

data Statement
  = Assignment Identifier ValueExpression
  | Print ValueExpression (Maybe Units)
  | DeclareDimension Identifier
  | DeclareUnit Identifier (Maybe Identifier)
  | DeclareConversion Identifier Identifier Transformation
  | Comment
  deriving Show

type Program = [Statement]

