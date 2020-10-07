{-# LANGUAGE FlexibleInstances #-}

module Doggerel.Ast (
    AssignmentOption(..),
    BinaryOperator(
      Add,
      Divide,
      LogicalAnd,
      LogicalOr,
      LessThan,
      LessThanOrEqualTo,
      GreaterThan,
      GreaterThanOrEqualTo,
      Multiply,
      Subtract
    ),
    Expr,
    Identifier,
    PrintOption(..),
    Program,
    Statement(..),
    Units,
    UnaryOperator(
      Exponent,
      LogicalNot,
      Negative
    ),
    ValueExpression(
      BinaryOperatorApply,
      FunctionApply,
      Literal,
      Reference,
      UnaryOperatorApply
    ),
    getPrintUnits,
    referencesOfExpr,
    unitsOfExpr
  ) where

import Data.Foldable (find)
import Data.List (nub)
import Data.Map.Strict (keys)
import Data.Set (Set)
import Doggerel.Charset
import Doggerel.Core
import Doggerel.Conversion
import Doggerel.DegreeMap (DegreeMap, getMap)

type Identifier = String

data UnaryOperator
  = Negative
  | Exponent Quantity
  | LogicalNot
  deriving (Eq, Ord)

instance ShowForCharset UnaryOperator where
  showForCharset _ Negative = "-"
  showForCharset _ (Exponent q) = "^" ++ show q
  showForCharset AsciiCharset LogicalNot = "!"
  showForCharset UnicodeCharset LogicalNot = "¬"
instance Show UnaryOperator where show = showForCharset UnicodeCharset

data BinaryOperator
  = Add
  | Divide
  | LogicalAnd
  | LogicalOr
  | LessThan
  | LessThanOrEqualTo
  | GreaterThan
  | GreaterThanOrEqualTo
  | Multiply
  | Subtract
  deriving (Eq, Ord)

instance ShowForCharset BinaryOperator where
  showForCharset _  Add = " + "
  showForCharset _ Subtract = " - "
  showForCharset _ LessThan = " < "
  showForCharset _ GreaterThan = " > "
  showForCharset AsciiCharset Multiply = " * "
  showForCharset UnicodeCharset Multiply = " × "
  showForCharset AsciiCharset Divide = " / "
  showForCharset UnicodeCharset Divide = " ÷ "
  showForCharset AsciiCharset LogicalAnd = " && "
  showForCharset UnicodeCharset LogicalAnd = " ∧ "
  showForCharset AsciiCharset LogicalOr = " || "
  showForCharset UnicodeCharset LogicalOr = " ∨ "
  showForCharset AsciiCharset LessThanOrEqualTo = " <= "
  showForCharset UnicodeCharset LessThanOrEqualTo = " ≤ "
  showForCharset AsciiCharset GreaterThanOrEqualTo = " >= "
  showForCharset UnicodeCharset GreaterThanOrEqualTo = " ≥ "

instance Show BinaryOperator where show = showForCharset UnicodeCharset

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
isSimpleExpr BinaryOperatorApply {} = False
isSimpleExpr (Reference _) = True
isSimpleExpr (FunctionApply _ _ ) = True

maybeWrap ::
     (RefShow ref, ShowForCharset lit)
  => OutputCharset
  -> ValueExpression ref lit
  -> String
maybeWrap charset e = if isSimpleExpr e
  then showForCharset charset e
  else "(" ++ showForCharset charset e ++ ")"

instance (RefShow ref, ShowForCharset lit) =>
  ShowForCharset (ValueExpression ref lit) where
    showForCharset charset (Literal s) = showForCharset charset s
    showForCharset charset (UnaryOperatorApply (Exponent radix) e)
      = maybeWrap charset e ++ "^" ++ show radix
    showForCharset charset (UnaryOperatorApply o e)
      = showForCharset charset o ++ maybeWrap charset e
    showForCharset charset (BinaryOperatorApply o e1 e2)
      = maybeWrap charset e1 ++ showForCharset charset o ++ maybeWrap charset e2
    showForCharset _ (Reference id) = refshow id
    showForCharset charset (FunctionApply id expr)
      = refshow id ++ "(" ++ showForCharset charset expr ++ ")"

instance (RefShow ref, ShowForCharset lit) =>
  Show (ValueExpression ref lit) where
    show = showForCharset UnicodeCharset

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
  | AsciiOnlyPragma
  | OutputUnits Units
  deriving (Eq, Ord, Show)

data Statement
  = Assignment Identifier Expr (Set AssignmentOption)
  | Print Expr (Set PrintOption)
  | DeclareDimension Identifier
  | DeclareUnit Identifier (Maybe Dimensionality)
  | DeclareConversion Units Units Transformation
  | Comment
  | Input Identifier Dimensionality
  | Relation
      Identifier
      (ValueExpression Units Quantity)
      (ValueExpression Units Quantity)
  | Block Program
  | Conditional Expr Program (Maybe Program)
  deriving (Eq, Show)

type Program = [Statement]

getPrintUnits :: Set PrintOption -> Maybe Units
getPrintUnits s = unwrap <$> find isUnits s
  where
    isUnits (OutputUnits _) = True
    isUnits _ = False
    unwrap (OutputUnits us) = us
