{-# LANGUAGE FlexibleInstances #-}

module Doggerel.Ast (
    AssignmentOption(..),
    BinaryOperator(..),
    Dimspec(..),
    DimspecTerm(..),
    Expr,
    Identifier,
    PrintOption(..),
    Program,
    RelationParams,
    Statement(..),
    Units,
    UnaryOperator(..),
    UnitOption(..),
    ValueExpression(..),
    binaryOperatorPrecendence,
    getPrintUnits,
    literalsOfExpr,
    mapRefs,
    referencesOfExpr,
    unitsOfExpr,
    vecDimsToDimspec
  ) where

import Data.Foldable (find)
import Data.List (intercalate, nub, sort)
import Data.Map.Strict as Map (keys, toList)
import Data.Set as Set (Set, empty, singleton, toList, union)
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
  deriving Eq

instance Ord BinaryOperator where
  b1 `compare` b2 = o b1 `compare` o b2
    where
      -- Define the precedence:
      o Multiply              = 0
      o Divide                = 1
      o Add                   = 2
      o Subtract              = 3
      o GreaterThan           = 4
      o LessThan              = 4
      o GreaterThanOrEqualTo  = 4
      o LessThanOrEqualTo     = 4
      o LogicalAnd            = 5
      o LogicalOr             = 6

binaryOperatorPrecendence :: [BinaryOperator]
binaryOperatorPrecendence = sort [
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
  ]

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
  | ListLiteral [ValueExpression ref lit]
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
    showForCharset charset (ListLiteral es)
      =   "["
      ++  (intercalate ", " $ map (showForCharset charset) es)
      ++  "]"

instance (RefShow ref, ShowForCharset lit) =>
  Show (ValueExpression ref lit) where
    show = showForCharset UnicodeCharset

-- Find the reference identifiers referred to explicitly anywhere in the given
-- expression
referencesOfExpr :: Eq ref => ValueExpression ref lit -> [ref]
referencesOfExpr (Literal _) = []
referencesOfExpr (UnaryOperatorApply _ e) = referencesOfExpr e
referencesOfExpr (BinaryOperatorApply _ e1 e2)
  = nub $ concatMap referencesOfExpr [e1, e2]
referencesOfExpr (Reference id) = [id]
referencesOfExpr (FunctionApply id _) = [id]
referencesOfExpr (ListLiteral es) = nub $ concatMap referencesOfExpr es

literalsOfExpr :: Eq lit => ValueExpression ref lit -> [lit]
literalsOfExpr (Literal l) = [l]
literalsOfExpr (UnaryOperatorApply _ e) = literalsOfExpr e
literalsOfExpr (BinaryOperatorApply _ e1 e2)
  = nub $ concatMap literalsOfExpr [e1, e2]
literalsOfExpr (Reference id) = []
literalsOfExpr (FunctionApply id _) = []
literalsOfExpr (ListLiteral es) = nub $ concatMap literalsOfExpr es

-- Find the BaseUnit values  referred to explicitly anywhere in the expression.
unitsOfExpr :: ValueExpression ref Scalar -> [BaseUnit]
unitsOfExpr (Literal (Scalar _ us)) = keys $ getMap us
unitsOfExpr (UnaryOperatorApply _ e) = unitsOfExpr e
unitsOfExpr (BinaryOperatorApply _ e1 e2)
  = nub $ concatMap unitsOfExpr [e1, e2]
unitsOfExpr (Reference _) = []
unitsOfExpr (FunctionApply _ e) = unitsOfExpr e
unitsOfExpr (ListLiteral es) = nub $ concatMap unitsOfExpr es

data AssignmentOption
  = ConstrainedScalar
  | ConstrainedDimensionality Dimspec
  deriving (Eq, Ord, Show)

data PrintOption
  = SingleLine
  | MultiLineFractions
  | AsciiOnlyPragma
  | OutputUnits Units
  deriving (Eq, Ord, Show)

data UnitOption
  = UnitDimensionality Dimspec
  | NaturalUnitDecl
  deriving (Eq, Ord, Show)

type RelationParams = [(Identifier, Units)]

data Statement
  = Assignment Identifier Expr (Set AssignmentOption)
  | Update Identifier Expr
  | Print Expr (Set PrintOption)
  | DeclareDimension Identifier (Maybe Dimspec)
  | DeclareUnit Identifier (Set UnitOption)
  | DeclareConversion Units Units Transformation
  | Comment
  | Input Identifier Dimensionality
  | Relation
      Identifier
      RelationParams
      (ValueExpression Identifier Quantity)
      (ValueExpression Identifier Quantity)
  | Block Program
  | Conditional Expr Program (Maybe Program)
  | WhileLoop Expr Program
  deriving (Eq, Show)

type Program = [Statement]

getPrintUnits :: Set PrintOption -> Maybe Units
getPrintUnits s = unwrap <$> find isUnits s
  where
    isUnits (OutputUnits _) = True
    isUnits _ = False
    unwrap (OutputUnits us) = us

data DimspecTerm
  = DSTermDim Identifier (Maybe Int) Int
  | DSTermRange Identifier (Maybe Int) (Maybe Int) Int
  | DSTermVar Identifier Int
  deriving (Eq, Ord)

instance ShowForCharset DimspecTerm where
  showForCharset charset (DSTermDim id Nothing deg) =
    id ++ showExponent charset deg
  showForCharset charset (DSTermDim id (Just i) deg) =
    id ++ "(" ++ show i ++ ")" ++ showExponent charset deg
  showForCharset charset (DSTermRange id mlow mhigh deg) =
    id ++ "(" ++ slow ++ ".." ++ shigh ++ ")" ++ showExponent charset deg
    where
      slow = case mlow of { Nothing -> ""; Just i -> show i; }
      shigh = case mhigh of { Nothing -> ""; Just i -> show i; }
  showForCharset charset (DSTermVar id deg) =
    ":" ++ id ++ showExponent charset deg
instance Show DimspecTerm where show = showForCharset AsciiCharset

data Dimspec
  = DSTerm DimspecTerm
  | DSSum [Dimspec]
  | DSProduct [Dimspec]
  deriving (Eq, Ord)

showDimspec :: OutputCharset -> Dimspec -> String
showDimspec charset (DSTerm t) = showForCharset charset t
showDimspec charset (DSSum dss)
  = intercalate " + " $ map (showDimspec charset) dss
showDimspec charset (DSProduct dss) = intercalate sep $ map f dss
  where
    f (DSTerm t) = showForCharset charset t
    f t = "( " ++ (showDimspec charset t) ++ " )"
    sep = if charset == UnicodeCharset then "·" else " "

instance ShowForCharset Dimspec where
  showForCharset charset ds = "{ " ++ (showDimspec charset ds) ++ " }"
instance Show Dimspec where show = showForCharset UnicodeCharset

mapRefs :: (a -> b) -> ValueExpression a v -> ValueExpression b v
mapRefs _ (Literal v) = Literal v
mapRefs f (Reference a) = Reference $ f a
mapRefs f (UnaryOperatorApply op e) = UnaryOperatorApply op $ mapRefs f e
mapRefs f (BinaryOperatorApply op e1 e2)
  = BinaryOperatorApply op (mapRefs f e1) (mapRefs f e2)
mapRefs f (FunctionApply ref args) = FunctionApply (f ref) (mapRefs f args)
mapRefs f (ListLiteral es) = ListLiteral $ map (mapRefs f) es

concreteDmToDsComponent :: DegreeMap Dimension -> Dimspec
concreteDmToDsComponent dm = let
    g (Dimension id midx, deg) = DSTerm $ DSTermDim id midx deg
  in case Map.toList $ getMap dm of
    [] -> DSProduct []
    [t] -> g t
    ts -> DSProduct $ map g ts

-- Translate a concrete vector dimensionality into dimspec format. Since it's
-- concrete, this won't do any range abbreviation.
vecDimsToDimspec :: VectorDimensionality -> Dimspec
vecDimsToDimspec (VecDims s) = case Set.toList s of
  [] -> DSProduct []
  [d] -> concreteDmToDsComponent d
  ds ->  DSSum $ map concreteDmToDsComponent ds
