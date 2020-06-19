module Doggerel.Output (
    PrintOption(MultiLineFractions),
    prettyPrint
  ) where

import Data.Map.Strict (assocs)
import Data.Set (Set)
import Doggerel.Ast
import Doggerel.Core
import Doggerel.DegreeMap

data PrintOption = MultiLineFractions
  deriving (Eq, Ord)

prettyPrint :: Set PrintOption -> ValueExpression -> Vector -> [String]
prettyPrint opts expr vec
  = if MultiLineFractions `elem` opts && anyComponentIsFraction vec
    then multiline expr vec
    else [oneLine expr vec]

anyComponentIsFraction :: Vector -> Bool
anyComponentIsFraction v = flip any (vectorAsFractions v) $ \f -> case f of
  Left _ -> False
  Right _ -> True

multiline :: ValueExpression -> Vector -> [String]
multiline expr vec = [
    topBot ++ "   " ++ openBraceTop ++ " " ++ a ++ " " ++ closeBraceTop,
    es     ++ " = " ++ openBraceMid ++ " " ++ b ++ " " ++ closeBraceMid,
    topBot ++ "   " ++ openBraceBot ++ " " ++ c ++ " " ++ closeBraceBot
  ]
  where
    fractions = vectorAsFractions vec
    triplets = map showComponent fractions
    (a, b, c) = concatLists ("   ", " , ", "   ") triplets
    (es, topBot) = showMultilineExpr expr

openBraceTop = "⎧"
openBraceMid = "⎨"
openBraceBot = "⎩"
closeBraceTop = "⎫"
closeBraceMid = "⎬"
closeBraceBot = "⎭"

showMultilineExpr :: ValueExpression -> (String, String)
showMultilineExpr e = (es, topBot)
  where
    es = show e
    topBot = take (length es) $ repeat ' '

oneLine :: ValueExpression -> Vector -> String
oneLine expr vec = show expr ++ " = " ++ show vec

showComponent ::
     Either Scalar (Quantity, Units, Units)
  -> (String, String, String)
showComponent (Left scalar) = (topBotton, mid, topBotton)
  where
    mid = show scalar
    topBotton = take (length mid) $ repeat ' '
showComponent (Right (q, num, den)) = (top, mid, bottom)
  where
    qs = show q
    nums' = show num
    nums = qs ++ " " ++ nums'
    dens = show den
    diff = abs $ length nums - length dens
    top = if length nums > length dens
      then nums
      else (take diff $ repeat ' ') ++ nums
    bottom = if length nums > length dens
      then (take diff $ repeat ' ') ++ dens
      else dens
    mid = take ((length top) `max` (length bottom)) $ repeat '─'

concatLists ::
     (String, String, String)
  -> [(String, String, String)]
  -> (String, String, String)
concatLists _ [] = ([], [], [])
concatLists sep@(sepTop, sepMid, sepBot) ((a, b, c):d)
  = if more
      then (a ++ sepTop ++ e, b ++ sepMid ++ f, c ++ sepBot ++ g)
      else (a, b, c)
      where
        more = not $ null d
        (e, f, g) = concatLists sep d

vectorAsFractions :: Vector -> [Either Scalar (Quantity, Units, Units)]
vectorAsFractions (Vector m) = map toEither $ assocs m
  where
    toEither :: (Units, Quantity) -> Either Scalar (Quantity, Units, Units)
    toEither (u, q) = if isEmpty den
      then Left $ Scalar q u
      else Right (q, num, den)
      where
        (num, den) = getFractionPair u
