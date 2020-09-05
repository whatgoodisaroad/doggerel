{-# LANGUAGE LambdaCase #-}

module Doggerel.Output (
    prettyPrint
  ) where

import Data.Map.Strict (assocs)
import Data.Set (Set)
import Doggerel.Ast
import Doggerel.Charset
import Doggerel.Core
import Doggerel.DegreeMap

-- Pretty print the expression and the resulting vector value that in the
-- desired style.
-- With the MultiLineFractions option, if any component of the vector have units
-- of negative degree, then print on three lines in a fraction style. Otherwise
-- print non fractional vectors on one line.
prettyPrint :: Set PrintOption -> Expr -> Vector -> [String]
prettyPrint opts expr vec
  = if MultiLineFractions `elem` opts && anyComponentIsFraction vec
    then multiline charset expr vec
    else [oneLine charset expr vec]
    where
      charset
        = if AsciiOnlyPragma `elem` opts then AsciiCharset else UnicodeCharset

-- Does any component of the given vector have units of negative degree.
anyComponentIsFraction :: Vector -> Bool
anyComponentIsFraction v = flip any (vectorAsFractions v) $ \case
  Left _ -> False
  Right _ -> True

-- Pretty print in the multiline fraction style.
multiline :: OutputCharset -> Expr -> Vector -> [String]
multiline charset expr vec = [
    topBot ++ "   " ++ openBraceTop ++ " " ++ a ++ " " ++ closeBraceTop,
    es     ++ " = " ++ openBraceMid ++ " " ++ b ++ " " ++ closeBraceMid,
    topBot ++ "   " ++ openBraceBot ++ " " ++ c ++ " " ++ closeBraceBot
  ]
  where
    fractions = vectorAsFractions vec
    triplets = map (showComponent charset) fractions
    (a, b, c) = concatLists ("   ", " , ", "   ") triplets
    (es, topBot) = showMultilineExpr charset expr
    ascii = charset == AsciiCharset

    openBraceTop = if ascii then "/" else "⎧"
    openBraceMid = if ascii then "|" else "⎨"
    openBraceBot = if ascii then "\\" else "⎩"
    closeBraceTop = if ascii then "\\" else "⎫"
    closeBraceMid = if ascii then "|" else "⎬"
    closeBraceBot = if ascii then "/" else "⎭"

-- Print the expression for multiline style as a tuple of the printed expression
-- and the spaces to print above and below it for proper alignment.
showMultilineExpr :: OutputCharset -> Expr -> (String, String)
showMultilineExpr charset e = (es, topBot)
  where
    es = showForCharset charset e
    topBot = replicate (length es) ' '

-- Pretty print in the one-line style.
oneLine :: OutputCharset -> Expr -> Vector -> String
oneLine charset expr vec
  = showForCharset charset expr ++ " = " ++ showForCharset charset vec

-- Given a vector component either represented as a non-fractional scalar, or as
-- a tuple of quantity, numerator units and denominator units, express the
-- printed result as three lines of text of equal length.
showComponent ::
     OutputCharset
  -> Either Scalar (Quantity, Units, Units)
  -> (String, String, String)
showComponent _ (Left scalar) = (topBotton, mid, topBotton)
  where
    mid = show scalar
    topBotton = replicate (length mid) ' '
showComponent charset (Right (q, num, den)) = (top, mid, bottom)
  where
    qs = show q
    nums' = showForCharset charset num
    nums = qs ++ " " ++ nums'
    dens = showForCharset charset den
    diff = abs $ length nums - length dens
    top = if length nums > length dens
      then nums
      else replicate diff ' ' ++ nums
    bottom = if length nums > length dens
      then replicate diff ' ' ++ dens
      else dens
    mid = replicate
      (length top `max` length bottom)
      (if charset == AsciiCharset then '-' else '─')

-- Given a triplet of separator strings and a list of printed component triples
-- concatenate each together (with the corresponding separator interspersed)
-- into a resulting triple.
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

-- Given a vector, give a list of Either values, each representing a component
-- of the vector. If a component has no fractional part, it's represented as a
-- scalar, but if it does have a fractional part, the parts of the component are
-- separated into a truple of quantity, numerator units and denominator units.
vectorAsFractions :: Vector -> [Either Scalar (Quantity, Units, Units)]
vectorAsFractions (Vector m) = map toEither $ assocs m
  where
    toEither :: (Units, Quantity) -> Either Scalar (Quantity, Units, Units)
    toEither (u, q) = if isEmpty den
      then Left $ Scalar q u
      else Right (q, num, den)
      where
        (num, den) = getFractionPair u
