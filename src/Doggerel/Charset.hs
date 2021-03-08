module Doggerel.Charset (
    ShowForCharset(..),
    OutputCharset(..),
    showExponent
  ) where

data OutputCharset = AsciiCharset | UnicodeCharset
  deriving Eq

-- This typeclass provides a convenient way for presentation to take advantage
-- of unicode when it's available.
class ShowForCharset a where
  showForCharset :: OutputCharset -> a -> String

instance ShowForCharset Double where showForCharset _ = show

-- Convert an Int value to the corresponding decimal representation in the
-- Unicode superscript characters.
intToSuperscript :: Int -> String
intToSuperscript n
  | n == 0 = (:[]) $ head superscriptDigits
  | n < 0 = superscriptMinus : intToSuperscript (negate n)
  | otherwise = reverse $ s n
  where
    superscriptDigits = "⁰¹²³⁴⁵⁶⁷⁸⁹"
    superscriptMinus = '⁻'
    s :: Int -> String
    s 0 = ""
    s n = (superscriptDigits !! (n `mod` 10)) : s (n `div` 10)

showExponent :: OutputCharset -> Int -> String
showExponent _ 1 = ""
showExponent UnicodeCharset i = intToSuperscript i
showExponent AsciiCharset i = '^' : show i
