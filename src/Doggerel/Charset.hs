module Doggerel.Charset (
    ShowForCharset(..),
    OutputCharset(..)
  ) where

data OutputCharset = AsciiCharset | UnicodeCharset
  deriving Eq

-- This typeclass provides a convenient way for presentation to take advantage
-- of unicode when it's available.
class ShowForCharset a where
  showForCharset :: OutputCharset -> a -> String

instance ShowForCharset Double where showForCharset _ = show
