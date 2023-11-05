-- | PrettyJson.hs

module PrettyJson.PrettyJson (string, renderJValue) where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

import PrettyJson.SimpleJson (JValue(..))
import PrettyJson.Prettify (Doc, (<->), char, integer, double, fsep, hcat, punctuate, text)

-- Json String Constructor
enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <-> x <-> char right

smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <-> text (replicate (4 - length h) '0')
           <-> text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <-> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape ch = ch < ' ' || ch == '\x7f' || ch > '\xff'
          simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
            where ch a b = (a, ['\\',b])

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

-- Json compound object constructor
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JInteger num) = integer num
renderJValue (JDouble num) = double num
renderJValue (JString str) = string str
renderJValue (JArray ary) = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
  where field (name,val) = string name
                          <-> text ": "
                          <-> renderJValue val

-- $setup
-- >>> let value = renderJValue (JObject [("f", JInteger 1), ("q", JBool True)])
--
-- | Renderer
--
-- Examples:
--
-- >>> :type value
-- value :: Doc
--
-- | >>> putStrLn (compact value)
-- {"f": 1.0,
-- "q": true
-- }
--
-- | >>> putStrLn (pretty 10 value)
-- {"f": 1.0,
-- "q": true
-- }
--
-- | >>> putStrLn (pretty 20 value)
-- {"f": 1.0, "q": true
-- }
--
-- | >>> putStrLn (pretty 30 value)
-- {"f": 1.0, "q": true }
