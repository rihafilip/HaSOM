-- | Parsing helpers for Alex
module HaSOM.Lexer.Alex.Parsing (decode, unstring, parseInt, parseDouble) where

import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.ByteString.Lazy as B
import Data.Char (digitToInt)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Word (Word8)
import GHC.Float (int2Double)

-- | Turn Bytestring to Text
decode :: ByteString -> Text
decode = either (error . show) id . decodeUtf8' . toStrict

unstring :: ByteString -> Text
unstring = flip (foldl appl) replacements . unquote . decode
  where
    unquote s =
      let s' = fromMaybe s $ T.stripPrefix "'" s
       in fromMaybe s' $ T.stripSuffix "'" s'
    appl source (find, replace) = T.replace find replace source
    replacements =
      [ ("\\t", "\t"),
        ("\\b", "\b"),
        ("\\n", "\n"),
        ("\\r", "\r"),
        ("\\f", "\f"),
        ("\\0", "\0"),
        ("\\\'", "\'"),
        ("\\\\", "\\")
      ]

-- | Parse Bytestring to Int
parseInt :: ByteString -> Int
parseInt = B.foldl addInt 0

-- | Parse Bytestring to Double
parseDouble :: ByteString -> Double
parseDouble = put . B.foldl f (0.0, Nothing)
  where
    f (i, Nothing) ch
      | ch == c2w '.' = (i, Just (0, 0.1))
      | otherwise = (i `addDouble` ch, Nothing)
    f (i, Just (d, scale)) ch = (i, Just (addDecimal d ch scale))

    put (i, Nothing) = i
    put (i, Just (d, _)) = i + d

------------------------------------------------------------

addNum :: Num a => (Word8 -> a) -> a -> Word8 -> a
addNum f n ch = n * 10 + f ch

addInt :: Int -> Word8 -> Int
addInt = addNum w2i

addDouble :: Double -> Word8 -> Double
addDouble = addNum w2d

addDecimal :: Double -> Word8 -> Double -> (Double, Double)
addDecimal n ch scale = (n + (w2d ch * scale), scale * 0.1)

------------------------------------------------------------

w2i :: Word8 -> Int
w2i = digitToInt . w2c

w2d :: Word8 -> Double
w2d = int2Double . w2i
