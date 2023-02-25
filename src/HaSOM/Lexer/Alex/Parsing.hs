module HaSOM.Lexer.Alex.Parsing(decode, parseInt, parseDouble) where

import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Internal (w2c, c2w)
import Data.Char (digitToInt)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Word (Word8)
import GHC.Float (int2Double)

decode :: ByteString -> Text
decode = either (error . show) id . decodeUtf8' . toStrict

parseInt :: ByteString -> Int
parseInt = B.foldl addInt 0

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
