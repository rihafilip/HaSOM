{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.VM.Primitive.Integer (primitives) where

import Control.Monad ((>=>))
import Data.Bits
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Utility (showT)
import Data.Word (Word32)
import GHC.Float (int2Double)
import HaSOM.VM.Object (ObjIx)
import HaSOM.VM.Object.VMObject (VMObject (..))
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations
import System.Random (randomIO)
import Text.Read (readMaybe)
import Data.Fixed (mod')

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Integer",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("+", binaryMethod (+)),
    ("-", binaryMethod (-)),
    ("*", binaryMethod (*)),
    ("/", nonzeroMethod div),
    ("//", divideDouble),
    ("%", nonzeroMethod mod),
    ("rem:", nonzeroMethod rem),
    ("&", binaryMethodInt (.&.)),
    ("<<", binaryMethodInt shiftL),
    (">>>", binaryMethodInt shiftR),
    ("bitXor:", binaryMethodInt xor),
    ("sqrt", sqrtInt),
    ("atRandom", atRandom),
    ("=", numberBoolMethod (==)),
    ("<", numberBoolMethod (<)),
    ("asString", asString),
    ("as32BitSignedValue", as32Signed ),
    ("as32BitUnsignedValue", as32Unsigned ),
    ("asDouble", asDouble)
  ]

classMs :: [(Text, NativeFun)]
classMs =
  [ ("fromString:", intFromString)
  ]

castOrPromote ::
  (GCEff r, Member ExcT r) =>
  ObjIx ->
  ObjIx ->
  Eff r (Either (Int, Int) (Double, Double))
castOrPromote a b = do
  aObj <- getAsObject a
  bObj <- getAsObject b

  case (aObj, bObj) of
    (IntObject {intValue = i1}, IntObject {intValue = i2}) ->
      pure $ Left (i1, i2)
    (IntObject {intValue = i1}, DoubleObject {doubleValue = d2}) ->
      pure $ Right (int2Double i1, d2)
    (IntObject {}, obj) -> wrongObjectType obj NumT
    (DoubleObject {doubleValue = d1}, IntObject {intValue = i2}) ->
      pure $ Right (d1, int2Double i2)
    (DoubleObject {doubleValue = d1}, DoubleObject {doubleValue = d2}) ->
      pure $ Right (d1, d2)
    (obj, _) -> wrongObjectType obj NumT

---------------------------------

binaryMethod :: (forall a. Num a => a -> a -> a) -> NativeFun
binaryMethod op = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  obj <-
    castOrPromote self other >>= \case
      Left (i1, i2) -> newInt (i1 `op` i2)
      Right (d1, d2) -> newDouble (d1 `op` d2)
  addToGC obj

binaryMethodInt :: (Int -> Int -> Int) -> NativeFun
binaryMethodInt op = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castInt self
  b <- castInt other

  newInt (a `op` b) >>= addToGC

nonzeroMethod :: (Int -> Int -> Int) -> NativeFun
nonzeroMethod op = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castInt self
  b <- castInt other
  if b == 0
    then getNil
    else newInt (a `op` b) >>= addToGC

mbyDowncast :: RestrictedNativeFun r => Double -> Eff r ObjIx
mbyDowncast res
  | res `mod'` 1 == 0 = newInt (round res) >>= addToGC
  | otherwise = newDouble res >>= addToGC

---------------------------------
-- Instance

divideDouble :: NativeFun
divideDouble = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castPromoteDouble self
  b <- castPromoteDouble other

  if b == 0
    then getNil
    else mbyDowncast (a / b)

sqrtInt :: NativeFun
sqrtInt = pureNativeFun @N0 $ \self Nil -> do
  s <- castInt self
  mbyDowncast (sqrt $ int2Double s)

atRandom :: NativeFun
atRandom = pureNativeFun @N0 $ \_ Nil -> do
  i <- lift (randomIO :: IO Int)
  newInt i >>= addToGC

asString :: NativeFun
asString = pureNativeFun @N0 $ \self Nil -> do
  a <- castInt self
  newString (showT a) >>= addToGC

asDouble :: NativeFun
asDouble = pureNativeFun @N0 $ \self Nil -> do
  a <- castInt self
  newDouble (int2Double a) >>= addToGC

as32Signed :: NativeFun
as32Signed = pureNativeFun @N0 $ \self Nil -> do
  a <- castInt self
  newInt (cast a) >>= addToGC
  where
    cast :: Int -> Int
    cast = fromIntegral . (fromIntegral :: Int -> Int32)

as32Unsigned :: NativeFun
as32Unsigned = pureNativeFun @N0 $ \self Nil -> do
  a <- castInt self
  newInt (cast a) >>= addToGC
  where
    cast :: Int -> Int
    cast = fromIntegral . (fromIntegral :: Int -> Word32)

---------------------------------
-- Class

intFromString :: NativeFun
intFromString = pureNativeFun @N1 $ \_ (strIx :+: Nil) -> do
  str <- castStringOrSymbol strIx

  maybe getNil (newInt >=> addToGC) $
    readMaybe (T.unpack str)
