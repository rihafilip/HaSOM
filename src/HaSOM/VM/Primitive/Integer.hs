{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.VM.Primitive.Integer (primitives) where

import System.Random (randomIO)
import Data.Bits
import Data.Text (Text)
import GHC.Float (int2Double)
import HaSOM.VM.Object (ObjIx)
import HaSOM.VM.Object.VMObject (VMObject (..))
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations
import Data.Text.Utility (showT)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Control.Monad ((>=>))

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
    ("/", divideInt),
    ("//", divideDouble),
    ("%", modInt),
    ("rem:", remInt),
    ("&", binaryMethodInt (.&.)),
    ("<<", binaryMethodInt shiftL),
    (">>>", binaryMethodInt rotateR),
    ("bitXor:", binaryMethodInt xor),
    ("sqrt", sqrtInt),
    ("atRandom", atRandom),
    ("=", equalInt),
    ("<", lessThanInt),
    ("asString", asString),
    ("as32BitSignedValue", nativeNotImplemented), -- TODO
    ("as32BitUnsignedValue", nativeNotImplemented), -- TODO
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

castPromoteDouble :: (GCEff r, Member ExcT r) => ObjIx -> Eff r Double
castPromoteDouble idx =
  getAsObject idx >>= \case
    IntObject {intValue} -> pure (int2Double intValue)
    DoubleObject {doubleValue} -> pure doubleValue
    obj -> wrongObjectType obj NumT

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

---------------------------------
-- Instance

divideDouble :: NativeFun
divideDouble = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castPromoteDouble self
  b <- castPromoteDouble other

  if b == 0
    then getNil
    else newDouble (a / b) >>= addToGC

divideInt :: NativeFun
divideInt = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castInt self
  b <- castInt other

  if b == 0
    then getNil
    else newInt (a `div` b) >>= addToGC

remInt :: NativeFun
remInt = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castInt self
  b <- castInt other

  if b == 0
    then getNil
    else newInt (a `rem` b) >>= addToGC

modInt :: NativeFun
modInt = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castInt self
  b <- castInt other

  if b == 0
    then getNil
    else newInt (a `mod` b) >>= addToGC


sqrtInt :: NativeFun
sqrtInt = pureNativeFun @N0 $ \self Nil -> do
  s <- castInt self

  newDouble (sqrt $ int2Double s) >>= addToGC

equalInt :: NativeFun
equalInt = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castInt self
  b <- castInt other

  if a == b
    then newTrue >>= addToGC
    else newFalse >>= addToGC

lessThanInt :: NativeFun
lessThanInt = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castInt self
  b <- castInt other

  if a < b
    then newTrue >>= addToGC
    else newFalse >>= addToGC

atRandom :: NativeFun
atRandom = pureNativeFun @N0 $ \self Nil -> do
  _ <- castInt self
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

---------------------------------
-- Class

intFromString :: NativeFun
intFromString = pureNativeFun @N1 $ \_ (strIx :+: Nil) -> do
  str <- castString strIx

  maybe getNil (newInt >=> addToGC) $
    readMaybe (T.unpack str)
