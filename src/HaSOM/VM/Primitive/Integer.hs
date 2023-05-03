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
  [ ("+", mkNativeFun $ binaryMethod (+)),
    ("-", mkNativeFun $ binaryMethod (-)),
    ("*", mkNativeFun $ binaryMethod (*)),
    ("/", mkNativeFun divideInt),
    ("//", mkNativeFun divideDouble),
    ("%", mkNativeFun modInt),
    ("rem:", mkNativeFun remInt),
    ("&", mkNativeFun $ binaryMethodInt (.&.)),
    ("<<", mkNativeFun $ binaryMethodInt shiftL),
    (">>>", mkNativeFun $ binaryMethodInt rotateR),
    ("bitXor:", mkNativeFun $ binaryMethodInt xor),
    ("sqrt", mkNativeFun sqrtInt),
    ("atRandom", mkNativeFun atRandom),
    ("=", mkNativeFun equalInt),
    ("<", mkNativeFun lessThanInt),
    ("asString", mkNativeFun asString),
    ("as32BitSignedValue", mkNativeFun nativeNotImplemented), -- TODO
    ("as32BitUnsignedValue", mkNativeFun nativeNotImplemented), -- TODO
    ("asDouble", mkNativeFun asDouble)
  ]

classMs :: [(Text, NativeFun)]
classMs =
  [ ("fromString:", mkNativeFun intFromString)
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

binaryMethod :: (UniverseEff r, Lifted IO r) => (forall a. Num a => a -> a -> a) -> Eff r ()
binaryMethod op = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  obj <-
    castOrPromote self other >>= \case
      Left (i1, i2) -> newInt (i1 `op` i2)
      Right (d1, d2) -> newDouble (d1 `op` d2)
  addToGC obj

binaryMethodInt :: (UniverseEff r, Lifted IO r) => (Int -> Int -> Int) -> Eff r ()
binaryMethodInt op = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castInt self
  b <- castInt other

  newInt (a `op` b) >>= addToGC

-- binaryMethodNonzero ::
--   (UniverseEff r, Lifted IO r) =>
--   (Int -> Int -> Int) ->
--   (Double -> Double -> Double) ->
--   Eff r ()
-- binaryMethodNonzero opI opD = pureNativeFun @N1 $ \self (other :+: Nil) -> do
--   castOrPromote self other >>= \case
--     Left (_, 0) -> getNil
--     Left (i1, i2) -> newInt (i1 `opI` i2) >>= addToGC
--     Right (_, 0) -> getNil
--     Right (d1, d2) -> newDouble (d1 `opD` d2) >>= addToGC

---------------------------------
-- Instance

divideDouble :: (UniverseEff r, Lifted IO r) => Eff r ()
divideDouble = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castPromoteDouble self
  b <- castPromoteDouble other

  if b == 0
    then getNil
    else newDouble (a / b) >>= addToGC

divideInt :: (UniverseEff r, Lifted IO r) => Eff r ()
divideInt = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castInt self
  b <- castInt other

  if b == 0
    then getNil
    else newInt (a `div` b) >>= addToGC

remInt :: (UniverseEff r, Lifted IO r) => Eff r ()
remInt = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castInt self
  b <- castInt other

  if b == 0
    then getNil
    else newInt (a `rem` b) >>= addToGC

modInt :: (UniverseEff r, Lifted IO r) => Eff r ()
modInt = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castInt self
  b <- castInt other

  if b == 0
    then getNil
    else newInt (a `mod` b) >>= addToGC


sqrtInt :: (UniverseEff r, Lifted IO r) => Eff r ()
sqrtInt = pureNativeFun @N0 $ \self Nil -> do
  s <- castInt self

  newDouble (sqrt $ int2Double s) >>= addToGC

equalInt :: (UniverseEff r, Lifted IO r) => Eff r ()
equalInt = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castInt self
  b <- castInt other

  if a == b
    then newTrue >>= addToGC
    else newFalse >>= addToGC

lessThanInt :: (UniverseEff r, Lifted IO r) => Eff r ()
lessThanInt = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castInt self
  b <- castInt other

  if a < b
    then newTrue >>= addToGC
    else newFalse >>= addToGC

atRandom :: (UniverseEff r, Lifted IO r) => Eff r ()
atRandom = pureNativeFun @N0 $ \self Nil -> do
  _ <- castInt self
  i <- lift (randomIO :: IO Int)
  newInt i >>= addToGC

asString :: (UniverseEff r, Lifted IO r) => Eff r ()
asString = pureNativeFun @N0 $ \self Nil -> do
  a <- castInt self
  newString (showT a) >>= addToGC

asDouble :: (UniverseEff r, Lifted IO r) => Eff r ()
asDouble = pureNativeFun @N0 $ \self Nil -> do
  a <- castInt self
  newDouble (int2Double a) >>= addToGC

---------------------------------
-- Class

intFromString :: (UniverseEff r, Lifted IO r) => Eff r ()
intFromString = pureNativeFun @N1 $ \_ (strIx :+: Nil) -> do
  str <- castString strIx

  maybe getNil (newInt >=> addToGC) $
    readMaybe (T.unpack str)
