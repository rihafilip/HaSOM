{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module HaSOM.VM.Primitive.String (primitives) where

import Data.Text (Text)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations
import Data.Text.Utility ((<+))
import Data.Hashable (Hashable(hash))
import HaSOM.VM.Object (ObjIx)
import Control.Monad ((>=>))
import qualified Data.Text as T
import Data.Char (isSpace, isLetter, isDigit)

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "String",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("concatenate:", mkNativeFun concatM),
    ("asSymbol", mkNativeFun asSymbol),
    ("hashcode", mkNativeFun hashcode),
    ("length", mkNativeFun lengthM),
    ("isWhiteSpace", mkNativeFun isWS),
    ("isLetters", mkNativeFun isLetters),
    ("isDigits", mkNativeFun isDigits),
    ("=", mkNativeFun equal),
    ("primSubstringFrom:to:", mkNativeFun substringFromTo)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

selfStr :: (UniverseEff r, Lifted IO r) => (Text -> Eff r ObjIx) -> Eff r ()
selfStr f = pureNativeFun @N0 $ \self Nil -> do
  str <- castString self
  f str

---------------------------------
-- Instance

concatM :: (UniverseEff r, Lifted IO r) => Eff r ()
concatM = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  selfT <- castString self
  otherT <- castString other

  newString (selfT <+ otherT) >>= addToGC

asSymbol :: (UniverseEff r, Lifted IO r) => Eff r ()
asSymbol = selfStr (newSymbol >=> addToGC)

hashcode :: (UniverseEff r, Lifted IO r) => Eff r ()
hashcode = selfStr (newInt . hash >=> addToGC)

lengthM :: (UniverseEff r, Lifted IO r) => Eff r ()
lengthM = selfStr (newInt . T.length >=> addToGC)

isPredicate :: (UniverseEff r, Lifted IO r) => (Char -> Bool) ->  Eff r ()
isPredicate p = selfStr $ \case
  "" -> newFalse >>= addToGC
  str ->
    if T.all p str
      then newTrue >>= addToGC
      else newFalse >>= addToGC

isWS :: (UniverseEff r, Lifted IO r) => Eff r ()
isWS = isPredicate isSpace

isLetters :: (UniverseEff r, Lifted IO r) => Eff r ()
isLetters = isPredicate isLetter

isDigits :: (UniverseEff r, Lifted IO r) => Eff r ()
isDigits = isPredicate isDigit

equal :: (UniverseEff r, Lifted IO r) => Eff r ()
equal = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  selfT <- castString self
  otherT <- castString other

  if selfT == otherT
    then newTrue >>= addToGC
    else newFalse >>= addToGC

substringFromTo :: (UniverseEff r, Lifted IO r) => Eff r ()
substringFromTo = pureNativeFun @N2 $ \self (from :+: to :+: Nil) -> do
  str <- castString self
  fromI <- castInt from
  toI <- castInt to

  let start = fromI - 1
  let len = toI - start

  let str' = T.take len (T.drop start str)
  newString str' >>= addToGC
