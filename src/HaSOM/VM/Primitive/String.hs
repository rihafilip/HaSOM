{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.VM.Primitive.String (primitives) where

import Control.Monad ((>=>))
import Data.Char (isDigit, isLetter, isSpace)
import Data.Hashable (Hashable (hash))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Utility ((<+))
import HaSOM.VM.Object (ObjIx)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "String",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("concatenate:", concatM),
    ("asSymbol", asSymbol),
    ("hashcode", hashcode),
    ("length", lengthM),
    ("isWhiteSpace", isWS),
    ("isLetters", isLetters),
    ("isDigits", isDigits),
    ("=", equal),
    ("primSubstringFrom:to:", substringFromTo)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

selfStr ::
  (forall r. (UniverseEff r, Lifted IO r) => Text -> Eff r ObjIx) ->
  NativeFun
selfStr f = pureNativeFun @N0 $ \self Nil -> do
  str <- castString self
  f str

---------------------------------
-- Instance

concatM :: NativeFun
concatM = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  selfT <- castString self
  otherT <- castString other

  newString (selfT <+ otherT) >>= addToGC

asSymbol :: NativeFun
asSymbol = selfStr (newSymbol >=> addToGC)

hashcode :: NativeFun
hashcode = selfStr (newInt . hash >=> addToGC)

lengthM :: NativeFun
lengthM = selfStr (newInt . T.length >=> addToGC)

isPredicate :: (Char -> Bool) -> NativeFun
isPredicate p = selfStr $ \case
  "" -> newFalse >>= addToGC
  str ->
    if T.all p str
      then newTrue >>= addToGC
      else newFalse >>= addToGC

isWS :: NativeFun
isWS = isPredicate isSpace

isLetters :: NativeFun
isLetters = isPredicate isLetter

isDigits :: NativeFun
isDigits = isPredicate isDigit

equal :: NativeFun
equal = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  selfT <- castString self
  otherT <- castString other

  if selfT == otherT
    then newTrue >>= addToGC
    else newFalse >>= addToGC

substringFromTo :: NativeFun
substringFromTo = pureNativeFun @N2 $ \self (from :+: to :+: Nil) -> do
  str <- castString self
  fromI <- castInt from
  toI <- castInt to

  let start = fromI - 1
  let len = toI - start

  let str' = T.take len (T.drop start str)
  newString str' >>= addToGC
