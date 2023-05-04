{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.VM.Primitive.Double (primitives) where

import Control.Monad ((>=>))
import Data.Fixed (mod')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Utility (showT)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations
import Text.Read (readMaybe)

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Double",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("+", binaryMethod (+)),
    ("-", binaryMethod (-)),
    ("*", binaryMethod (*)),
    ("//", nonzeroMethod (/)),
    ("%", moduloDouble),
    ("sqrt", unaryMethod sqrt),
    ("round", roundDouble),
    ("asInteger", asInteger),
    ("cos", unaryMethod cos),
    ("sin", unaryMethod sin),
    ("=", numberBoolMethod (==)),
    ("<", numberBoolMethod (<)),
    ("asString", asString)
  ]

classMs :: [(Text, NativeFun)]
classMs =
  [ ("PositiveInfinity", infinityDouble),
    ("fromString:", doubleFromString)
  ]

---------------------------------

binaryMethod :: (Double -> Double -> Double) -> NativeFun
binaryMethod op = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castDouble self
  b <- castPromoteDouble other

  newDouble (a `op` b) >>= addToGC

unaryMethod :: (Double -> Double) -> NativeFun
unaryMethod op = pureNativeFun @N0 $ \self Nil ->
  castDouble self >>= newDouble . op >>= addToGC

nonzeroMethod :: (Double -> Double -> Double) -> NativeFun
nonzeroMethod op = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- castDouble self
  b <- castPromoteDouble other

  if b == 0
    then getNil
    else newDouble (a `op` b) >>= addToGC

---------------------------------
-- Instance

moduloDouble :: NativeFun
moduloDouble = nonzeroMethod $ \a b -> abs (a `mod'` b) * signum a

roundDouble :: NativeFun
roundDouble = pureNativeFun @N0 $ \self Nil ->
  castDouble self >>= newInt . round' >>= addToGC
  where
    round' x
      | x `mod'` 1 == 0.5 = ceiling x
      | otherwise = round x

asInteger :: NativeFun
asInteger = pureNativeFun @N0 $ \self Nil ->
  castDouble self >>= newInt . truncate >>= addToGC

asString :: NativeFun
asString = pureNativeFun @N0 $ \self Nil -> do
  a <- castDouble self
  newString (showT a) >>= addToGC

---------------------------------
-- Class

infinityDouble :: NativeFun
infinityDouble = pureNativeFun @N0 $ \_ Nil ->
  newDouble (1 / 0) >>= addToGC

doubleFromString :: NativeFun
doubleFromString = pureNativeFun @N1 $ \_ (strIx :+: Nil) -> do
  str <- castStringOrSymbol strIx

  maybe getNil (newDouble >=> addToGC) $
    readMaybe (T.unpack str)
