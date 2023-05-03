{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module HaSOM.VM.Primitive.Array (primitives) where

import Data.Text (Text)
import HaSOM.VM.Object
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations
import qualified HaSOM.VM.VMArray as Arr

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Array",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("at:", mkNativeFun at),
    ("at:put:", mkNativeFun atPut),
    ("length", mkNativeFun lengthA)
  ]

classMs :: [(Text, NativeFun)]
classMs = [ ("new:", mkNativeFun new) ]

---------------------------------
-- Instance

at :: (UniverseEff r, Lifted IO r) => Eff r ()
at = pureNativeFun @N1 $ \self (atI :+: Nil) -> do
  (arr, _) <- castArray self
  atInt <- castInt atI

  maybe getNil pure $ Arr.get (ix atInt - 1) arr

atPut :: (UniverseEff r, Lifted IO r) =>Eff r ()
atPut = pureNativeFun @N2 $ \self (atI :+: val :+: Nil) -> do
  (arr, selfUpdate) <- castArray self
  atInt <- castInt atI

  let newArr = selfUpdate <$> Arr.set (ix atInt - 1) val arr

  maybe (pure ()) (setObject self) newArr

  pure self

lengthA :: (UniverseEff r, Lifted IO r) => Eff r ()
lengthA = pureNativeFun @N0 $ \self Nil -> do
   (arr, _) <- castArray self
   let l = Arr.length arr
   newInt l >>= addToGC


---------------------------------
-- Class

new :: (UniverseEff r, Lifted IO r) => Eff r ()
new = pureNativeFun @N1 $ \_ (sizeI :+: Nil) -> do
  size <- castInt sizeI
  nil <- getNil
  let arrayValue = Arr.new size nil
  MkCoreClasses {arrayClass} <- ask
  obj <- newObject arrayClass $ \clazz fields -> ArrayObject {clazz, fields, arrayValue}
  addToGC obj
