{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.VM.Primitive.Class (primitives) where

import Control.Monad ((>=>))
import Data.Text (Text)
import HaSOM.VM.Object
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations
import qualified HaSOM.VM.VMArray as Arr

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Class",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("name", mkNativeFun nameM),
    ("new", mkNativeFun new),
    ("superclass", mkNativeFun superclassM),
    ("fields", mkNativeFun fieldsM),
    ("methods", mkNativeFun methodsM)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance

nameM :: (UniverseEff r, Lifted IO r) => Eff r ()
nameM = pureNativeFun @N0 $ \self Nil -> do
  MkVMClass {name} <-
    getAsObject self >>= \case
      ClassObject {classOf} -> getClass classOf
      obj -> wrongObjectType obj ClassT

  newSymbol name >>= addToGC

new :: (UniverseEff r, Lifted IO r) => Eff r ()
new = pureNativeFun @N0 $ \self Nil -> do
  classIx <-
    getAsObject self >>= \case
      ClassObject {classOf} -> pure classOf
      obj -> wrongObjectType obj ClassT

  newInstance classIx >>= addToGC

superclassM :: (UniverseEff r, Lifted IO r) => Eff r ()
superclassM = pureNativeFun @N0 $ \self Nil -> do
  MkVMClass {superclass} <-
    getAsObject self >>= \case
      ClassObject {classOf} -> getClass classOf
      obj -> wrongObjectType obj ClassT

  nilIx <- getNil

  case superclass of
    Nothing -> pure nilIx
    Just gi -> asObject <$> getClass gi

fieldsM :: (UniverseEff r, Lifted IO r) => Eff r ()
fieldsM = pureNativeFun @N0 $ \self Nil -> do
  MkVMClass {instanceFields} <-
    getAsObject self >>= \case
      ClassObject {classOf} -> getClass classOf
      obj -> wrongObjectType obj ClassT

  fs <- mapM (newSymbol >=> addToGC) instanceFields
  newArray fs >>= addToGC

methodsM :: (UniverseEff r, Lifted IO r) => Eff r ()
methodsM = pureNativeFun @N0 $ \self Nil -> do
  holder <-
    getAsObject self >>= \case
      ClassObject {classOf} -> pure classOf
      obj -> wrongObjectType obj ClassT

  MkVMClass {methods} <- getClass holder
  MkCoreClasses {methodClass, primitiveClass} <- ask
  ms <- mapM (trans holder methodClass primitiveClass >=> addToGC) $ methodsAsList methods

  newArray (Arr.fromList ms) >>= addToGC
  where
    trans holder mIx _ (methodValue, BytecodeMethod {}) =
      newObject mIx $ \clazz fields ->
        MethodObject
          { clazz,
            fields,
            holder,
            methodValue
          }
    trans holder _ pIx (methodValue, NativeMethod {}) =
      newObject pIx $ \clazz fields ->
        PrimitiveObject
          { clazz,
            fields,
            holder,
            methodValue
          }
