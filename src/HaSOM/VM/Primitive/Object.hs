{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use forM_" #-}

module HaSOM.VM.Primitive.Object (primitives) where

import Data.Hashable (Hashable (hash))
import Data.Text (Text)
import HaSOM.VM.Object
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations
import qualified HaSOM.VM.VMArray as Arr

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Object",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("class", classM),
    ("objectSize", objectSize),
    ("==", equals),
    ("hashcode", hashcode),
    ("inspect", nativeNotImplemented), -- TODO
    ("halt", nativeNotImplemented), -- TODO
    ("perform:", perform),
    ("perform:withArguments:", performWithArguments),
    ("perform:inSuperclass:", performInSuperclass),
    ("perform:withArguments:inSuperclass:", performWithArgumentsInSuperclass),
    ("instVarAt:", insVarAt),
    ("instVarAt:put:", insVarAtPut),
    ("instVarNamed:", instVarNamed)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance

classM :: NativeFun
classM = pureNativeFun @N0 $ \self Nil ->
  asObject . clazz <$> getAsObject self

objectSize :: NativeFun
objectSize = pureNativeFun @N0 $ \_ Nil -> do
  newInt (-1) >>= addToGC

equals :: NativeFun
equals = pureNativeFun @N1 $ \self (other :+: Nil) -> do
  a <- getAsObject self
  b <- getAsObject other

  let eq = case (a, b) of
        (ClassObject {classOf = c1}, ClassObject {classOf = c2}) -> c1 == c2
        (IntObject {intValue = i1}, IntObject {intValue = i2}) -> i1 == i2
        (DoubleObject {doubleValue = d1}, DoubleObject {doubleValue = d2}) -> d1 == d2
        (StringObject {stringValue = s1}, StringObject {stringValue = s2}) -> s1 == s2
        (SymbolObject {symbolValue = s1}, SymbolObject {symbolValue = s2}) -> s1 == s2
        (MethodObject {methodValue = m1, holder = h1}, MethodObject {methodValue = m2, holder = h2}) ->
          m1 == m2 && h1 == h2
        (PrimitiveObject {methodValue = m1, holder = h1}, PrimitiveObject {methodValue = m2, holder = h2}) ->
          m1 == m2 && h1 == h2
        _ -> self == other

  if eq
    then newTrue >>= addToGC
    else newFalse >>= addToGC

hashcode :: NativeFun
hashcode = pureNativeFun @N0 $ \self Nil -> do
  obj <- getAsObject self

  let h = case obj of
        ClassObject {classOf} -> hash classOf
        IntObject {intValue} -> hash intValue
        DoubleObject {doubleValue} -> hash doubleValue
        StringObject {stringValue} -> hash stringValue
        SymbolObject {symbolValue} -> hash symbolValue
        MethodObject {methodValue, holder} ->
          hash (methodValue, holder)
        PrimitiveObject {methodValue, holder} ->
          hash (methodValue, holder)
        _ -> hash self
  newInt h >>= addToGC

performCommon ::
  (UniverseEff r) =>
  ObjIx ->
  ObjIx ->
  Arr.VMArray FieldIx ObjIx ->
  GlobalIx ->
  Eff r ()
performCommon self aSymbol args cls = do
  sym <- castSymbol aSymbol
  mIx <- internLiteralE (SymbolLiteral sym)
  sendMessage self mIx cls args

perform :: NativeFun
perform = nativeFun @N1 $ \self (aSymbol :+: Nil) -> do
  clazz <-
    getAsObject self >>= getAsObject . asObject . clazz >>= \case
      ClassObject {classOf} -> pure classOf
      obj -> wrongObjectType obj ClassT
  performCommon self aSymbol (Arr.fromList []) clazz

performWithArguments :: NativeFun
performWithArguments = nativeFun @N2 $ \self (aSymbol :+: args :+: Nil) -> do
  clazz <-
    getAsObject self >>= getAsObject . asObject . clazz >>= \case
      ClassObject {classOf} -> pure classOf
      obj -> wrongObjectType obj ClassT

  (arr, _) <- castArray args
  performCommon self aSymbol arr clazz

performInSuperclass :: NativeFun
performInSuperclass = nativeFun @N2 $ \self (aSymbol :+: clazzIx :+: Nil) -> do
  clazz <-
    getAsObject clazzIx >>= \case
      ClassObject {classOf} -> pure classOf
      obj -> wrongObjectType obj ClassT
  performCommon self aSymbol (Arr.fromList []) clazz

performWithArgumentsInSuperclass :: NativeFun
performWithArgumentsInSuperclass =
  nativeFun @N3 $ \self (aSymbol :+: args :+: clazzIx :+: Nil) -> do
    clazz <-
      getAsObject clazzIx >>= \case
        ClassObject {classOf} -> pure classOf
        obj -> wrongObjectType obj ClassT

    (arr, _) <- castArray args
    performCommon self aSymbol arr clazz

insVarAt :: NativeFun
insVarAt = pureNativeFun @N1 $ \self (idx :+: Nil) -> do
  selfObj <- getAsObject self
  i <- castInt idx

  if i == 0
    then pure (asObject (clazz selfObj))
    else maybe getNil pure $ getField (ix i - 1) selfObj

insVarAtPut :: NativeFun
insVarAtPut = pureNativeFun @N2 $ \self (idx :+: obj :+: Nil) -> do
  selfObj <- getAsObject self
  i <- castInt idx

  case setField (ix i - 1) obj selfObj of
    Nothing -> pure ()
    Just newSelf -> setObject self newSelf

  pure self

instVarNamed :: NativeFun
instVarNamed = pureNativeFun @N1 $ \self (sym :+: Nil) -> do
  selfObj <- getAsObject self
  let fields = instanceFields (clazz selfObj)
  name <- castSymbol sym

  case Arr.elemIndex name fields of
    Nothing -> getNil
    Just fi -> maybe getNil pure $ getField fi selfObj
