{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.VM.Universe.Operations
  ( -- * Primitive operations

    -- ** Stack
    pushStack,
    popStack,
    topStack,

    -- ** Globals
    getGlobalE,
    setGlobalE,
    getClass,

    -- ** Call stack
    getCurrentCallFrame,
    pushCallFrame,
    popCallFrame,

    -- ** Locals
    getLocal,
    setLocal,
    getSelf,

    -- ** Literals
    getLiteralE,

    -- * Fields
    getFieldE,
    setFieldE,

    -- ** GC
    getAsObject,
    setObject,

    -- * Instructions
    doHalt,
    doDup,
    doPop,
    doPushLiteral,
    doPushLocal,
    doPushField,
    doPushGlobal,
    doSetLocal,
    doSetField,
    doSetGlobal,
    doCall,
    doSupercall,
    doReturn,
    doNonlocalReturn,

    -- * Re-export of used effects
    module Control.Eff,
    module Control.Eff.ExcT,
    module Control.Eff.Reader.Strict,
    module Control.Eff.State.Strict,
  )
where

import Combinator ((...))
import Control.Eff
import Control.Eff.ExcT
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Eff.Utility
import Control.Monad (void)
import qualified Data.Stack as St
import Data.Text (Text)
import Data.Text.Utility
import qualified HaSOM.VM.GC as GC
import HaSOM.VM.Object
import HaSOM.VM.Universe
import qualified HaSOM.VM.VMArray as Arr

---------------------------------------------------------------
-- Helper functions

infixl 4 <?>

(<?>) :: Member ExcT r => Text -> Maybe a -> Eff r a
(<?>) = throwOnNothing

infixl 4 <?.

(<?.) :: Member ExcT r => Text -> (a -> Maybe b) -> a -> Eff r b
(<?.) txt f = throwOnNothing txt . f

onIdxErrorMessage :: Show a => Text -> a -> Text
onIdxErrorMessage accessing idx = accessing <+ " on index " <+ showT idx <+ " doesn't exists "

callErrorMessage :: LiteralIx -> Text
callErrorMessage li = "Couldn't find method " <+ showT li

callFrameEnvironmentErrorMessage :: LocalIx -> Text
callFrameEnvironmentErrorMessage env =
  "Call frame on environment"
    <+ showT env
    <+ "is not available"

getter :: (Show k, Member ExcT r) => (k -> c -> Maybe a) -> Text -> k -> c -> Eff r a
getter access accessing key =
  throwOnNothing
    (onIdxErrorMessage accessing key)
    . access key

---------------------------------------------------------------
-- Stack manipulation

pushStack :: ObjStackEff r => ObjIx -> Eff r ()
pushStack idx = modify $ St.push idx

popStack :: (ObjStackEff r, Member ExcT r) => Eff r ObjIx
popStack =
  modifyEffYield $
    "Popping an empty stack" <?. St.pop

topStack :: (ObjStackEff r, Member ExcT r) => Eff r ObjIx
topStack = get >>= "Accessing a top of an empty stack" <?. St.top

---------------------------------------------------------------
-- Globals manipulation

getGlobalE :: (GlobalsEff r, Member ExcT r) => GlobalIx -> Eff r VMGlobalNat
getGlobalE idx = get >>= getter getGlobal "Global" idx

setGlobalE :: (GlobalsEff r) => GlobalIx -> VMGlobalNat -> Eff r ()
setGlobalE = modify ... setGlobal

getClass :: (GlobalsEff r, Member ExcT r) => GlobalIx -> Eff r VMClassNat
getClass idx =
  getGlobalE idx >>= \case
    ClassGlobal c -> pure c
    ObjectGlobal _ ->
      throwT $
        "Expected class on index "
          <+ showT idx
          <+ ", but got global object instead"

---------------------------------------------------------------
-- Call frame manipulation

getCurrentCallFrame :: (CallStackEff r, Member ExcT r) => Eff r CallFrameNat
getCurrentCallFrame =
  get >>= ("Trying to access top of empty call stack" <?. St.top)

pushCallFrame :: CallStackEff r => CallFrameNat -> Eff r ()
pushCallFrame = modify . St.push

popCallFrame :: (CallStackEff r, Member ExcT r) => Eff r ()
popCallFrame =
  modifyEff @CallStackNat $
    "Trying to pop empty call stack" <?. St.popSt

---------------------------------------------------------------
-- Locals manipulation

getLocal :: (Member ExcT r, CallStackEff r) => LocalIx -> LocalIx -> Eff r ObjIx
getLocal env li = do
  cf <- getCurrentCallFrame
  locs <-
    locals
      <$> ( callFrameEnvironmentErrorMessage env <?> findCallStack env cf
          )
  getter Arr.get "Local" li locs
  where
    findCallStack 0 cf = Just cf
    findCallStack n BlockCallFrame {capturedFrame} = findCallStack (n - 1) capturedFrame
    findCallStack _ _ = Nothing

setLocal :: (Member ExcT r, CallStackEff r) => LocalIx -> LocalIx -> ObjIx -> Eff r ()
setLocal env li objIx = do
  callStack <- get @CallStackNat

  newCallStack <- case St.pop callStack of
    Nothing -> throwT "Trying to access top of empty call stack"
    Just (cfs, cf) -> (`St.push` cfs) <$> atCallStack env cf

  put newCallStack
  where
    atCallStack 0 cf = edit cf
    atCallStack n cf@BlockCallFrame {capturedFrame} = do
      nested <- atCallStack (n - 1) capturedFrame
      pure cf {capturedFrame = nested}
    atCallStack _ _ = throwT $ callFrameEnvironmentErrorMessage env

    edit cf = do
      let ls = locals cf
      ls' <- onIdxErrorMessage "Field" li <?> Arr.set li objIx ls
      pure cf {locals = ls'}

getSelf :: (CallStackEff r, Member ExcT r) => Eff r ObjIx
getSelf = getLocal 0 0

---------------------------------------------------------------
-- Get literal

getLiteralE :: (LiteralEff r, Member ExcT r) => LiteralIx -> Eff r VMLiteral
getLiteralE idx = get @VMLiterals >>= getter getLiteral "Literal" idx

---------------------------------------------------------------
-- Fields manipulation

getFieldE :: (CallStackEff r, GCEff r, Member ExcT r) => FieldIx -> Eff r ObjIx
getFieldE fi = do
  self <- getSelf >>= getAsObject
  getter getField "Field" fi self

setFieldE :: (CallStackEff r, GCEff r, Member ExcT r) => FieldIx -> ObjIx -> Eff r ()
setFieldE fi obj = do
  selfIdx <- getSelf
  self <- getAsObject selfIdx
  newSelf <- onIdxErrorMessage "Field" fi <?> setField fi obj self
  setObject selfIdx newSelf

---------------------------------------------------------------
-- GC manipulation

addToGC :: GCEff r => VMObjectNat -> Eff r ObjIx
addToGC obj = do
  gc <- get @GCNat
  let (gc', newIdx) = GC.new gc
  let gc'' = GC.setAt newIdx obj gc'
  put gc''
  pure newIdx

getAsObject :: (GCEff r, Member ExcT r) => ObjIx -> Eff r VMObjectNat
getAsObject idx = get >>= getter GC.getAt "Object" idx

setObject :: GCEff r => ObjIx -> VMObjectNat -> Eff r ()
setObject = modify ... GC.setAt

---------------------------------------------------------------
-- Simple operations

doHalt :: Eff r ()
doHalt = undefined -- TODO

doDup :: (ObjStackEff r, Member ExcT r) => Eff r ()
doDup = topStack >>= pushStack

doPop :: (ObjStackEff r, Member ExcT r) => Eff r ()
doPop = void popStack

---------------------------------------------------------------
-- Pushing on stack

doPushLiteral :: UniverseEff r => LiteralIx -> Eff r ()
doPushLiteral li = do
  lit <- getLiteralE li

  MkCoreClasses {..} <- ask

  -- Create literal object
  obj <- uncurry newObject $ case lit of
    IntLiteral intValue ->
      (integerClass, \clazz fields -> IntObject {clazz, fields, intValue})
    DoubleLiteral doubleValue ->
      (doubleClass, \clazz fields -> DoubleObject {clazz, fields, doubleValue})
    StringLiteral stringValue ->
      (stringClass, \clazz fields -> StringObject {clazz, fields, stringValue})
    SymbolLiteral symbolValue ->
      (symbolClass, \clazz fields -> SymbolObject {clazz, fields, symbolValue})
    BlockLiteral vb ->
      (blockClass, \clazz fields -> undefined) -- TODO

  -- Push to GC
  idx <- addToGC obj
  pushStack idx

doPushLocal :: (Member ExcT r, CallStackEff r, ObjStackEff r) => LocalIx -> LocalIx -> Eff r ()
doPushLocal env li = getLocal env li >>= pushStack

doPushField :: (CallStackEff r, GCEff r, Member ExcT r, ObjStackEff r) => FieldIx -> Eff r ()
doPushField fi = getFieldE fi >>= pushStack

doPushGlobal :: (ObjStackEff r, GlobalsEff r, Member ExcT r) => GlobalIx -> Eff r ()
doPushGlobal gi = getGlobalE gi >>= pushStack . transform
  where
    transform (ClassGlobal MkVMClass {asObject}) = asObject
    transform (ObjectGlobal oi) = oi

---------------------------------------------------------------
-- Setting values

doSetLocal :: (CallStackEff r, ObjStackEff r, Member ExcT r) => LocalIx -> LocalIx -> Eff r ()
doSetLocal env li = popStack >>= setLocal env li

doSetField :: (ObjStackEff r, CallStackEff r, GCEff r, Member ExcT r) => FieldIx -> Eff r ()
doSetField fi = popStack >>= setFieldE fi

doSetGlobal :: (ObjStackEff r, GlobalsEff r, Member ExcT r) => GlobalIx -> Eff r ()
doSetGlobal gi = popStack >>= setGlobalE gi . ObjectGlobal

---------------------------------------------------------------
-- Call

doCall :: UniverseEff r => LiteralIx -> Eff r ()
doCall li = do
  thisIx <- popStack
  clazz <- clazz <$> getAsObject thisIx
  doCallUnified thisIx li clazz

doSupercall :: UniverseEff r => LiteralIx -> Eff r ()
doSupercall li = do
  thisIx <- popStack
  clazz <- clazz <$> getAsObject thisIx
  superclass <-
    (callErrorMessage li <?> superclass clazz)
      >>= getClass

  doCallUnified thisIx li superclass

doCallUnified :: UniverseEff r => ObjIx -> LiteralIx -> VMClassNat -> Eff r ()
doCallUnified thisIx li clazz = do
  method <- findMethod li clazz >>= throwOnNothing (callErrorMessage li)

  let (pCount, lCount, currentCode) = case method of
        BytecodeMethod {body, parameterCount, localCount} -> (parameterCount, localCount, Right body)
        NativeMethod {nativeBody, parameterCount} -> (parameterCount, 0, Left nativeBody)

  localsL <- createLocals thisIx pCount lCount

  let cf =
        MethodCallFrame
          { currentCode,
            pc = 0,
            locals = Arr.fromList localsL,
            frameId = Nothing
          }
  pushCallFrame cf

---------------------------------------------------------------
-- Returning

doReturn :: (CallStackEff r, Member ExcT r) => Eff r ()
doReturn = popCallFrame

doNonlocalReturn :: Eff r ()
doNonlocalReturn = undefined -- TODO

---------------------------------------------------------------

newObject :: (GlobalsEff r, Member ExcT r, GCEff r) => GlobalIx -> (VMClassNat -> Fields -> VMObjectNat) -> Eff r VMObjectNat
newObject idx constructor = do
  clazz <- getClass idx
  n <- GC.nil <$> get @GCNat
  pure $ constructor clazz (newFields clazz n)

---------------------------------------------------------------

-- [ self, arg1, arg2, ..., local1, local2, ... ]
createLocals :: (GCEff r, ObjStackEff r, Member ExcT r) => ObjIx -> Int -> Int -> Eff r [ObjIx]
createLocals self pCount lCount = do
  nilIx <- GC.nil <$> get @GCNat
  let locals = replicate lCount nilIx
  withArgs <- mkArgs pCount locals
  pure (self : withArgs)
  where
    mkArgs :: (ObjStackEff r, Member ExcT r) => Int -> [ObjIx] -> Eff r [ObjIx]
    mkArgs 0 acc = pure acc
    mkArgs n acc = do
      idx <-
        modifyEffYield @ObjStack $
          "Not enough parameters on stack"
            <?. St.pop
      mkArgs (n - 1) (idx : acc)

findMethod :: (GlobalsEff r, Member ExcT r) => LiteralIx -> VMClassNat -> Eff r (Maybe VMMethodNat)
findMethod litIx MkVMClass {methods, superclass} =
  case (superclass, getMethod litIx methods) of
    (_, Just m) -> pure $ Just m
    (Just superIx, Nothing) -> do
      super <- getClass superIx
      findMethod litIx super
    (Nothing, Nothing) -> pure Nothing
