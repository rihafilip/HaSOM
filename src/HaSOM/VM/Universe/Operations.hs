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
    internGlobalE,

    -- ** Call stack
    getCurrentCallFrame,
    pushCallFrame,
    popCallFrame,
    advancePC,

    -- ** Locals
    getLocal,
    setLocal,
    getSelf,
    internLiteralE,

    -- ** Literals
    getLiteralE,

    -- * Fields
    getFieldE,
    setFieldE,

    -- ** GC
    addToGC,
    getAsObject,
    setObject,
    getNil,

    -- ** Locals creation
    createLocals,

    -- ** Object creation
    newObject,
    newInstance,
    newTrue,
    newFalse,
    newInt,
    newDouble,
    newString,
    newSymbol,
    newBlock,
    newArray,

    -- ** Method manipulation
    sendMessage,
    findMethod,

    -- * Helper functions
    onIdxErrorMessage,
    callErrorMessage,
    callFrameEnvironmentErrorMessage,

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
import Control.Eff.IO.Utility
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Eff.Utility
import qualified Data.Stack as St
import Data.Text (Text)
import Data.Text.Utility
import qualified HaSOM.VM.GC as GC
import HaSOM.VM.Object
import HaSOM.VM.Universe
import qualified HaSOM.VM.VMArray as Arr

---------------------------------------------------------------
-- Helper functions

infixl 4 <?.

(<?.) :: Member ExcT r => Text -> (a -> Maybe b) -> a -> Eff r b
(<?.) txt f = throwOnNothing txt . f

onIdxErrorMessage :: Show a => Text -> a -> Text
onIdxErrorMessage accessing idx = accessing <+ " on index " <+ showT idx <+ " doesn't exists "

callErrorMessage :: (LiteralEff r, Member ExcT r) => LiteralIx -> Text -> Eff r Text
callErrorMessage li clazz = do
  m <- showT <$> getLiteralE li
  pure $ "Couldn't find method " <+ m <+ " in " <+ clazz

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

internGlobalE :: GlobalsEff r => Text -> Eff r GlobalIx
internGlobalE = modifyYield @VMGlobalsNat . internGlobal

---------------------------------------------------------------
-- Call frame manipulation

getCurrentCallFrame :: (CallStackEff r, Member ExcT r, Lifted IO r) => Eff r CallFrameNat
getCurrentCallFrame =
  get >>= ("Trying to access top of empty call stack" <?. St.top) >>= getCallFrame

pushCallFrame :: CallStackEff r => CallFrameNat -> Eff r ()
pushCallFrame = modify . St.push . PureCallFrame

popCallFrame :: (CallStackEff r, Member ExcT r) => Eff r CallStackItemNat
popCallFrame =
  modifyEffYield @CallStackNat $
    "Trying to pop empty call stack" <?. St.pop

advancePC :: (Lifted IO r, CallStackEff r, Member ExcT r) => Eff r ()
advancePC = do
  ci <- popCallFrame
  ci' <- modifyCallFrame ci (\cf -> pure cf {pc = pc cf + 1})
  modify @CallStackNat $ St.push ci'

---------------------------------------------------------------
-- Locals manipulation

getLocal :: (Member ExcT r, CallStackEff r, Lifted IO r) => LocalIx -> LocalIx -> Eff r ObjIx
getLocal env li = do
  cf <- getCurrentCallFrame
  foundCallFrame <- findCallStack env cf
  locs <-
    locals <$> throwOnNothing (callFrameEnvironmentErrorMessage env) foundCallFrame
  getter Arr.get "Local" li locs
  where
    findCallStack 0 cf = pure (Just cf)
    findCallStack n BlockCallFrame {capturedFrame} = lreadIORef capturedFrame >>= findCallStack (n - 1)
    findCallStack _ _ = pure Nothing

setLocal :: (Member ExcT r, CallStackEff r, Lifted IO r) => LocalIx -> LocalIx -> ObjIx -> Eff r ()
setLocal env li objIx = modifyEff @CallStackNat $ \callStack ->
  case St.pop callStack of
    Nothing -> throwT "Trying to access top of empty call stack"
    Just (cfs, cf) -> (`St.push` cfs) <$> modifyCallFrame cf (atCallStack env)
  where
    atCallStack 0 cf = edit cf
    atCallStack n cf@BlockCallFrame {capturedFrame} = do
      nested <- lreadIORef capturedFrame >>= atCallStack (n - 1)
      lwriteIORef capturedFrame nested
      pure cf
    atCallStack _ _ = throwT $ callFrameEnvironmentErrorMessage env

    edit cf = do
      let ls = locals cf
      ls' <- throwOnNothing (onIdxErrorMessage "Field" li) (Arr.set li objIx ls)
      pure cf {locals = ls'}

getSelf :: (CallStackEff r, Member ExcT r, Lifted IO r) => Eff r ObjIx
getSelf = getLocal 0 0

---------------------------------------------------------------
-- Get literal

getLiteralE :: (LiteralEff r, Member ExcT r) => LiteralIx -> Eff r VMLiteral
getLiteralE idx = get @VMLiterals >>= getter getLiteral "Literal" idx

internLiteralE :: LiteralEff r => VMLiteral -> Eff r LiteralIx
internLiteralE = modifyYield . internLiteral

---------------------------------------------------------------
-- Fields manipulation

getFieldE :: (CallStackEff r, GCEff r, Member ExcT r, Lifted IO r) => FieldIx -> Eff r ObjIx
getFieldE fi = do
  self <- getSelf >>= getAsObject
  getter getField "Field" fi self

setFieldE :: (CallStackEff r, GCEff r, Member ExcT r, Lifted IO r) => FieldIx -> ObjIx -> Eff r ()
setFieldE fi obj = do
  selfIdx <- getSelf
  self <- getAsObject selfIdx
  newSelf <- throwOnNothing (onIdxErrorMessage "Field" fi) (setField fi obj self)
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

getNil :: GCEff r => Eff r ObjIx
getNil = GC.nil <$> get @GCNat

---------------------------------------------------------------

newObject :: (GlobalsEff r, Member ExcT r, GCEff r) => GlobalIx -> (VMClassNat -> Fields -> VMObjectNat) -> Eff r VMObjectNat
newObject idx constructor = do
  clazz <- getClass idx
  n <- GC.nil <$> get @GCNat
  pure $ constructor clazz (newFields clazz n)

newInstance :: (GlobalsEff r, Member ExcT r, GCEff r) => GlobalIx -> Eff r VMObjectNat
newInstance = flip newObject (\clazz fields -> InstanceObject {clazz, fields})

newFalse :: (CoreClassesEff r, GlobalsEff r, Member ExcT r, GCEff r) => Eff r VMObjectNat
newFalse = do
  MkCoreClasses {falseClass} <- ask
  newInstance falseClass

newTrue :: (CoreClassesEff r, GlobalsEff r, Member ExcT r, GCEff r) => Eff r VMObjectNat
newTrue = do
  MkCoreClasses {trueClass} <- ask
  newInstance trueClass

newInt :: (CoreClassesEff r, GlobalsEff r, Member ExcT r, GCEff r) => Int -> Eff r VMObjectNat
newInt intValue = do
  MkCoreClasses {integerClass} <- ask
  newObject integerClass $ \clazz fields -> IntObject {clazz, fields, intValue}

newDouble :: (CoreClassesEff r, GlobalsEff r, Member ExcT r, GCEff r) => Double -> Eff r VMObjectNat
newDouble doubleValue = do
  MkCoreClasses {doubleClass} <- ask
  newObject doubleClass $ \clazz fields -> DoubleObject {clazz, fields, doubleValue}

newString :: (CoreClassesEff r, GlobalsEff r, Member ExcT r, GCEff r) => Text -> Eff r VMObjectNat
newString stringValue = do
  MkCoreClasses {stringClass} <- ask
  newObject stringClass $ \clazz fields -> StringObject {clazz, fields, stringValue}

newSymbol :: (CoreClassesEff r, GlobalsEff r, Member ExcT r, GCEff r) => Text -> Eff r VMObjectNat
newSymbol symbolValue = do
  MkCoreClasses {symbolClass} <- ask
  newObject symbolClass $ \clazz fields -> SymbolObject {clazz, fields, symbolValue}

newBlock :: (UniverseEff r, Lifted IO r) => VMBlock -> Eff r VMObjectNat
newBlock block = do
  -- Capture call frame
  (cf, blockCapturedFrame) <- popCallFrame >>= promoteCallFrame
  modify @CallStackNat $ St.push cf

  MkCoreClasses {block1Class, block2Class, block3Class} <- ask

  cl <- case blockParameterCount block of
    0 -> pure block1Class
    1 -> pure block2Class
    2 -> pure block3Class
    _ -> throwT "Trying to create block with more than 2 parameters"

  newObject cl $ \clazz fields -> BlockObject {clazz, fields, blockCapturedFrame, block}

newArray :: (CoreClassesEff r, GlobalsEff r, Member ExcT r, GCEff r) => Arr.VMArray FieldIx ObjIx -> Eff r VMObjectNat
newArray arrayValue = do
  MkCoreClasses{arrayClass} <- ask
  newObject arrayClass $ \clazz fields -> ArrayObject {clazz, fields, arrayValue}

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

sendMessage :: UniverseEff r => ObjIx -> LiteralIx -> GlobalIx -> Arr.VMArray FieldIx ObjIx -> Eff r ()
sendMessage self methodIx classIx args = do
  clazz <- getClass classIx
  errM <- callErrorMessage methodIx (name clazz)
  method <- findMethod methodIx clazz >>= throwOnNothing errM

  let (pCount, lCount) = case method of
        BytecodeMethod {parameterCount, localCount} -> (parameterCount, localCount)
        NativeMethod {parameterCount} -> (parameterCount, 0)

  nilIx <- getNil

  let argsL = Arr.length args
  let args'
        | argsL < pCount = Arr.toList args ++ replicate (pCount - argsL) nilIx
        | otherwise = drop pCount $ Arr.toList args

  let locals =  Arr.fromList (self : ( args' ++ replicate lCount nilIx ))

  let cf =
        MethodCallFrame
          { method,
            pc = 0,
            locals
          }
  pushCallFrame cf

findMethod :: (GlobalsEff r, Member ExcT r) => LiteralIx -> VMClassNat -> Eff r (Maybe VMMethodNat)
findMethod litIx MkVMClass {methods, superclass} =
  case (superclass, getMethod litIx methods) of
    (_, Just m) -> pure $ Just m
    (Just superIx, Nothing) -> do
      super <- getClass superIx
      findMethod litIx super
    (Nothing, Nothing) -> pure Nothing
