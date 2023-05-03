{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | VM Instructions implementation
module HaSOM.VM.Universe.Instructions
  ( doDup,
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
  )
where

import Combinator ((.>))
import Control.Eff.IO.Utility (lreadIORef)
import Control.Monad (void, (>=>))
import Data.Functor ((<&>))
import qualified Data.Stack as St
import HaSOM.VM.Object
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations
import qualified HaSOM.VM.VMArray as Arr
import Data.Text.Utility ((<+))

---------------------------------------------------------------
-- Simple operations

doDup :: (ObjStackEff r, Member ExcT r) => Eff r ()
doDup = topStack >>= pushStack

doPop :: (ObjStackEff r, Member ExcT r) => Eff r ()
doPop = void popStack

---------------------------------------------------------------
-- Pushing on stack

doPushLiteral :: (UniverseEff r, Lifted IO r) => LiteralIx -> Eff r ()
doPushLiteral = doGetLiteral >=> pushStack

doGetLiteral :: (UniverseEff r, Lifted IO r) => LiteralIx -> Eff r ObjIx
doGetLiteral li = do
  lit <- getLiteralE li

  -- Create literal object
  obj <- case lit of
    IntLiteral value -> newInt value
    DoubleLiteral value -> newDouble value
    StringLiteral value -> newString value
    SymbolLiteral value -> newSymbol value
    ArrayLiteral value -> mapM doGetLiteral value >>= newArray . Arr.fromList
    BlockLiteral value -> newBlock value

  addToGC obj


doPushLocal :: (Member ExcT r, CallStackEff r, ObjStackEff r, Lifted IO r) => LocalIx -> LocalIx -> Eff r ()
doPushLocal env li = getLocal env li >>= pushStack

doPushField :: (CallStackEff r, GCEff r, Member ExcT r, ObjStackEff r, Lifted IO r) => FieldIx -> Eff r ()
doPushField fi = getFieldE fi >>= pushStack

doPushGlobal :: (ObjStackEff r, GlobalsEff r, Member ExcT r) => GlobalIx -> Eff r ()
doPushGlobal gi = getGlobalE gi >>= pushStack . transform
  where
    transform (ClassGlobal MkVMClass {asObject}) = asObject
    transform (ObjectGlobal oi) = oi

---------------------------------------------------------------
-- Setting values

doSetLocal :: (CallStackEff r, ObjStackEff r, Member ExcT r, Lifted IO r) => LocalIx -> LocalIx -> Eff r ()
doSetLocal env li = popStack >>= setLocal env li

doSetField :: (ObjStackEff r, CallStackEff r, GCEff r, Member ExcT r, Lifted IO r) => FieldIx -> Eff r ()
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

doSupercall :: (Lifted IO r, UniverseEff r) => LiteralIx -> Eff r ()
doSupercall li = do
  holder <- methodHolder<$> getCurrentCallFrame
  thisIx <- popStack
  superclass <-
    throwOnNothing ("No superclass in class " <+ name holder) (superclass holder)
      >>= getClass

  doCallUnified thisIx li superclass

doCallUnified :: UniverseEff r => ObjIx -> LiteralIx -> VMClassNat -> Eff r ()
doCallUnified thisIx li clazz = do
  errM <- callErrorMessage li (name clazz)
  (methodHolder, method) <- findMethod li clazz >>= throwOnNothing errM

  let (pCount, lCount) = case method of
        BytecodeMethod {parameterCount, localCount} -> (parameterCount, localCount)
        NativeMethod {parameterCount} -> (parameterCount, 0)

  localsL <- createLocals thisIx pCount lCount
  callStackHeight <- St.size <$> get @CallStackNat
  let cf =
        MethodCallFrame
          { methodHolder,
            method,
            pc = 0,
            locals = Arr.fromList localsL,
            callStackHeight
          }
  pushCallFrame cf

---------------------------------------------------------------
-- Returning

doReturn :: (CallStackEff r, Member ExcT r) => Eff r ()
doReturn = void popCallFrame

doNonlocalReturn :: (CallStackEff r, Member ExcT r, Lifted IO r) => Eff r ()
doNonlocalReturn = do
  cf <- popCallFrame >>= getCallFrame
  target <- getTarget cf >>= throwOnNothing "Non-local return called inside of method"
  newStack <-
    get @CallStackNat
      <&> popWhile target
      >>= throwOnNothing "Captured call frame has escaped block context"
  put newStack
  where
    popWhile ref =
      St.pop .> \case
        Nothing -> Nothing
        Just (st', ReferenceCallFrame cf) | cf == ref -> Just st'
        Just (st', _) -> popWhile ref st'

    getTarget BlockCallFrame {capturedFrame} =
      lreadIORef capturedFrame
        >>= \case
          MethodCallFrame {} -> pure (Just capturedFrame)
          cf@BlockCallFrame {} -> getTarget cf
    getTarget _ = pure Nothing
