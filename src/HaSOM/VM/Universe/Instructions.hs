{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | VM Instructions implementation
module HaSOM.VM.Universe.Instructions(
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
) where

import Control.Monad (void)
import HaSOM.VM.Object
import HaSOM.VM.Universe
import qualified HaSOM.VM.VMArray as Arr
import HaSOM.VM.Universe.Operations

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

  -- Create literal object
  obj <- case lit of
    IntLiteral value -> newInt value
    DoubleLiteral value -> newDouble value
    StringLiteral value -> newString value
    SymbolLiteral value -> newSymbol value
    BlockLiteral vb -> undefined -- TODO

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

  let (pCount, lCount) = case method of
        BytecodeMethod {parameterCount, localCount} -> (parameterCount, localCount)
        NativeMethod {parameterCount} -> (parameterCount, 0)

  localsL <- createLocals thisIx pCount lCount

  let cf =
        MethodCallFrame
          { method,
            pc = 0,
            locals = Arr.fromList localsL,
            frameId = Nothing
          }
  pushCallFrame cf

---------------------------------------------------------------
-- Returning

doReturn :: (CallStackEff r, Member ExcT r) => Eff r ()
doReturn = void popCallFrame

doNonlocalReturn :: Eff r ()
doNonlocalReturn = undefined -- TODO

---------------------------------------------------------------

findMethod :: (GlobalsEff r, Member ExcT r) => LiteralIx -> VMClassNat -> Eff r (Maybe VMMethodNat)
findMethod litIx MkVMClass {methods, superclass} =
  case (superclass, getMethod litIx methods) of
    (_, Just m) -> pure $ Just m
    (Just superIx, Nothing) -> do
      super <- getClass superIx
      findMethod litIx super
    (Nothing, Nothing) -> pure Nothing
