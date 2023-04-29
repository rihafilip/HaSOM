{-# LANGUAGE FlexibleContexts #-}

-- | Code execution methods
module HaSOM.VM.Interpreter (interpret, bootstrap) where

import Control.Eff (Eff, Lifted)
import Control.Eff.ExcT (throwOnNothing)
import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text.Utility (showT, (<+))
import HaSOM.VM.Object
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Instructions
import HaSOM.VM.Universe.Operations (addToGC, getCurrentCallFrame, getGlobalE, internGlobalE, internLiteralE, newArray, newString, pushCallFrame, pushStack)
import qualified HaSOM.VM.VMArray as Arr

-- | Run the interpreter
interpret :: (Lifted IO r, UniverseEff r) => Eff r Int
interpret = do
  cf <- getCurrentCallFrame

  r <- case method cf of
    BytecodeMethod {body} ->
      throwOnNothing
        ("Index " <+ showT (pc cf) <+ " fell out of code block")
        (getInstruction (pc cf) body)
        >>= executeInstruction
    NativeMethod {nativeBody} -> Nothing <$ runNativeFun nativeBody

  maybe interpret pure r

executeInstruction :: (Lifted IO r, UniverseEff r) => Bytecode -> Eff r (Maybe Int)
executeInstruction HALT = Just <$> doHalt
executeInstruction bc =
  Nothing <$ case bc of
    DUP -> doDup
    POP -> doPop
    PUSH_LITERAL li -> doPushLiteral li
    PUSH_LOCAL env li -> doPushLocal env li
    PUSH_FIELD fi -> doPushField fi
    PUSH_GLOBAL gi -> doPushGlobal gi
    SET_LOCAL env li -> doSetLocal env li
    SET_FIELD fi -> doSetField fi
    SET_GLOBAL gi -> doSetGlobal gi
    CALL li -> doCall li
    SUPER_CALL li -> doSupercall li
    RETURN -> doReturn
    NONLOCAL_RETURN -> doNonlocalReturn

-- | Bootstrap the System>>#initialize: method with command line arguments
bootstrap :: UniverseEff r => Text -> [Text] -> Eff r ()
bootstrap clazz args = do
  argsIdxs <- mapM (newString >=> addToGC) (clazz : args)
  arrIdx <- newArray (Arr.fromList argsIdxs) >>= addToGC
  systemIdx <-
    internGlobalE "system" >>= getGlobalE <&> \case
      ClassGlobal MkVMClass {asObject} -> asObject
      ObjectGlobal oi -> oi

  pushStack arrIdx
  pushStack systemIdx

  methodIdx <- internLiteralE (SymbolLiteral "initialize:")
  let body = code [CALL methodIdx, HALT]

  let bootstrapM = BytecodeMethod {body, parameterCount = 0, localCount = 0}
  pushCallFrame $ MethodCallFrame {method = bootstrapM, pc = 0, locals = Arr.fromList []}
