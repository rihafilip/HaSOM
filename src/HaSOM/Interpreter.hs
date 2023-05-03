{-# LANGUAGE FlexibleContexts #-}

-- | Code execution methods
module HaSOM.Interpreter (interpret, bootstrap) where

import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Data.PrettyPrint (runPrettyPrint)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Utility (showT, (<+))
import HaSOM.VM.Disassembler (disassembleBytecodeInsSimple)
import HaSOM.VM.Object
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Instructions
import HaSOM.VM.Universe.Operations
import qualified HaSOM.VM.VMArray as Arr

-- | Run the interpreter
interpret :: (Lifted IO r, UniverseEff r, TraceEff r) => Eff r Int
interpret = do
  cf <- getCurrentCallFrame

  r <- case method cf of
    BytecodeMethod {signature, body} -> do
      ins <-
        throwOnNothing
          ("Index " <+ showT (pc cf) <+ " fell out of code block")
          (getInstruction (pc cf) body)
      advancePC
      executeInstruction ins
    NativeMethod {signature, nativeBody} -> do
      runNativeFun nativeBody
      ask >>= \case
        NoTrace -> pure ()
        DoTrace -> do
          lift $ TIO.putStrLn ("Entering: " <+ signature)
      pure Nothing

  maybe interpret pure r

executeInstruction :: (Lifted IO r, UniverseEff r) => Bytecode -> Eff r (Maybe Int)
executeInstruction HALT = pure $ Just 0
executeInstruction bc = do
  case bc of
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
  pure Nothing

-- | Bootstrap the System>>#initialize: method with command line arguments
bootstrap :: UniverseEff r => Text -> [Text] -> Eff r ()
bootstrap mainClass args = do
  argsIdxs <- mapM (newString >=> addToGC) (mainClass : args)
  arrIdx <- newArray (Arr.fromList argsIdxs) >>= addToGC
  systemIdx <-
    internGlobalE "system" >>= getGlobalE <&> \case
      ClassGlobal MkVMClass {asObject} -> asObject
      ObjectGlobal oi -> oi

  methodHolder <- clazz <$> getAsObject systemIdx

  pushStack arrIdx
  pushStack systemIdx

  methodIdx <- internLiteralE (SymbolLiteral "initialize:")
  let body = code [CALL methodIdx, HALT]

  let bootstrapM =
        BytecodeMethod
          { signature = "System>>#bootstrap",
            body,
            parameterCount = 0,
            localCount = 0
          }
  pushCallFrame $
    MethodCallFrame
      { methodHolder,
        method = bootstrapM,
        pc = 0,
        locals = Arr.fromList [],
        callStackHeight = 0
      }
