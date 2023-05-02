{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.VM.Disassembler
  ( disassembleLiterals,
    disassembleGlobals,
    disassembleClassSimple,
  )
where

import Combinator ((.>))
import Control.Eff (Eff)
import Control.Eff.State.Strict (evalState, get)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.PrettyPrint
import Data.Text (Text, justifyRight)
import Data.Text.Utility
import HaSOM.VM.Object
import HaSOM.VM.Universe

formatIdx :: VMIx i => i -> Text
formatIdx = justifyRight 4 '0' . showT . getIx

infixr 5 <->

(<->) :: Text -> Text -> Text
l <-> r = l <+ " " <+ r

-----------------------------------------------------------

literalName :: (LiteralEff r) => LiteralIx -> Eff r Text
literalName idx = maybe "??" showT . getLiteral idx <$> get

globalName :: GlobalsEff r => GlobalIx -> Eff r Text
globalName idx = fromMaybe "??" . getGlobalName idx <$> get @VMGlobalsNat

className :: GlobalsEff r => GlobalIx -> Eff r Text
className idx = fromMaybe "??" . getGlobalName idx <$> get @VMGlobalsNat

-----------------------------------------------------------

formatBlock :: (LiteralEff r, GlobalsEff r, PrettyPrintEff r) => VMBlock -> Eff r ()
formatBlock MkVMBlock {blockBody, blockParameterCount, blockLocalCount} = do
  "Parameters count" .: showT blockParameterCount
  "Locals count" .: showT blockLocalCount
  disassembleBytecode_ blockBody

-----------------------------------------------------------

insWithComment :: VMIx i => Text -> i -> Text -> Text
insWithComment ins idx comment = ins <-> formatIdx idx <-> ":" <+ comment

disassembleBytecodeIns :: (GlobalsEff r, LiteralEff r) => Bytecode -> Eff r Text
disassembleBytecodeIns (PUSH_LITERAL li) =
  insWithComment "PUSH_LITERAL" li
    <$> literalName li
disassembleBytecodeIns (PUSH_GLOBAL gi) =
  insWithComment "PUSH_GLOBAL" gi
    <$> globalName gi
disassembleBytecodeIns (SET_GLOBAL gi) =
  insWithComment "SET_GLOBAL" gi
    <$> globalName gi
disassembleBytecodeIns (CALL li) =
  insWithComment "CALL" li
    <$> literalName li
disassembleBytecodeIns (SUPER_CALL li) =
  insWithComment "SUPER_CALL" li
    <$> literalName li
disassembleBytecodeIns (PUSH_LOCAL 0 0) =
  pure $
    "PUSH_LOCAL"
      <-> formatIdx (ix @LocalIx 0)
      <-> formatIdx (ix @LocalIx 0)
      <-> ":self"
disassembleBytecodeIns bc =
  pure $ case bc of
    HALT -> "HALT"
    DUP -> "DUP"
    POP -> "POP"
    PUSH_LOCAL li li' -> "PUSH_LOCAL" <-> formatIdx li <-> formatIdx li'
    PUSH_FIELD fi -> "PUSH_FIELD" <-> formatIdx fi
    SET_LOCAL li li' -> "SET_LOCAL" <-> formatIdx li <-> formatIdx li'
    SET_FIELD fi -> "SET_FIELD" <-> formatIdx fi
    RETURN -> "RETURN"
    NONLOCAL_RETURN -> "NONLOCAL_RETURN"

disassembleBytecode_ :: (GlobalsEff r, LiteralEff r, PrettyPrintEff r) => Code -> Eff r ()
disassembleBytecode_ = indented . mapM_ (>>= addLine) . zipWith f [0 ..] . codeAsList
  where
    f :: (GlobalsEff r, LiteralEff r) => Int -> Bytecode -> Eff r Text
    f idx ins = do
      insT <- disassembleBytecodeIns ins
      pure $ justifyRight 4 '0' (showT idx) <-> insT

-----------------------------------------------------------

disassembleMethod_ :: (GlobalsEff r, LiteralEff r, PrettyPrintEff r) => Text -> VMMethodNat -> Eff r ()
disassembleMethod_ name BytecodeMethod {body, parameterCount, localCount} = do
  "Name" .: name
  "Local count" .: showT localCount
  "Parameters count" .: showT parameterCount
  addLine "Body:"
  indented $ disassembleBytecode_ body
disassembleMethod_ name NativeMethod {parameterCount} = do
  "Name" .: name
  "Parameters count" .: showT parameterCount
  "Body" .: "<native>"

-----------------------------------------------------------

disassembleClass_ :: (GlobalsEff r, LiteralEff r, PrettyPrintEff r) => VMClassNat -> Eff r ()
disassembleClass_ MkVMClass {fieldsCount, superclass, methods} = do
  superclassT <- maybe (pure "none") className superclass
  ms <- mapM (\(idx, m) -> (,m) <$> literalName idx) $ methodsAsList methods

  "Superclass" .: quote superclassT
  "Fields count" .: showT fieldsCount
  addLine "Methods:"
  indented $ mapM_ (\(n, m) -> disassembleMethod_ n m >> addLine "") ms

-----------------------------------------------------------

disassembleLiterals :: (GlobalsEff r, LiteralEff r) => VMLiterals -> Eff r Text
disassembleLiterals =
  runPrettyPrint . \lits -> do
    addLine "Literals:"
    indented $
      mapM_ (uncurry transf) $
        sortOn fst $
          literalsToList lits
  where
    transf idx (BlockLiteral blck) = do
      formatIdx idx .: "<block>"
      indented $ formatBlock blck
    transf idx lit = formatIdx idx .: showT lit

disassembleGlobals :: LiteralEff r => VMGlobalsNat -> Eff r Text
disassembleGlobals =
  runPrettyPrint . \gs -> do
    addLine "Globals:"
    indented $ mapM_ (evalState gs . f) $ sortOn (\(x, _, _) -> x) $ globalsToList gs
  where
    f (idx, name, Just (ClassGlobal cl)) = do
      showT idx .: quote name
      indented $ disassembleClass_ cl
      addLine ""
    f (idx, name, Just (ObjectGlobal oi)) = do
      showT idx .: quote name
      indented $ "GlobalObject" .: showT oi
      addLine ""
    f (idx, name, Nothing) = do
      showT idx .: quote name
      addLine "<nil>"

-----------------------------------------------------------

disassembleClassSimple :: (GlobalsEff r, LiteralEff r) => Text -> Eff r Text
disassembleClassSimple name = runPrettyPrint $ do
  "Name" .: name
  get @VMGlobalsNat >>= \globs -> case getGlobal (snd $ internGlobal name globs) globs of
    Just (ClassGlobal MkVMClass {fieldsCount, superclass, methods}) -> do
      superclassT <- maybe (pure "none") className superclass
      ms <- mapM (\(midx, m) -> (,m) <$> literalName midx) $ methodsAsList methods

      "Superclass" .: quote superclassT
      "Fields count" .: showT fieldsCount
      addLine "Methods:"
      indented $ mapM_ (\(n, m) -> disassembleMethodSimple n m >> addLine "") ms
    _ -> addLine "Not a class"

disassembleMethodSimple :: (GlobalsEff r, LiteralEff r, PrettyPrintEff r) => Text -> VMMethodNat -> Eff r ()
disassembleMethodSimple name BytecodeMethod {body, parameterCount, localCount} = do
  "Name" .: name
  "Local count" .: showT localCount
  "Parameters count" .: showT parameterCount
  addLine "Body:"
  indented $ disassembleBytecodeSimple body
disassembleMethodSimple name NativeMethod {parameterCount} = do
  "Name" .: name
  "Parameters count" .: showT parameterCount
  "Body" .: "<native>"

disassembleBytecodeSimple :: (GlobalsEff r, LiteralEff r, PrettyPrintEff r) => Code -> Eff r ()
disassembleBytecodeSimple = mapM_ disassembleBytecodeInsSimple . codeAsList

disassembleBytecodeInsSimple :: (GlobalsEff r, LiteralEff r, PrettyPrintEff r) => Bytecode -> Eff r ()
disassembleBytecodeInsSimple (PUSH_LITERAL li) = do
  get @VMLiterals >>= getLiteral li .> \case
    Just (BlockLiteral MkVMBlock {blockBody, blockParameterCount, blockLocalCount}) -> do
      addLine $
        "PUSH_LITERAL"
          <-> "<block>"
          <-> "params:"
          <-> showT blockParameterCount
          <-> "locals:"
          <-> showT blockLocalCount
      indented $ disassembleBytecodeSimple blockBody
    Nothing -> addLine $ "PUSH_LITERAL" <-> "??"
    Just lit -> addLine $ "PUSH_LITERAL" <-> showT lit
disassembleBytecodeInsSimple (PUSH_GLOBAL gi) = globalName gi >>= ("PUSH_GLOBAL" <->) .> addLine
disassembleBytecodeInsSimple (SET_GLOBAL gi) = globalName gi >>= ("SET_GLOBAL" <->) .> addLine
disassembleBytecodeInsSimple (CALL li) = literalName li >>= ("CALL" <->) .> addLine
disassembleBytecodeInsSimple (SUPER_CALL li) = literalName li >>= ("SUPER_CALL" <->) .> addLine
disassembleBytecodeInsSimple (PUSH_LOCAL 0 0) = addLine $ "PUSH_LOCAL" <-> ":self"
disassembleBytecodeInsSimple bc =
  addLine $ case bc of
    HALT -> "HALT"
    DUP -> "DUP"
    POP -> "POP"
    PUSH_LOCAL li li' -> "PUSH_LOCAL" <-> formatIdx li <-> formatIdx li'
    PUSH_FIELD fi -> "PUSH_FIELD" <-> formatIdx fi
    SET_LOCAL li li' -> "SET_LOCAL" <-> formatIdx li <-> formatIdx li'
    SET_FIELD fi -> "SET_FIELD" <-> formatIdx fi
    RETURN -> "RETURN"
    NONLOCAL_RETURN -> "NONLOCAL_RETURN"
