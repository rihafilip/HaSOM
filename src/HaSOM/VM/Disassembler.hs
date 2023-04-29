{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.VM.Disassembler
  ( dissasembleBytecodeIns,
    dissasembleBytecode,
    dissasembleMethod,
    dissasembleClass,
    disassembleLiterals,
    disassembleGlobals,
  )
where

import Control.Eff
import Control.Eff.State.Strict (get, evalState)
import Data.Maybe (fromMaybe)
import Data.Text (Text, intercalate, justifyLeft)
import Data.Text.Utility
import HaSOM.VM.Object
import HaSOM.VM.Universe

formatIdx :: VMIx i => i -> Text
formatIdx = justifyLeft 2 '0' . showT . getIx

infixr 5 <->

(<->) :: Text -> Text -> Text
l <-> r = l <+ " " <+ r

infixr 5 <\>

(<\>) :: Text -> Text -> Text
l <\> r = l <+ "\n" <+ r

-----------------------------------------------------------

dissasembleBytecodeIns :: Bytecode -> Text
dissasembleBytecodeIns =
  \case
    HALT -> "HALT"
    DUP -> "DUP"
    POP -> "POP"
    PUSH_LITERAL li -> "PUSH_LITERAL" <-> formatIdx li
    PUSH_LOCAL li li' -> "PUSH_LOCAL" <-> formatIdx li <-> formatIdx li'
    PUSH_FIELD fi -> "PUSH_FIELD" <-> formatIdx fi
    PUSH_GLOBAL gi -> "PUSH_GLOBAL" <-> formatIdx gi
    SET_LOCAL li li' -> "SET_LOCAL" <-> formatIdx li <-> formatIdx li'
    SET_FIELD fi -> "SET_FIELD" <-> formatIdx fi
    SET_GLOBAL gi -> "SET_GLOBAL" <-> formatIdx gi
    CALL li -> "CALL" <-> formatIdx li
    SUPER_CALL li -> "SUPER_CALL" <-> formatIdx li
    RETURN -> "RETURN"
    NONLOCAL_RETURN -> "NONLOCAL_RETURN"

dissasembleBytecode :: Code -> Text
dissasembleBytecode = foldl (<\>) "" . zipWith f [0 ..] . codeAsList
  where
    f :: Int -> Bytecode -> Text
    f idx ins =
      justifyLeft 4 '0' (showT idx)
        <-> dissasembleBytecodeIns ins

-----------------------------------------------------------
-- stackTrace :: (GCEff r, Lifted IO r) => CallStackNat -> Eff r [Text]
-- stackTrace = St.pop .> \case
--    Nothing -> pure []
--    Just (st, it) -> do
--     cf <- stackItem it
--     rest <- stackTrace st
--     pure (cf : rest)
--   where
--     stackItem (PureCallFrame cf) = callFrame cf
--     stackItem (ReferenceCallFrame cf) = lreadIORef cf >>= callFrame
--     callFrame cf = do
--       let m = method cf
--       let (ptr, sig) = case m of
--             BytecodeMethod{signature} -> (showT $ getIx $ pc cf, signature)
--             NativeMethod{signature} -> ("<primitive>", signature)
--       _

dissasembleMethod :: Text -> VMMethodNat -> Text
dissasembleMethod name BytecodeMethod {body, parameterCount, localCount} =
  "Name :"
    <+ name
    <\> "Local count: "
    <+ showT localCount
    <\> "Parameters count: "
    <+ showT parameterCount
    <\> "Body:"
    <\> dissasembleBytecode body
dissasembleMethod name NativeMethod {parameterCount} =
  "Name :"
    <+ name
    <\> "Parameters count: "
    <+ showT parameterCount
    <\> "Body:\n<native>"

dissasembleClass :: (GlobalsEff r, LiteralEff r) => VMClassNat -> Eff r Text
dissasembleClass MkVMClass {fieldsCount, superclass, methods} = do
  superclassT <- maybe (pure "none") className superclass
  ms <- mapM (\(idx, m) -> (,m) <$> literalName idx) $ methodsAsList methods

  pure
    ( "Fields count: "
        <+ showT fieldsCount
        <\> "Superclass: "
        <+ superclassT
        <\> "Methods:"
        <\> intercalate "\n\n" (map (uncurry dissasembleMethod) ms)
    )

className :: GlobalsEff r => GlobalIx -> Eff r Text
className idx = fromMaybe "??" . getGlobalName idx <$> get @VMGlobalsNat

literalName :: LiteralEff r => LiteralIx -> Eff r Text
literalName idx = maybe "??" showT . getLiteral idx <$> get

formatList :: VMIx i => [(i, Text, Text)] -> Text
formatList = intercalate "\n\n" . map f
  where
    f (idx, name, x) = formatIdx idx <+ ": " <+ name <\> x

disassembleLiterals :: VMLiterals -> Text
disassembleLiterals =
  ("Literals:" <+)
    . formatList
    . map (\(a, b, c) -> (a, b, showT c))
    . literalsToList

disassembleGlobals :: LiteralEff r => VMGlobalsNat -> Eff r Text
disassembleGlobals gs = ("Globals:" <+) . formatList <$> mapM f (globalsToList gs)
  where
    f (a, b, ClassGlobal cl) = (a,b,) <$> evalState gs (dissasembleClass cl)
    f (a, b, ObjectGlobal oi) = pure (a, b, "object " <+ showT (getIx oi))
