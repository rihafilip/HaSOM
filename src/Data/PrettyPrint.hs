{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.PrettyPrint where

import Control.Eff (Eff, type (<::))
import Control.Eff.Reader.Strict
import Control.Eff.Writer.Strict
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Utility

type PrettyPrintEff r = [Reader Int, Writer Text] <:: r

runPrettyPrint :: Eff (Reader Int : Writer Text : r) a -> Eff r Text
runPrettyPrint =
  (T.unlines <$>)
    . (execListWriter @Text)
    . runReader (0 :: Int)

indented :: PrettyPrintEff r => Eff r () -> Eff r ()
indented f = do
  (currIndent :: Int) <- ask
  local (const $ currIndent + 1) f

addLine :: PrettyPrintEff r => Text -> Eff r ()
addLine str = do
  (indentSize :: Int) <- ask
  let indentatiton = T.replicate indentSize "  "
  tell $ indentatiton <+ str

(.:) :: PrettyPrintEff r => Text -> Text -> Eff r ()
name .: value = addLine (name <+ ": " <+ value)

-- | Print bracketed single line
(<#>) :: PrettyPrintEff r => Text -> Text -> Eff r ()
name <#> field = addLine $ "(" <+ name <+ " " <+ field <+ ")"

formatList :: [Text] -> Text
formatList list = "[ " <+ T.unwords list <+ " ]"

quote :: Text -> Text
quote = ("\"" <+) . (<+ "\"")
