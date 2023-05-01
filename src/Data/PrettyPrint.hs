{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A pretty printing effect
module Data.PrettyPrint
  ( -- * Run formating
    PrettyPrintEff,
    runPrettyPrint,

    -- * Basic interface
    indented,
    addLine,

    -- * Formating helpers
    (.:),
    (<#>),
    formatList,
    quote,
  )
where

import Control.Eff (Eff, type (<::))
import Control.Eff.Reader.Strict
import Control.Eff.Writer.Strict
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Utility

-- | The pretty printing effect
type PrettyPrintEff r = [Reader Int, Writer Text] <:: r

-- | Run pretty printing
runPrettyPrint :: Eff (Reader Int : Writer Text : r) a -> Eff r Text
runPrettyPrint =
  (T.unlines <$>)
    . (execListWriter @Text)
    . runReader (0 :: Int)

-- | Add one level of indentation
indented :: PrettyPrintEff r => Eff r () -> Eff r ()
indented f = do
  (currIndent :: Int) <- ask
  local (const $ currIndent + 1) f

-- | Output a line
addLine :: PrettyPrintEff r => Text -> Eff r ()
addLine str = do
  (indentSize :: Int) <- ask
  let indentatiton = T.replicate indentSize "  "
  tell $ indentatiton <+ str

-- | "a" .: "b" prints as "a: b"
(.:) :: PrettyPrintEff r => Text -> Text -> Eff r ()
name .: value = addLine (name <+ ": " <+ value)

-- | Print bracketed single line
(<#>) :: PrettyPrintEff r => Text -> Text -> Eff r ()
name <#> field = addLine $ "(" <+ name <+ " " <+ field <+ ")"

-- | Format a list
formatList :: [Text] -> Text
formatList list = "[ " <+ T.unwords list <+ " ]"

-- | Quote given text
quote :: Text -> Text
quote = ("\"" <+) . (<+ "\"")
