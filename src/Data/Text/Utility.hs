module Data.Text.Utility ((<+)) where
import Data.Text (Text, append)

-- | Append operator
infixr 5 <+
(<+) :: Text -> Text -> Text
(<+) = append
