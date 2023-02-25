module Data.Text.Utility ((<+)) where
import Data.Text (Text, append)

infixr 5 <+
(<+) :: Text -> Text -> Text
(<+) = append
