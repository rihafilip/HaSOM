-- | Helper functions on Text data type
module Data.Text.Utility ((<+), showT) where
import Data.Text (Text, append, pack)

-- | Append operator
infixr 5 <+
(<+) :: Text -> Text -> Text
(<+) = append

-- | Show to Text
showT :: Show a => a -> Text
showT = pack . show
