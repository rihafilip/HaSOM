{-# LANGUAGE FlexibleContexts #-}
-- | Exception effect specialised to Text
module Control.Eff.ExcT(ExcT, throwT, throwOnNothing) where

import Control.Eff (Eff, Member)
import Control.Eff.Exception (Exc, throwError)
import Data.Text ( Text )

-- | Text Exception effect
type ExcT = Exc Text

-- | Throw a Text exception
throwT :: Member ExcT r => Text -> Eff r a
throwT = throwError

-- | Throw an error on Nothing, otherwise return value
throwOnNothing :: Member ExcT r => Text -> Maybe a -> Eff r a
throwOnNothing err = maybe (throwError err) pure
