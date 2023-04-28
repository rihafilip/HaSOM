{-# LANGUAGE FlexibleContexts #-}

-- | Utility functions for manipulating IORef inside Lifted IO effect
module Control.Eff.IO.Utility (lreadIORef, lwriteIORef, lmodifyIORef) where

import Combinator ((...))
import Control.Eff
import Data.IORef (IORef, readIORef, writeIORef)

lreadIORef :: Lifted IO r => IORef a -> Eff r a
lreadIORef = lift . readIORef

lwriteIORef :: Lifted IO r => IORef a -> a -> Eff r ()
lwriteIORef = lift ... writeIORef

lmodifyIORef :: Lifted IO r => IORef a -> (a -> Eff r a) -> Eff r ()
lmodifyIORef ref f = lreadIORef ref >>= f >>= lwriteIORef ref
