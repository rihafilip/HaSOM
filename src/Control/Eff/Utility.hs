{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility effect functions
module Control.Eff.Utility (modifyEffYield, modifyYield, modifyEff) where

import Control.Eff (Eff, Member)
import Control.Eff.State.Strict (State, get, put)
import Control.Monad ((>=>))

-- | Modify the state in an effect while also returning a value
modifyEffYield :: forall s r b . Member (State s) r => (s -> Eff r (s, b)) -> Eff r b
modifyEffYield f = do
  (x :: s) <- get
  (newX, ret) <- f x
  put newX
  pure ret

-- | Modify the state while also returning a value
modifyYield :: Member (State s) r => (s -> (s, b)) -> Eff r b
modifyYield = modifyEffYield . (pure .)

-- | Modify the state in an effect
modifyEff :: Member (State s) r => (s -> Eff r s) -> Eff r ()
modifyEff = (get >>=) >=> put
