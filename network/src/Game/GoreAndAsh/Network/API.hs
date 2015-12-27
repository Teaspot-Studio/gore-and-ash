module Game.GoreAndAsh.Network.API(
    NetworkMonad(..)
  ) where

import Control.Monad.Trans 

import Game.GoreAndAsh
-- import Game.GoreAndAsh.Network.State
import Game.GoreAndAsh.Network.Module

class Monad m => NetworkMonad m where
  networkDummy :: m ()

instance Monad m => NetworkMonad (NetworkT s m) where
  networkDummy = return ()

instance NetworkMonad m => NetworkMonad (GameMonadT m) where
  networkDummy = lift networkDummy