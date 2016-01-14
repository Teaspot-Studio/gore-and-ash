module Game.GoreAndAsh.SDL.API(
    SDLMonad(..)
  ) where

import Control.Monad.State.Strict
-- import Control.Wire
import Prelude hiding (id, (.))

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL.State
import Game.GoreAndAsh.SDL.Module

-- | Low level API for module
class MonadIO m => SDLMonad m where 
  sdlStub :: m ()

instance {-# OVERLAPPING #-} MonadIO m => SDLMonad (SDLT s m) where
  sdlStub = return ()

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), SDLMonad m, MonadTrans mt) => SDLMonad (mt m) where 
  sdlStub = lift sdlStub