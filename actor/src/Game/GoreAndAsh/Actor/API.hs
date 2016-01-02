module Game.GoreAndAsh.Actor.API(
    ActorMonad(..)
  ) where

import Control.Monad.State.Strict
import Control.Wire
import Prelude hiding (id, (.))

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor.State
import Game.GoreAndAsh.Actor.Module

-- | Low level API for module
class Monad m => ActorMonad m where 
  actorDummy :: m ()

instance {-# OVERLAPPING #-} Monad m => ActorMonad (ActorT s m) where
  actorDummy = return ()

instance {-# OVERLAPPABLE #-} (Monad (mt m), ActorMonad m, MonadTrans mt) => ActorMonad (mt m) where 
  actorDummy = lift actorDummy