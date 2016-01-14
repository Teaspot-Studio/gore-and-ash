{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.SDL.Module(
    SDLT(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Fix 
import Control.Monad.State.Strict

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL.State

newtype SDLT s m a = SDLT { runSDLT :: StateT (SDLState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (SDLState s), MonadFix, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance GameModule m s => GameModule (SDLT s m) (SDLState s) where 
  type ModuleState (SDLT s m) = SDLState s
  runModule (SDLT m) s = do
    ((a, s'), nextState) <- runModule (runStateT m s) (sdlNextState s)
    return (a, flashSDLState $ s' { 
        sdlNextState = nextState
      })

  newModuleState = emptySDLState <$> newModuleState
  withModule _ = id
  cleanupModule _ = return ()