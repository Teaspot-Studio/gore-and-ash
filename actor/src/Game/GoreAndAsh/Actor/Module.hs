module Game.GoreAndAsh.Actor.Module(
    ActorT(..)
  ) where

import Control.Monad.Fix 
import Control.Monad.State.Strict

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor.State

newtype ActorT s m a = ActorT { runActorT :: StateT (ActorState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (ActorState s), MonadFix, MonadTrans, MonadIO)

instance GameModule m s => GameModule (ActorT s m) (ActorState s) where 
  type ModuleState (ActorT s m) = ActorState s
  runModule (ActorT m) s = do
    ((a, s'), nextState) <- runModule (runStateT m s) (actorNextState s)
    return (a, s' { 
       actorNextState = nextState 
      })

  newModuleState = do
    s <- newModuleState
    return $ ActorState {
        actorNextState = s
      }

  withModule _ = id
  cleanupModule _ = return ()