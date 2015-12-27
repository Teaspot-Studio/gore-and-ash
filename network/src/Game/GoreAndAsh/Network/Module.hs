{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Network.Module(
    NetworkT(..)
  ) where

import Control.Monad.Fix
import Control.Monad.State.Strict

import Game.GoreAndAsh
import Game.GoreAndAsh.Network.State 

newtype NetworkT s m a = NetworkT { runNetworkT :: StateT (NetworkState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (NetworkState s), MonadFix, MonadTrans, MonadIO)

instance GameModule m s => GameModule (NetworkT s m) (NetworkState s) where
    runModule (NetworkT m) s = do 
      ((a, s'), nextState) <- runModule (runStateT m s) (networkNextState s)
      return (a, s' {
          networkNextState = nextState
        })
    
    newModuleState = do 
      s <- newModuleState 
      return $ NetworkState {
          networkNextState = s
        }