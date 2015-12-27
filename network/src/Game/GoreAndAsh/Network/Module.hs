{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Network.Module(
    NetworkT(..)
  ) where

import Control.Monad.Extra (whenJust)
import Control.Monad.Fix
import Control.Monad.State.Strict
import Network.ENet
import Network.ENet.Host 
import Network.ENet.Peer 

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
      , networkHost = Nothing 
      , networkPeers = []
      }

  withModule _ = withENetDo
  cleanupModule NetworkState{..} = do 
    forM_ networkPeers $ \p -> disconnectNow p 0
    whenJust networkHost destroy