{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Network.Module(
    NetworkT(..)
  ) where

import Control.Monad.Extra (whenJust)
import Control.Monad.Fix
import Control.Monad.State.Strict
import Game.GoreAndAsh
import Game.GoreAndAsh.Network.State
import Network.ENet
import Network.ENet.Host 
import Network.ENet.Peer 
import Network.ENet.Packet (peek)
import qualified Network.ENet.Bindings as B 

newtype NetworkT s m a = NetworkT { runNetworkT :: StateT (NetworkState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (NetworkState s), MonadFix, MonadTrans, MonadIO)

instance GameModule m s => GameModule (NetworkT s m) (NetworkState s) where
  runModule (NetworkT m) s = do 
    ((a, s'), nextState) <- runModule (runStateT m s) (networkNextState s)
    s'' <- case networkHost s' of 
      Nothing -> return s'
      Just h -> processNetEvents s' h
    return (a, s'' {
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

-- | Poll all events from ENet
processNetEvents :: MonadIO m => NetworkState s -> Host -> m (NetworkState s)
processNetEvents nst hst = liftIO $ untilNothing nst (service hst 0) handle
  where
    untilNothing acc f h = do 
      ma <- f 
      case ma of 
        Nothing -> return acc
        Just a -> do
          acc' <- h acc a
          untilNothing acc' f h

    handle s@NetworkState{..} (B.Event et peer ch edata packetPtr) = case et of
      B.None -> do
        putStrLn "Network: Event none"
        return s
      B.Connect -> do 
        putStrLn "Network: Peer connected"
        return $ s {
            networkPeers = peer : networkPeers 
          }
      B.Disconnect -> do 
        putStrLn $ "Network: Peer disconnected, code " ++ show edata
        return $ s {
            networkPeers = filter (/= peer) networkPeers
          }
      B.Receive -> do 
        (Packet fs bs) <- peek packetPtr
        putStrLn $ "Network: Received message at channel " ++ show ch ++ ": "
          ++ show fs ++ ", payload: " ++ show bs
        return s