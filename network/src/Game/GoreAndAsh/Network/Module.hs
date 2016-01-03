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
import Network.ENet.Packet (peek)
import Network.ENet.Peer 
import qualified Data.Foldable as F 
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S
import qualified Network.ENet.Bindings as B 

newtype NetworkT s m a = NetworkT { runNetworkT :: StateT (NetworkState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (NetworkState s), MonadFix, MonadTrans, MonadIO)

instance GameModule m s => GameModule (NetworkT s m) (NetworkState s) where
  type ModuleState (NetworkT s m) = NetworkState s
  
  runModule (NetworkT m) s = do 
    ((a, s'), nextState) <- runModule (runStateT m s) (networkNextState s)
    s'' <- processEvents <=< clearMessages <=< moveDisconnected <=< moveConnected $ s'
    return (a, s'' {
        networkNextState = nextState
      })
    where 
      processEvents s' = case networkHost s' of 
        Nothing -> return s'
        Just h -> processNetEvents s' h

      moveConnected s' = return $ s' {
          networkPeers = networkPeers s' `H.union` H.fromList ((, ()) <$> networkConnectedPeers s')
        , networkConnectedPeers = []
        }

      moveDisconnected s' = return $ s' {
          networkPeers = F.foldl' (flip H.delete) (networkPeers s') $ networkDisconnectedPeers s'
        , networkDisconnectedPeers = []
        }

      clearMessages s' = return $ s' {
          networkMessages = H.empty
        }

  newModuleState = do 
    s <- newModuleState 
    return $ NetworkState {
        networkNextState = s
      , networkHost = Nothing 
      , networkPeers = H.empty
      , networkMessages = H.empty
      , networkDetailedLogging = False
      , networkConnectedPeers = []
      , networkDisconnectedPeers = []
      }

  withModule _ = withENetDo
  cleanupModule NetworkState{..} = do 
    forM_ (H.toList networkPeers) $ \(p, _) -> disconnectNow p 0
    forM_ networkConnectedPeers $ \p -> disconnectNow p 0
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
        when networkDetailedLogging $ putStrLn "Network: Event none"
        return s
      B.Connect -> do 
        when networkDetailedLogging $ putStrLn "Network: Peer connected"
        return $ s {
            networkConnectedPeers = peer : networkConnectedPeers 
          }
      B.Disconnect -> do 
        when networkDetailedLogging $ putStrLn $ "Network: Peer disconnected, code " ++ show edata
        return $ s {
            networkDisconnectedPeers = peer : networkDisconnectedPeers
          }
      B.Receive -> do 
        (Packet fs bs) <- peek packetPtr
        when networkDetailedLogging $ putStrLn $ "Network: Received message at channel " ++ show ch ++ ": "
          ++ show fs ++ ", payload: " ++ show bs
        return $ s {
            networkMessages = case H.lookup (peer, ch) networkMessages of
              Nothing -> H.insert (peer, ch) (S.singleton bs) networkMessages
              Just msgs -> H.insert (peer, ch) (msgs S.|> bs) networkMessages
          }