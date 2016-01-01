module Game.GoreAndAsh.Network.API(
    NetworkMonad(..)
  , peersConnected
  , peers
  ) where

import Control.DeepSeq 
import Control.Monad.State.Strict
import Control.Wire.Core 
import Control.Wire.Unsafe.Event 
import Data.Monoid ((<>))
import Data.Text
import Foreign
import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network.Module
import Game.GoreAndAsh.Network.State
import Network.ENet.Host
import Network.Socket (SockAddr)
import qualified Data.HashMap.Strict as H 

class MonadIO m => NetworkMonad m where
  networkBind :: LoggingMonad m => Maybe SockAddr -- ^ Address to listen, Nothing is client
    -> Word -- ^ Maximum count of connections
    -> Word -- ^ Number of channels to open
    -> Word32 -- ^ Incoming max bandwidth
    -> Word32 -- ^ Outcoming max bandwidth
    -> m ()

  -- | Returns peers that were connected during last frame
  peersConnectedM :: m [Peer]

  -- | Initiate connection to the remote host
  networkConnect :: LoggingMonad m => SockAddr -- ^ Address of host
    -> Word -- ^ Count of channels to open
    -> Word32 -- ^ Additional data (0 default)
    -> m (Maybe ())

  -- | Returns list of currently connected peers (servers on client side, clients on server side)
  networkPeersM :: m [Peer]

instance {-# OVERLAPPING #-} MonadIO m => NetworkMonad (NetworkT s m) where
  networkBind addr conCount chanCount inBandth outBandth = do
    nstate <- NetworkT get 
    phost <- liftIO $ create addr (fromIntegral conCount) (fromIntegral chanCount) inBandth outBandth
    if phost == nullPtr
      then case addr of 
        Nothing -> putMsgLnM "Network: failed to initalize client side"
        Just a -> putMsgLnM $ "Network: failed to connect to " <> pack (show a)
      else do
        putMsgLnM $ case addr of 
          Nothing -> "Network: client network system initalized"
          Just a -> "Network: binded to " <> pack (show a)
        NetworkT $ put $ nstate {
            networkHost = Just phost
          }

  peersConnectedM = do 
    NetworkState{..} <- NetworkT get 
    return networkConnectedPeers

  networkConnect addr chanCount datum = do 
    nstate <- NetworkT get 
    case networkHost nstate of 
      Nothing -> do 
        putMsgLnM $ "Network: cannot connect to " <> pack (show addr) <> ", system isn't initalized"
        return $ Just ()
      Just host -> do
        peer <- liftIO $ connect host addr (fromIntegral chanCount) datum 
        if peer == nullPtr
          then do
            putMsgLnM $ "Network: failed to connect to " <> pack (show addr)
            return Nothing
          else return $ Just ()

  networkPeersM = do 
    NetworkState{..} <- NetworkT get 
    return $ H.keys networkPeers  

instance {-# OVERLAPPABLE #-} (Monad (mt m), MonadIO (mt m), LoggingMonad m, NetworkMonad m, MonadTrans mt) => NetworkMonad (mt m) where 
  networkBind a mc mch ib ob = lift $ networkBind a mc mch ib ob
  peersConnectedM = lift peersConnectedM
  networkConnect a b c = lift $ networkConnect a b c 
  networkPeersM = lift networkPeersM

-- | Fires when one or several clients were connected
peersConnected :: (LoggingMonad m, NetworkMonad m) => GameWire m a (Event [Peer])
peersConnected = mkGen_ $ \_ -> do 
  ps <- peersConnectedM
  case ps of 
    [] -> return $! Right NoEvent
    _ -> return $! ps `deepseq` Right (Event ps)

-- | Returns list of current peers (clients on server, servers on client)
peers :: (LoggingMonad m, NetworkMonad m) => GameWire m a [Peer]
peers = liftGameMonad networkPeersM