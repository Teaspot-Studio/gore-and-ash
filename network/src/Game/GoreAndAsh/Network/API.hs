module Game.GoreAndAsh.Network.API(
    NetworkMonad(..)
  , peersConnected
  , currentPeers
  , peerMessages
  , peerSend
  ) where

import Control.DeepSeq 
import Control.Monad.State.Strict
import Control.Wire.Core 
import Control.Wire.Unsafe.Event 
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text
import Foreign
import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network.Module
import Game.GoreAndAsh.Network.State
import Network.ENet.Host
import Network.ENet.Packet as P
import Network.ENet.Peer
import Network.Socket (SockAddr)
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 
import Control.Exception (bracket)

class MonadIO m => NetworkMonad m where
  -- | Start listening for messages, should be called once
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

  -- | Returns received packets for given peer and channel
  peerMessagesM :: Peer -> ChannelID -> m (S.Seq Packet)

  -- | Sends a packet to given peer on given channel
  peerSendM :: Peer -> ChannelID -> Packet -> m ()

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

  peerMessagesM peer ch = do
    msgs <- networkMessages <$> NetworkT get
    return . fromMaybe S.empty $! H.lookup (peer, ch) msgs

  peerSendM peer ch p = liftIO $ bracket (P.poke p) P.destroy $ send peer ch

  networkPeersM = do 
    NetworkState{..} <- NetworkT get 
    return $ H.keys networkPeers  

instance {-# OVERLAPPABLE #-} (Monad (mt m), MonadIO (mt m), LoggingMonad m, NetworkMonad m, MonadTrans mt) => NetworkMonad (mt m) where 
  networkBind a mc mch ib ob = lift $ networkBind a mc mch ib ob
  peersConnectedM = lift peersConnectedM
  networkConnect a b c = lift $ networkConnect a b c 
  peerMessagesM a b = lift $ peerMessagesM a b 
  peerSendM a b c = lift $ peerSendM a b c
  networkPeersM = lift networkPeersM

-- | Fires when one or several clients were connected
peersConnected :: (LoggingMonad m, NetworkMonad m) => GameWire m a (Event [Peer])
peersConnected = mkGen_ $ \_ -> do 
  ps <- peersConnectedM
  return $! case ps of 
    [] -> Right NoEvent
    _ -> ps `deepseq` Right (Event ps)

-- | Returns list of current peers (clients on server, servers on client)
currentPeers :: (LoggingMonad m, NetworkMonad m) => GameWire m a [Peer]
currentPeers = liftGameMonad networkPeersM

-- | Returns sequence of packets that were recieved during last frame from given peer and channel id
peerMessages :: (LoggingMonad m, NetworkMonad m) => Peer -> ChannelID -> GameWire m a (Event (S.Seq Packet)) 
peerMessages p ch = mkGen_ $ \_ -> do 
  msgs <- peerMessagesM p ch
  return $! if S.null msgs 
    then Right NoEvent
    else msgs `deepseq` Right (Event msgs)

-- | Send packet to given peer with given channel id
peerSend :: (LoggingMonad m, NetworkMonad m) => Peer -> ChannelID -> GameWire m (Event Packet) (Event ())
peerSend peer chid = liftGameMonadEvent1 $ peerSendM peer chid 