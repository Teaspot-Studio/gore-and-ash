module Game.GoreAndAsh.Network.API(
    NetworkMonad(..)
  -- | Peer handling
  , peersConnected
  , peersDisconnected
  , peerDisconnected
  , currentPeers
  , onPeers
  -- | Messaging support
  , peerMessages
  , peerSend
  , peerSendMany
  ) where

import Control.DeepSeq hiding (force)
import Control.Exception.Base (IOException)
import Control.Monad.Catch
import Control.Monad.State.Strict
import Control.Wire hiding (when)
import Control.Wire.Unsafe.Event
import Data.Maybe (fromMaybe)
import Data.Text
import Foreign
import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network.Message
import Game.GoreAndAsh.Network.Module
import Game.GoreAndAsh.Network.State
import Network.ENet.Host
import Network.ENet.Packet as P
import Network.ENet.Peer
import Network.Socket (SockAddr)
import Prelude hiding ((.), id)
import qualified Data.ByteString as BS 
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S

class (MonadIO m, MonadCatch m) => NetworkMonad m where
  -- | Start listening for messages, should be called once
  networkBind :: LoggingMonad m => Maybe SockAddr -- ^ Address to listen, Nothing is client
    -> Word -- ^ Maximum count of connections
    -> Word -- ^ Number of channels to open
    -> Word32 -- ^ Incoming max bandwidth
    -> Word32 -- ^ Outcoming max bandwidth
    -> m ()

  -- | Returns peers that were connected during last frame
  peersConnectedM :: m (S.Seq Peer)

  -- | Returns peers that were disconnected during last frame
  peersDisconnectedM :: m (S.Seq Peer)

  -- | Initiate connection to the remote host
  networkConnect :: LoggingMonad m => SockAddr -- ^ Address of host
    -> Word -- ^ Count of channels to open
    -> Word32 -- ^ Additional data (0 default)
    -> m (Maybe ())

  -- | Returns received packets for given peer and channel
  peerMessagesM :: Peer -> ChannelID -> m (S.Seq BS.ByteString)

  -- | Sends a packet to given peer on given channel
  peerSendM :: LoggingMonad m => Peer -> ChannelID -> Message -> m ()

  -- | Returns list of currently connected peers (servers on client side, clients on server side)
  networkPeersM :: m (S.Seq Peer)

  -- | Sets flag for detailed logging (for debug)
  networkSetDetailedLoggingM :: Bool -> m ()

  -- | Return count of allocated network channels
  networkChannels :: m Word 

instance {-# OVERLAPPING #-} (MonadIO m, MonadCatch m) => NetworkMonad (NetworkT s m) where
  networkBind addr conCount chanCount inBandth outBandth = do
    nstate <- NetworkT get 
    phost <- liftIO $ create addr (fromIntegral conCount) (fromIntegral chanCount) inBandth outBandth
    if phost == nullPtr
      then case addr of 
        Nothing -> putMsgLnM "Network: failed to initalize client side"
        Just a -> putMsgLnM $ "Network: failed to connect to " <> pack (show a)
      else do
        when (networkDetailedLogging nstate) $ putMsgLnM $ case addr of 
          Nothing -> "Network: client network system initalized"
          Just a -> "Network: binded to " <> pack (show a)
        NetworkT $ put $ nstate {
            networkHost = Just phost
          , networkMaximumChannels = chanCount
          }

  peersConnectedM = do 
    NetworkState{..} <- NetworkT get 
    return networkConnectedPeers

  peersDisconnectedM = do 
    NetworkState{..} <- NetworkT get 
    return networkDisconnectedPeers

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
          else do
            NetworkT . put $! nstate {
                networkMaximumChannels = chanCount
              }
            return $ Just ()

  peerMessagesM peer ch = do
    msgs <- networkMessages <$> NetworkT get
    return . fromMaybe S.empty $! H.lookup (peer, ch) msgs

  peerSendM peer ch msg = do
    nstate <- NetworkT get 
    when (networkDetailedLogging nstate) $ putMsgLnM $ "Network: sending packet via channel "
      <> pack (show ch) <> ", payload: " <> pack (show msg)
    -- IOError
    let sendAction = liftIO $ send peer ch =<< P.poke (messageToPacket msg)
    catch sendAction $ \(e :: IOException) -> do 
      putMsgLnM $ "Network: failed to send packet '" <> pack (show e) <> "'"
    

  networkPeersM = do 
    NetworkState{..} <- NetworkT get 
    return $! networkPeers S.><  networkConnectedPeers

  networkSetDetailedLoggingM f = do 
    s <- NetworkT get 
    put $ s { networkDetailedLogging = f }

  networkChannels = do 
    s <- NetworkT get 
    return $ networkMaximumChannels s 

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), MonadCatch (mt m), LoggingMonad m, NetworkMonad m, MonadTrans mt) => NetworkMonad (mt m) where 
  networkBind a mc mch ib ob = lift $ networkBind a mc mch ib ob
  peersConnectedM = lift peersConnectedM
  peersDisconnectedM = lift peersDisconnectedM
  networkConnect a b c = lift $ networkConnect a b c 
  peerMessagesM a b = lift $ peerMessagesM a b 
  peerSendM a b c = lift $ peerSendM a b c
  networkPeersM = lift networkPeersM
  networkSetDetailedLoggingM = lift . networkSetDetailedLoggingM
  networkChannels = lift networkChannels 
  
-- | Fires when one or several clients were connected
peersConnected :: (LoggingMonad m, NetworkMonad m) => GameWire m a (Event (S.Seq Peer))
peersConnected = mkGen_ $ \_ -> do 
  ps <- peersConnectedM
  return $! if S.null ps  
    then Right NoEvent
    else ps `deepseq` Right (Event ps)

-- | Fires when one of connected peers is disconnected for some reason
peersDisconnected :: (LoggingMonad m, NetworkMonad m) => GameWire m a (Event (S.Seq Peer))
peersDisconnected = mkGen_ $ \_ -> do 
  ps <- peersDisconnectedM 
  return $! if S.null ps  
    then Right NoEvent
    else ps `deepseq` Right (Event ps)

-- | Fires when statically known peer is disconnected
peerDisconnected :: (LoggingMonad m, NetworkMonad m) => Peer -> GameWire m a (Event ())
peerDisconnected p = mkGen_ $ \_ -> do 
  ps <- peersDisconnectedM 
  return $! case F.find (p ==) ps of 
    Nothing -> Right NoEvent
    Just _ -> Right $! Event ()

-- | Returns list of current peers (clients on server, servers on client)
currentPeers :: (LoggingMonad m, NetworkMonad m) => GameWire m a (S.Seq Peer)
currentPeers = liftGameMonad networkPeersM

-- | Returns sequence of packets that were recieved during last frame from given peer and channel id
peerMessages :: (LoggingMonad m, NetworkMonad m) => Peer -> ChannelID -> GameWire m a (Event (S.Seq BS.ByteString)) 
peerMessages p ch = mkGen_ $ \_ -> do 
  msgs <- peerMessagesM p ch
  return $! if S.null msgs 
    then Right NoEvent
    else msgs `deepseq` Right (Event msgs)

-- | Send message to given peer with given channel id
peerSend :: (LoggingMonad m, NetworkMonad m) => Peer -> ChannelID -> GameWire m (Event Message) (Event ())
peerSend peer chid = liftGameMonadEvent1 $ peerSendM peer chid 

-- | Send several messages to given peer with given channel id
peerSendMany :: (LoggingMonad m, NetworkMonad m, F.Foldable t) => Peer -> ChannelID -> GameWire m (Event (t Message)) (Event ())
peerSendMany peer chid = liftGameMonadEvent1 $ mapM_ (peerSendM peer chid) 

-- | Sometimes you want to listen all peers and use statefull computations at the same time.
--
-- The helper maintance internal collection of current peers and switches over it each time
-- it changes.
onPeers :: forall m a b . (MonadFix m, LoggingMonad m, NetworkMonad m)
  => (S.Seq Peer -> GameWire m a b) -- ^ Wire that uses current peer collection
  -> GameWire m a b
onPeers w = switch $ proc _ -> do -- Trick to immediate switch to current set of peers
  epeers <- now . currentPeers -< ()
  returnA -< (error "onPeers: impossible", go <$> epeers)
  where
  go initalPeers = proc a -> do 
    conEvent <- peersConnected -< ()
    disEvent <- peersDisconnected -< ()

    -- | Local state loop to catch up peers
    rec curPeers' <- forceNF . delay initalPeers -< curPeers
        let addEvent = (\ps -> curPeers' S.>< ps) <$> conEvent
        let addedPeers = event curPeers' id addEvent -- To not loose added peers when some removed
        let remEvent = (F.foldl' (\ps p -> S.filter (/= p) ps) addedPeers) <$> disEvent
        let ew = fmap listenPeers $ addEvent `mergeR` remEvent
        (curPeers, b) <- rSwitch (listenPeers initalPeers) -< (a, ew)
    returnA -< b
    where
      listenPeers :: S.Seq Peer -> GameWire m a (S.Seq Peer, b)
      listenPeers peers = proc a -> do 
        b <- w peers -< a 
        returnA -< (peers, b)