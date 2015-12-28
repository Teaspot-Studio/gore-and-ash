module Game.GoreAndAsh.Network.API(
    NetworkMonad(..)
  , peersConnected
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

class MonadIO m => NetworkMonad m where
  networkBind :: LoggingMonad m => Maybe SockAddr -- ^ Address to listen, Nothing is client
    -> Word -- ^ Maximum count of connections
    -> Word -- ^ Number of channels to open
    -> Word32 -- ^ Incoming max bandwidth
    -> Word32 -- ^ Outcoming max bandwidth
    -> m ()

  -- | Returns peers that were connected during last frame
  peersConnectedM :: m [Peer]


instance MonadIO m => NetworkMonad (NetworkT s m) where
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

instance (LoggingMonad m, NetworkMonad m) => NetworkMonad (GameMonadT m) where
  networkBind a mc mch ib ob = lift $ networkBind a mc mch ib ob
  peersConnectedM = lift peersConnectedM

-- | Fires when one or several clients were connected
peersConnected :: (LoggingMonad m, NetworkMonad m) => GameWire m a (Event [Peer])
peersConnected = mkGen_ $ \_ -> do 
  ps <- peersConnectedM
  case ps of 
    [] -> return $! Right NoEvent
    _ -> return $! ps `deepseq` Right (Event ps)