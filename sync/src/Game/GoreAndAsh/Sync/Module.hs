{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Sync.Module(
    SyncT(..)
  , registerSyncIdInternal
  , addSyncTypeRepInternal
  , syncRequestIdInternal
  , getServiceChannel
  , syncLog
  ) where

import Control.Monad.Catch
import Control.Monad.Fix 
import Control.Monad.State.Strict
import Data.Maybe
import Data.Monoid 
import Data.Serialize
import Data.Text (Text, pack)
import Data.Word
import qualified Data.ByteString as BS 
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor 
import Game.GoreAndAsh.Actor.TypeRep
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync.State

newtype SyncT s m a = SyncT { runSyncT :: StateT (SyncState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (SyncState s), MonadFix, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance (NetworkMonad m, LoggingMonad m, ActorMonad m, GameModule m s) => GameModule (SyncT s m) (SyncState s) where 
  type ModuleState (SyncT s m) = SyncState s
  runModule (SyncT m) s = do
    ((a, s'), nextState) <- runModule runCurrentModule (syncNextState s)
    return (a, s' {
        syncNextState = nextState 
      })  
    where
    runCurrentModule = do 
      (a, s') <- runStateT m s
      s'' <- processServiceMessages s'
      return (a, s'')

  newModuleState = emptySyncState <$> newModuleState
  withModule _ = id
  cleanupModule _ = return ()

-- | Detect service messages arrived to the machine and process them
--
-- Note: service channel had id 1 by default, but if there is no such
-- channel, it fallbacks to 0. Make shure that client and server has corresponding
-- count of channels.
processServiceMessages :: (ActorMonad m, NetworkMonad m, LoggingMonad m) => SyncState s -> m (SyncState s)
processServiceMessages sstate = do 
  serviceChan <- getServiceChannel
  peers <- networkPeersM
  foldM (process serviceChan) sstate peers
  where
  -- | Process one peer
  process serviceChan s peer = do 
    bss <- peerMessagesM peer serviceChan
    let serviceMsgs = catMaybesSeq . fmap decodeService $ bss
    foldM (processService serviceChan peer) s serviceMsgs

  -- | Decode service message
  decodeService bs = case decode bs of 
    Left _ -> Nothing 
    Right (w64 :: Word64, mbs :: BS.ByteString) -> if w64 == 0 
      then case decode mbs of 
        Left _ -> Nothing 
        Right !msg -> Just msg 
      else Nothing 

  -- | Service one service message for given peer
  processService serviceChan peer s serviceMsg = case serviceMsg of
    SyncServiceRequestId aname -> do 
      syncLog s $ "Received request for network id for actor " <> pack aname
      marep <- findActorTypeRepM aname
      case marep of 
        Nothing -> do
          syncLog s "Such actor isn't known"
          sendService peer serviceChan $ SyncServiceResponseNotRegistered aname
          return s
        Just arep -> case H.lookup arep . syncIdMap $ s of 
          Nothing -> do
            syncLog s "Registering actor network id, sending"
            let (w64, s') = registerSyncIdInternal arep s 
            sendService peer serviceChan $ SyncServiceResponseId aname w64
            return s'
          Just w64 -> do 
            syncLog s "Known actor id, sending"
            sendService peer serviceChan $ SyncServiceResponseId aname w64
            return s 
    SyncServiceResponseId aname w64 -> do 
      syncLog s $ "Received response for network id for actor " <> pack aname <> " and id " <> pack (show w64)
      marep <- findActorTypeRepM aname 
      case marep of 
        Nothing -> do
          syncLog s "Not known actor, ignoring"
          return s 
        Just arep -> do 
          syncLog s "Sending all scheduled messages"
          let s' = addSyncTypeRepInternal arep w64 s 
              msgs = fromMaybe S.empty . H.lookup peer . syncScheduledMessages $! s'
          sheduled <- fmap catMaybesSeq . forM msgs $ \(aname', chan, msg) -> if aname == aname'
            then do 
              peerSendM peer chan . msg $! w64 
              return Nothing
            else return $! Just (aname', chan, msg)
          
          let sended = S.length msgs - S.length sheduled
          syncLog s $ "Sended: " <> pack (show sended)
          
          return $! s' {
              syncScheduledMessages = H.insert peer sheduled . syncScheduledMessages $! s'
            }
    SyncServiceResponseNotRegistered aname -> do 
      putMsgLnM $ "Sync module: Failed to resolve actor id with name " <> (pack aname) 
      return s 

-- | Helper for sending service messages
sendService :: (NetworkMonad m, LoggingMonad m) => Peer -> ChannelID -> SyncServiceMsg -> m ()
sendService peer chanid msg = do 
  let msg' = encode (0 :: Word64, encode msg)
  peerSendM peer chanid . Message ReliableMessage $ msg'

-- | catMaybes for sequences
catMaybesSeq :: S.Seq (Maybe a) -> S.Seq a 
catMaybesSeq = fmap fromJust . S.filter isJust

-- | Internal implementation of actor registrarion when monadic context isn't in scope
registerSyncIdInternal :: HashableTypeRep -> SyncState s -> (Word64, SyncState s)
registerSyncIdInternal tr sstate = case H.lookup tr . syncIdMap $! sstate of 
  Just !i -> (i, sstate)
  Nothing -> (i, sstate { 
        syncIdMap = H.insert tr i . syncIdMap $! sstate
      , syncIdMapRev = H.insert i tr . syncIdMapRev $! sstate
      , syncNextId = i+1          
      })
    where
      !i = findNextEmptyId sstate $ syncNextId sstate
  where
    findNextEmptyId ss i = case H.lookup i .syncIdMapRev $! ss of 
      Nothing -> i 
      Just _ -> findNextEmptyId ss (i+1)

-- | Internal implementation of actor registrarion when monadic context isn't in scope
addSyncTypeRepInternal :: HashableTypeRep -> Word64 -> SyncState s -> SyncState s
addSyncTypeRepInternal !tr !i sstate = case H.lookup i . syncIdMapRev $! sstate of 
  Just _ -> sstate
  Nothing -> sstate { 
      syncIdMap = H.insert tr i . syncIdMap $! sstate
    , syncIdMapRev = H.insert i tr . syncIdMapRev $! sstate
    }

-- | Internal implementation of sending service request for actor net id
syncRequestIdInternal :: forall proxy i m s . (ActorMonad m, NetworkMonad m, LoggingMonad m, NetworkMessage i) 
    => Peer -> proxy i -> SyncState s -> m (SyncState s)
syncRequestIdInternal peer p s = do
  chan <- getServiceChannel
  registerActorTypeRepM p
  sendService peer chan $ SyncServiceRequestId $ show $ actorFingerprint p
  return s

-- | Return channel id 1 if network module has more than 1 channel, either fallback to 0
--
-- Note: If you open more than one channel,
-- the module would use chanel id 1 as service channel, therefore count of channels
-- on client and server should match (server won't response on channel 1 if it doesn't
-- have it).
getServiceChannel :: NetworkMonad m => m ChannelID 
getServiceChannel = do 
  maxi <- networkChannels
  return . ChannelID $! if maxi > 1 then 1 else 0

-- | Log only when flag is turned on
syncLog :: LoggingMonad m => SyncState s -> Text -> m ()
syncLog SyncState{..} = when syncLogging . putMsgLnM . ("Sync module: " <>)