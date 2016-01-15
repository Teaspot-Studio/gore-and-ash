module Game.GoreAndAsh.Sync.API(
    SyncMonad(..)
  ) where

import Control.Monad.State.Strict
import Control.Wire
import Data.Proxy 
import Data.Serialize (encode, Serialize)
import Data.Text
import Data.Word 
import Prelude hiding (id, (.))
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Actor.TypeRep
import Game.GoreAndAsh.Logging 
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync.Module
import Game.GoreAndAsh.Sync.State

-- | Low level API for module
-- Need at least one network channel to operate. If you open more than one channel,
-- the module would use chanel id 1 as service channel, therefore count of channels
-- on client and server should match (server won't response on channel 1 if it doesn't
-- have it).
class MonadIO m => SyncMonad m where 
  -- | Find actor id by it stable type representation
  getSyncIdM :: HashableTypeRep -> m (Maybe Word64)
  -- | Find actor type representation by it id
  getSyncTypeRepM :: Word64 -> m (Maybe HashableTypeRep)
  -- | Generate and register new id for given actor type representation
  registerSyncIdM :: LoggingMonad m => HashableTypeRep -> m Word64
  -- | Register new type rep with given id, doesn't overide existing records
  addSyncTypeRepM :: LoggingMonad m => HashableTypeRep -> Word64 -> m ()

  -- | Send message as soon as network id of actor is resolved
  syncScheduleMessageM :: (NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
    => Peer -- ^ Which peer we sending to
    -> ChannelID -- ^ Which channel we are sending within
    -> i -- ^ ID of actor
    -> MessageType -- ^ Strategy of the message (reliable, unordered etc.)
    -> NetworkMessageType i -- ^ Message to send
    -> m ()

  -- | Switch on/off detailed logging of the module
  syncSetLoggingM :: Bool -> m ()

  -- | Setups behavior model in synchronizing of actor ids
  -- Note: clients should be slaves and server master
  syncSetRoleM :: SyncRole -> m ()

  -- | Returns current behavior model in synchronizing of actor ids
  -- Note: clients should be slaves and server master
  syncGetRoleM :: m SyncRole

  -- | Send request for given peer for id of given actor
  syncRequestIdM :: forall proxy i . (ActorMonad m, NetworkMonad m, LoggingMonad m, NetworkMessage i) 
    => Peer -> proxy i -> m ()

instance {-# OVERLAPPING #-} MonadIO m => SyncMonad (SyncT s m) where
  getSyncIdM !tr = do 
    sstate <- SyncT get 
    return . H.lookup tr . syncIdMap $! sstate

  getSyncTypeRepM !w = do 
    sstate <- SyncT get 
    return . H.lookup w . syncIdMapRev $! sstate

  registerSyncIdM !tr = do 
    sstate <- SyncT get
    let (w64, s') = registerSyncIdInternal tr sstate
    syncLog s' $ "Registering new actor type " <> pack (show tr) <> " with id " <> pack (show w64)
    SyncT . put $! s'
    return w64

  addSyncTypeRepM !tr !i = do 
    sstate <- SyncT get 
    syncLog sstate $ "Registering new actor type " <> pack (show tr) <> " with id " <> pack (show i)
    SyncT . put $! addSyncTypeRepInternal tr i sstate

  syncScheduleMessageM peer ch i mt msg = do 
    sstate <- SyncT get 
    let name = getActorName i 
        serviceMsg = Message ReliableMessage $! encode (0 :: Word64, encode $! SyncServiceRequestId name )
        actorId = fromIntegral (toCounter i) :: Word64
        v = (name, ch, \netid -> Message mt $! encode (netid, encode (actorId, encode msg)))
    serviceChan <- getServiceChannel 
    peerSendM peer serviceChan serviceMsg
    SyncT . put $! sstate {
        syncScheduledMessages = case H.lookup peer . syncScheduledMessages $! sstate of 
          Nothing -> H.insert peer (S.singleton v) . syncScheduledMessages $! sstate
          Just msgs -> H.insert peer (msgs S.|> v) . syncScheduledMessages $! sstate
      }

  syncSetLoggingM f = do 
    sstate <- SyncT get 
    SyncT . put $! sstate {
        syncLogging = f
      }

  syncSetRoleM r = do 
    sstate <- SyncT get
    SyncT . put $! sstate {
        syncRole = r 
      }

  syncGetRoleM = syncRole <$> SyncT get 

  syncRequestIdM peer p = do
    s <- SyncT get
    syncLog s $ "request id of actor " <> pack (show $ actorFingerprint p)
    s' <- syncRequestIdInternal peer p s
    SyncT . put $! s'

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), SyncMonad m, ActorMonad m, NetworkMonad m, LoggingMonad m,  MonadTrans mt) => SyncMonad (mt m) where 
  getSyncIdM = lift . getSyncIdM
  getSyncTypeRepM = lift . getSyncTypeRepM
  registerSyncIdM = lift . registerSyncIdM
  addSyncTypeRepM a b = lift $ addSyncTypeRepM a b
  syncScheduleMessageM peer ch i mt msg  = lift $ syncScheduleMessageM peer ch i mt msg
  syncSetLoggingM = lift . syncSetLoggingM
  syncSetRoleM = lift . syncSetRoleM
  syncGetRoleM = lift syncGetRoleM
  syncRequestIdM a b = lift $ syncRequestIdM a b 

-- | Return typename of actor using it type representation
getActorName :: forall i . ActorMessage i => i -> String 
getActorName _ = show $ actorFingerprint (Proxy :: Proxy i)