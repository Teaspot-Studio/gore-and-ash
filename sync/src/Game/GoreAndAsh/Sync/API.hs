module Game.GoreAndAsh.Sync.API(
    SyncMonad(..)
  ) where

import Control.Monad.State.Strict
import Control.Wire
import Data.Proxy 
import Data.Serialize (encode, Serialize)
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
  registerSyncIdM :: HashableTypeRep -> m Word64
  -- | Register new type rep with given id, doesn't overide existing records
  addSyncTypeRepM :: HashableTypeRep -> Word64 -> m ()

  -- | Send message as soon as network id of actor is resolved
  syncScheduleMessageM :: (NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
    => Peer -- ^ Which peer we sending to
    -> ChannelID -- ^ Which channel we are sending within
    -> i -- ^ ID of actor
    -> MessageType -- ^ Strategy of the message (reliable, unordered etc.)
    -> NetworkMessageType i -- ^ Message to send
    -> m ()

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
    SyncT . put $! s'
    return w64

  addSyncTypeRepM !tr !i = do 
    sstate <- SyncT get 
    SyncT . put $! addSyncTypeRepInternal tr i sstate

  syncScheduleMessageM peer ch i mt msg = do 
    sstate <- SyncT get 
    let name = getActorName i 
        serviceMsg = Message ReliableMessage $! encode (0 :: Word64, encode $! SyncServiceRequestId name )
        k = (peer, ch)
        actorId = fromIntegral (toCounter i) :: Word64
        v = (name, \netid -> Message mt $! encode (netid, encode (actorId, msg)))
    serviceChan <- getServiceChannel 
    peerSendM peer serviceChan serviceMsg
    SyncT . put $! sstate {
        syncScheduledMessages = case H.lookup k . syncScheduledMessages $! sstate of 
          Nothing -> H.insert k (S.singleton v) . syncScheduledMessages $! sstate
          Just msgs -> H.insert k (msgs S.|> v) . syncScheduledMessages $! sstate
      }

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), SyncMonad m, NetworkMonad m, LoggingMonad m,  MonadTrans mt) => SyncMonad (mt m) where 
  getSyncIdM = lift . getSyncIdM
  getSyncTypeRepM = lift . getSyncTypeRepM
  registerSyncIdM = lift . registerSyncIdM
  addSyncTypeRepM a b = lift $ addSyncTypeRepM a b
  syncScheduleMessageM peer ch i mt msg  = lift $ syncScheduleMessageM peer ch i mt msg

-- | Return typename of actor using it type representation
getActorName :: forall i . ActorMessage i => i -> String 
getActorName _ = show $ actorFingerprint (Proxy :: Proxy i)