{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Sync.Module(
    SyncT(..)
  , registerSyncIdInternal
  , addSyncTypeRepInternal
  , getServiceChannel
  ) where

import Control.Monad.Catch
import Control.Monad.Fix 
import Control.Monad.State.Strict
import Data.Maybe
import Data.Monoid 
import Data.Serialize
import Data.Text (pack)
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
      s' <- processServiceMessages s
      runStateT m s'

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
  foldM (process serviceChan) sstate . H.toList . syncScheduledMessages $ sstate
  where
  -- | Process one peer
  process serviceChan s ((peer, chan), msgs) = do 
    bss <- peerMessagesM peer chan
    let serviceMsgs = catMaybesSeq . fmap decodeService $ bss
    foldM (processService serviceChan peer chan msgs) s serviceMsgs

  -- | Decode service message
  decodeService bs = case decode bs of 
    Left _ -> Nothing 
    Right (w64 :: Word64, mbs :: BS.ByteString) -> if w64 == 0 
      then case decode mbs of 
        Left _ -> Nothing 
        Right !msg -> Just msg 
      else Nothing 

  -- | Service one service message for given peer
  processService serviceChan peer chan msgs s serviceMsg = case serviceMsg of
    SyncServiceRequestId aname -> do 
      marep <- findActorTypeRepM aname
      case marep of 
        Nothing -> do 
          peerSendM peer serviceChan . Message ReliableMessage . encode $ SyncServiceResponseNotRegistered aname
          return s
        Just arep -> case H.lookup arep . syncIdMap $ s of 
          Nothing -> do 
            let (w64, s') = registerSyncIdInternal arep s 
            peerSendM peer serviceChan . Message ReliableMessage . encode $ SyncServiceResponseId aname w64
            return s'
          Just w64 -> do 
            peerSendM peer serviceChan . Message ReliableMessage . encode $ SyncServiceResponseId aname w64
            return s 
    SyncServiceResponseId aname w64 -> do 
      marep <- findActorTypeRepM aname 
      case marep of 
        Nothing -> return s 
        Just arep -> do 
          let s' = addSyncTypeRepInternal arep w64 s 
          sheduled <- fmap catMaybesSeq . forM msgs $ \(aname', msg) -> if aname == aname'
            then do 
              peerSendM peer chan . msg $! w64 
              return Nothing
            else return $! Just (aname', msg)
          return $! s' {
              syncScheduledMessages = H.insert (peer, chan) sheduled . syncScheduledMessages $! s'
            }
    SyncServiceResponseNotRegistered aname -> do 
      putMsgLnM $ "Sync module: Failed to resolve actor id with name " <> (pack aname) 
      return s 

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