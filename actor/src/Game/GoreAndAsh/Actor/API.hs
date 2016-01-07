module Game.GoreAndAsh.Actor.API(
    ActorMonad(..)
  , ActorException(..)
  -- | Message API
  , actorSend
  , actorSendMany
  , actorSendDyn
  , actorSendManyDyn
  , actorProcessMessages
  , actorProcessMessagesM
  -- | Actor API
  , makeActor
  , makeFixedActor
  , runActor
  , runActor'
  , stateActor
  , stateActorM
  , stateActorFixed
  , stateActorFixedM
  -- | Helpers for libraries
  , getActorFingerprint
  ) where

import Control.Monad.Catch
import Control.Monad.Fix 
import Control.Monad.State.Strict
import Control.Wire
import Data.Dynamic
import Data.Maybe (catMaybes)
import Data.Word 
import GHC.Generics 
import Prelude hiding (id, (.))
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor.Indexed
import Game.GoreAndAsh.Actor.Message
import Game.GoreAndAsh.Actor.Module
import Game.GoreAndAsh.Actor.State

-- | Exceptions thrown by ActorMonad
data ActorException = 
  ActorIdConflict Word64 Int -- ^ Tried to register already presented actor
  deriving (Show, Generic)

instance Exception ActorException

-- | Low level API for module
class MonadThrow m => ActorMonad m where 
  -- | Registers new actor in message system
  actorRegisterM :: ActorMessage i => m i 
  
  -- | Registers specific id, throws ActorException if there is id clash
  actorRegisterFixedM :: ActorMessage i => i -> m ()

  -- | Deletes actor with given id
  actorDeleteM :: ActorMessage i => i -> m ()

  -- | Checks if given id is already taken
  actorRegisteredM :: ActorMessage i => i -> m Bool

  -- | Sends typed message to actor with given id
  actorSendM :: (ActorMessage i, Typeable (ActorMessageType i)) 
    => i -> ActorMessageType i -> m ()

  -- | Get all messages that were collected for given actor's id and
  -- clear the queue
  actorGetMessagesM :: (ActorMessage i, Typeable (ActorMessageType i))
    => i -> m [ActorMessageType i]

instance {-# OVERLAPPING #-} MonadThrow m => ActorMonad (ActorT s m) where
  actorRegisterM = do 
    astate <- ActorT get 
    let (fpt, i, astate') = pushActorNextId astate 
    ActorT . put $! astate' {
        actorBoxes = H.insert (fpt, toCounter i) S.empty $! actorBoxes astate'
      }
    return i 

  actorRegisterFixedM i = do 
    astate <- ActorT get
    case regActorFixedId i astate of 
      Nothing -> throwM $! ActorIdConflict (getActorFingerprint i) (toCounter i)
      Just astate' -> ActorT . put $! astate'

  actorDeleteM i = do 
    astate <- ActorT get 
    ActorT . put $! deleteActorId i astate
  
  actorRegisteredM i = do 
    astate <- ActorT get 
    return . isActorIdRegistered i $! astate

  actorSendM i msg = do 
    astate <- ActorT get 
    ActorT . put $! putActorMessage i (toDyn msg) astate

  actorGetMessagesM i = do 
    astate <- ActorT get 
    let (msgs, astate') = getActorMessages i astate 
    ActorT . put $! astate'
    return . catMaybes $! fromDynamic <$> msgs

instance {-# OVERLAPPABLE #-} (MonadThrow (mt m), ActorMonad m, MonadTrans mt) => ActorMonad (mt m) where 
  actorRegisterM = lift actorRegisterM
  actorRegisterFixedM = lift . actorRegisterFixedM
  actorDeleteM = lift . actorDeleteM
  actorRegisteredM = lift . actorRegisteredM
  actorSendM a b = lift $ actorSendM a b
  actorGetMessagesM = lift . actorGetMessagesM

getActorFingerprint :: forall i . ActorMessage i => i -> Word64
getActorFingerprint _ = actorFingerprint (Proxy :: Proxy i)

-- | Returns next unregistered id of actor and updates internal state
pushActorNextId :: forall i s . ActorMessage i => ActorState s -> (Word64, i, ActorState s)
pushActorNextId !s = case H.lookup k (actorBoxes s) of
  Just _ -> pushActorNextId nextState
  Nothing -> (fingerprint, nextId, nextState)
  where 
    fingerprint = actorFingerprint (Proxy :: Proxy i)
    (nextId, nextState) = rawGet s 
    k = (fingerprint, toCounter nextId)

    -- | Update @actorNextId@ map
    rawGet :: ActorState s -> (i, ActorState s)
    rawGet s' = case H.lookup fingerprint (actorNextId s') of 
      Nothing -> (fromCounter 0, s' {
        actorNextId = H.insert fingerprint 1 (actorNextId s')
        })
      Just i -> (fromCounter i, s' {
        actorNextId = H.insert fingerprint (i+1) (actorNextId s')
        })
 
-- | Try to register given id in the mailbox map
regActorFixedId :: forall i s . ActorMessage i => i -> ActorState s -> Maybe (ActorState s)
regActorFixedId !i !s = case H.lookup k (actorBoxes s) of 
  Just _ -> Nothing
  Nothing -> Just $! s {
      actorBoxes = H.insert k S.empty $! actorBoxes s
    }
  where
    fingerprint = actorFingerprint (Proxy :: Proxy i)
    k = (fingerprint, toCounter i)

-- | Remove actor id from mailbox map
deleteActorId :: forall i s . ActorMessage i => i -> ActorState s -> ActorState s 
deleteActorId !i !s = s {
    actorBoxes = H.delete k . actorBoxes $! s 
  }
  where
    fingerprint = actorFingerprint (Proxy :: Proxy i)
    k = (fingerprint, toCounter i)

-- | Returns True if given ID is registered
isActorIdRegistered :: forall i s . ActorMessage i => i -> ActorState s -> Bool
isActorIdRegistered !i !s = case H.lookup k . actorBoxes $! s of 
  Nothing -> False 
  Just _ -> True 
  where
    fingerprint = actorFingerprint (Proxy :: Proxy i)
    k = (fingerprint, toCounter i)

-- | Appends given message in corresponding message queue
putActorMessage :: forall i s . ActorMessage i => i -> Dynamic -> ActorState s -> ActorState s
putActorMessage !i !msg !s = case H.lookup k . actorBoxes $! s of 
  Nothing -> s
  Just msgs -> s {
      actorBoxes = H.insert k (msgs S.|> msg) . actorBoxes $! s
    }
  where
    fingerprint = actorFingerprint (Proxy :: Proxy i)
    k = (fingerprint, toCounter i)

-- | Return all messages in corresponding mailbox and cleanup it
getActorMessages :: forall i s . ActorMessage i => i -> ActorState s -> ([Dynamic], ActorState s)
getActorMessages !i !s = case H.lookup k . actorBoxes $! s of 
  Nothing -> ([], s)
  Just msgs -> (F.toList msgs, s {
      actorBoxes = H.insert k S.empty . actorBoxes $! s
    })
  where
    fingerprint = actorFingerprint (Proxy :: Proxy i)
    k = (fingerprint, toCounter i)

-- | Sends message to statically known actor
actorSend :: (ActorMonad m, ActorMessage i, Typeable (ActorMessageType i)) 
  => i -> GameWire m (Event (ActorMessageType i)) (Event ())
actorSend i = liftGameMonadEvent1 $ actorSendM i

-- | Sends many messages to statically known actor
actorSendMany :: (ActorMonad m, ActorMessage i, Typeable (ActorMessageType i), F.Foldable t)
  => i -> GameWire m (Event (t (ActorMessageType i))) (Event ())
actorSendMany i = liftGameMonadEvent1 $ F.mapM_ (actorSendM i)

-- | Sends message to actor with incoming id
actorSendDyn :: (ActorMonad m, ActorMessage i, Typeable (ActorMessageType i)) 
  => GameWire m (Event (i, ActorMessageType i)) (Event ())
actorSendDyn = liftGameMonadEvent1 $ \(i, m) -> actorSendM i m

-- | Sends many messages, dynamic version of actorSendMany which takes actor id as arrow input
actorSendManyDyn :: (ActorMonad m, ActorMessage i, Typeable (ActorMessageType i), F.Foldable t)
  => GameWire m (Event (t (i, ActorMessageType i))) (Event ())
actorSendManyDyn = liftGameMonadEvent1 $ F.mapM_ (uncurry actorSendM)

-- | Helper to process all messages from message queue and update a state
actorProcessMessages :: (ActorMonad m, ActorMessage i, Typeable (ActorMessageType i))
  => i -- ^ Actor id known statically
  -> (a -> ActorMessageType i -> a) -- ^ Action that modifies accumulator
  -> GameWire m a a -- ^ Wire that updates input value using supplied function
actorProcessMessages i f = liftGameMonad1 $ \a -> do 
  msgs <- actorGetMessagesM i
  return . F.foldl' f a $! msgs

-- | Helper to process all messages from message queue and update a state (monadic version)
actorProcessMessagesM :: (ActorMonad m, ActorMessage i, Typeable (ActorMessageType i))
  => i -- ^ Actor id known statically
  -> (a -> ActorMessageType i -> GameMonadT m a) -- ^ Monadic action that modifies accumulator
  -> GameWire m a a -- ^ Wire that updates input value using supplied function
actorProcessMessagesM i f = liftGameMonad1 $ \a -> do 
  msgs <- actorGetMessagesM i 
  foldM f a msgs

-- | Registers new index for wire and makes an actor wire
makeActor :: (ActorMonad m, ActorMessage i) 
  => (i -> GameWire m a b) -- ^ Body wire
  -> GameActor m i a b -- ^ Operation that makes actual actor
makeActor wbody = do 
  i <- actorRegisterM
  return $! GameWireIndexed i (wbody i)

-- | Registers new actor with fixed id, can fail with ActorException if there is already 
-- registered actor for that id
makeFixedActor :: (ActorMonad m, ActorMessage i) 
  => i -- ^ Manual id of actor
  -> GameWire m a b -- ^ Body wire
  -> GameActor m i a b -- ^ Operation that makes actual actor
makeFixedActor i wbody = do 
  actorRegisterFixedM i
  return $! GameWireIndexed i wbody

-- | If need no dynamic switching, you can use the function to embed index wire just at time
runActor :: ActorMonad m 
  => GameActor m i a b -- ^ Actor creator
  -> GameWire m a (b, i) -- ^ Usual wire that also returns id of inner indexed wire
runActor actor = switch makeWire
  where
  -- | Switches immidieatly to created wire, thats why error is used for
  -- value that should be returned in case where there is no event.
  makeWire = proc _ -> do 
    e <- mapE (\iw -> arr (, indexedId iw) . indexedWire iw) . now . liftGameMonadOnce actor -< ()
    returnA -< (error "runActor: impossible", e)

-- | Same as runActor, but doesn't return id of actor
runActor' :: ActorMonad m 
  => GameActor m i a b -- ^ Actor creator
  -> GameWire m a b -- ^ Usual wire
runActor' actor = arr fst . runActor actor

-- | Helper to create stateful actors, same as @stateWire@
stateActor :: (ActorMonad m, MonadFix m, ActorMessage i, Typeable (ActorMessageType i))
  => (i -> b) -- ^ Inital value of state
  -> (i -> b -> ActorMessageType i -> b) -- ^ Handler for messages
  -> (i -> GameWire m (a, b) b) -- ^ Handler that transforms current state
  -> GameActor m i a b -- ^ Resulting actor incapsulating @b@ in itself
stateActor bi f w = makeActor $ \i -> stateWire (bi i) $ proc (a, b) -> do 
  b' <- actorProcessMessages i (f i) -< b
  w i -< (a, b')

-- | Helper to create stateful actors, same as @stateWire@, monadic version of @stateActor@
stateActorM :: (ActorMonad m, MonadFix m, ActorMessage i, Typeable (ActorMessageType i))
  => (i -> b) -- ^ Inital value of state
  -> (i -> b -> ActorMessageType i -> GameMonadT m b) -- ^ Handler for messages
  -> (i -> GameWire m (a, b) b) -- ^ Handler that transforms current state
  -> GameActor m i a b -- ^ Resulting actor incapsulating @b@ in itself
stateActorM bi f w = makeActor $ \i -> stateWire (bi i) $ proc (a, b) -> do 
  b' <- actorProcessMessagesM i (f i) -< b
  w i -< (a, b')

-- | Helper to create stateful actors, same as @stateWire@
stateActorFixed :: (ActorMonad m, MonadFix m, ActorMessage i, Typeable (ActorMessageType i))
  => i -- ^ Fixed id
  -> b -- ^ Inital value of state
  -> (b -> ActorMessageType i -> b) -- ^ Handler for messages
  -> GameWire m (a, b) b -- ^ Handler that transforms current state
  -> GameActor m i a b -- ^ Resulting actor incapsulating @b@ in itself
stateActorFixed i bi f w = makeFixedActor i $ stateWire bi $ proc (a, b) -> do 
  b' <- actorProcessMessages i f -< b
  w -< (a, b')

-- | Helper to create stateful actors, same as @stateWire@, monadic version
stateActorFixedM :: (ActorMonad m, MonadFix m, ActorMessage i, Typeable (ActorMessageType i))
  => i -- ^ Fixed id
  -> b -- ^ Inital value of state
  -> (b -> ActorMessageType i -> GameMonadT m b) -- ^ Handler for messages, monadic
  -> GameWire m (a, b) b -- ^ Handler that transforms current state
  -> GameActor m i a b -- ^ Resulting actor incapsulating @b@ in itself
stateActorFixedM i bi f w = makeFixedActor i $ stateWire bi $ proc (a, b) -> do 
  b' <- actorProcessMessagesM i f -< b
  w -< (a, b')