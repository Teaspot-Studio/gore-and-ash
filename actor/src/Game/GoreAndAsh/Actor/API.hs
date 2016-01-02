module Game.GoreAndAsh.Actor.API(
    ActorMonad(..)
  , actorSend
  , actorSendDyn
  , actorProcessMessages
  , actorProcessMessagesM
  ) where

import Control.Monad.State.Strict
import Control.Wire
import Data.Dynamic
import Data.Maybe (isJust, catMaybes)
import Prelude hiding (id, (.))
import qualified Data.Foldable as F 
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor.Message
import Game.GoreAndAsh.Actor.State
import Game.GoreAndAsh.Actor.Module

-- | Low level API for module
class Monad m => ActorMonad m where 
  -- | Registers new actor in message system
  actorRegisterM :: ActorMessage i => m i 

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

instance {-# OVERLAPPING #-} Monad m => ActorMonad (ActorT s m) where
  actorRegisterM = do 
    astate <- ActorT get 
    let (i, astate') = pushActorNextId astate 
    ActorT . put $! astate' {
        actorBoxes = H.insert i S.empty $! actorBoxes astate'
      }
    return $! fromCounter i 

  actorDeleteM i = do 
    astate <- ActorT get 
    ActorT . put $! astate { 
      actorBoxes = H.delete (toCounter i) $! actorBoxes astate
    }
  
  actorRegisteredM i = do 
    astate <- ActorT get 
    return . isJust . H.lookup (toCounter i) . actorBoxes $! astate

  actorSendM i msg = do 
    astate <- ActorT get 
    case H.lookup (toCounter i) . actorBoxes $! astate of 
      Nothing -> return ()
      Just msgs -> ActorT . put $! astate {
          actorBoxes = H.insert (toCounter i) (msgs S.|> toDyn  msg) . actorBoxes $! astate
        }

  actorGetMessagesM i = do 
    astate <- ActorT get 
    case H.lookup (toCounter i) . actorBoxes $! astate of 
      Nothing -> return []
      Just msgs -> do 
        ActorT . put $! astate {
          actorBoxes = H.insert (toCounter i) S.empty . actorBoxes $! astate
        }
        return . catMaybes . F.toList $! fromDynamic <$> msgs

instance {-# OVERLAPPABLE #-} (Monad (mt m), ActorMonad m, MonadTrans mt) => ActorMonad (mt m) where 
  actorRegisterM = lift actorRegisterM
  actorDeleteM = lift . actorDeleteM
  actorRegisteredM = lift . actorRegisteredM
  actorSendM a b = lift $ actorSendM a b
  actorGetMessagesM = lift . actorGetMessagesM

-- | Sends message to statically known actor
actorSend :: (ActorMonad m, ActorMessage i, Typeable (ActorMessageType i)) 
  => i -> GameWire m (Event (ActorMessageType i)) (Event ())
actorSend i = liftGameMonadEvent1 $ actorSendM i

-- | Sends message to actor with incoming id
actorSendDyn :: (ActorMonad m, ActorMessage i, Typeable (ActorMessageType i)) 
  => GameWire m (Event (i, ActorMessageType i)) (Event ())
actorSendDyn = liftGameMonadEvent1 $ \(i, m) -> actorSendM i m

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