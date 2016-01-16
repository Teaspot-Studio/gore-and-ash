{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Actor.State(
    ActorState(..)
  , emptyActorState
  , moveSendedMessages
  ) where

import Control.DeepSeq 
import Data.Dynamic
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

import Game.GoreAndAsh.Actor.TypeRep

-- | Inner state of actor module
data ActorState s = ActorState {
  -- | Stores messages for actor with specified id
  --
  -- Message has type of Dynamic as message manager doesn't know anything about message types.
  -- We don't need to serialization protocol due passing via memory. Type safety is forced 
  -- with Messagable type class with type family (see Actor.Message module). Id space is separate for each actor type
  --
  -- There are two sequences of messages, one for recieved messages from previous frame, and
  -- the second one for messages recieved at current frame. At the end of each frame first
  -- sequence is purged and filled with contents of first and the second one is replaced with
  -- empty sequence.
  actorBoxes :: !(H.HashMap (HashableTypeRep, Int) (S.Seq Dynamic, S.Seq Dynamic))
  -- | Next empty id of actor, id space is separate for each actor type
, actorNextId :: !(H.HashMap HashableTypeRep Int)
  -- | Search table for actor names
, actorNameMap :: !(H.HashMap String HashableTypeRep)
  -- | Next state in state chain of modules
, actorNextState :: !s
} deriving (Generic)

instance NFData Dynamic where 
  rnf = (`seq` ())

instance NFData s => NFData (ActorState s)

-- | Create empty actor state
emptyActorState :: s -> ActorState s 
emptyActorState s = ActorState {
    actorBoxes = H.empty
  , actorNextId = H.empty
  , actorNameMap = H.empty
  , actorNextState = s
  }

-- | Perform rotation between sended messages and recieved ones
moveSendedMessages :: ActorState s -> ActorState s 
moveSendedMessages s = s {
    actorBoxes = fmap (\(_, b) -> (b, S.empty)) . actorBoxes $! s
  }