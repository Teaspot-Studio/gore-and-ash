{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Actor.State(
    ActorState(..)
  ) where

import Control.DeepSeq 
import Data.Dynamic 
import Data.Hashable
import GHC.Fingerprint.Type
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

-- | Inner state of actor module
data ActorState s = ActorState {
  -- | Stores messages for actor with specified id
  -- Message has type of Dynamic as message manager doesn't know anything about message types.
  -- We don't need to serialization protocol due passing via memory. Type safety is forced 
  -- with Messagable type class with type family (see Actor.Message module). Id space is separate for each actor type
  actorBoxes :: !(H.HashMap (Fingerprint, Int) (S.Seq Dynamic))
  -- | Next empty id of actor, id space is separate for each actor type
, actorNextId :: !(H.HashMap Fingerprint Int)
  -- | Next state in state chain of modules
, actorNextState :: !s
} deriving (Generic)

instance NFData Dynamic where 
  rnf = (`seq` ())

instance NFData s => NFData (ActorState s)

deriving instance Generic Fingerprint
instance Hashable Fingerprint