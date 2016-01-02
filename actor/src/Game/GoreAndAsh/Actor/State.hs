module Game.GoreAndAsh.Actor.State(
    ActorState(..)
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq 

-- | Inner state of actor module
data ActorState s = ActorState {
  actorNextState :: !s
} deriving (Generic)

instance NFData s => NFData (ActorState s)