{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Actor.State(
    ActorState(..)
  , pushActorNextId
  ) where

import Control.DeepSeq 
import Data.Dynamic 
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

-- | Inner state of actor module
data ActorState s = ActorState {
  -- | Stores messages for actor with specified id
  actorBoxes :: !(H.HashMap Int (S.Seq Dynamic))
  -- | Next empty id of actor
, actorNextId :: !Int
  -- | Next state in state chain of modules
, actorNextState :: !s
} deriving (Generic)

instance NFData Dynamic where 
  rnf = (`seq` ())

instance NFData s => NFData (ActorState s)

-- | Returns next unregistered id of actor and updates internal state
pushActorNextId :: ActorState s -> (Int, ActorState s)
pushActorNextId !s@ActorState{..} = case H.lookup actorNextId actorBoxes of
  Just _ -> pushActorNextId $! s { actorNextId = actorNextId + 1 }
  Nothing -> let
    i = actorNextId
    s' = s {
        actorNextId = actorNextId + 1
      }
    in (i, s')