module Game.GoreAndAsh.Actor.Indexed(
    GameWireIndexed(..)
  , GameActor
  , updateIndexedWire
  , postActorAction
  , preActorAction
  ) where

import Control.Wire 
import Prelude hiding (id, (.))

import Game.GoreAndAsh.Core.Arrow
import Game.GoreAndAsh.Core.Monad 

-- | Game wire that has its own id
data GameWireIndexed m i a b = GameWireIndexed {
  indexedId :: i 
, indexedWire :: GameWire m a b
}

-- | Equality by equality of ids
instance Eq i => Eq (GameWireIndexed m i a b) where
  gw1 == gw2 = indexedId gw1 == indexedId gw2

-- | Replaces controlling wire in indexed wire
updateIndexedWire :: (GameWire m a b -> GameWire m a b) -> GameWireIndexed m i a b -> GameWireIndexed m i a b 
updateIndexedWire f wi = wi { indexedWire = f $ indexedWire wi }

-- | Common pattern in game for creating incapsulated objects
--
-- Usually wires that are actors need context to register themselfes in core.
-- Major part of wire functions operates with such wrapped indexed arrows thats
-- why the convinient type synonym is exists.
type GameActor m i a b = GameMonadT m (GameWireIndexed m i a b)

-- | Compose actor and wire, the wire is added at the end of actor controller
postActorAction :: Monad m => GameActor m i a b -> (i -> GameWire m b c) -> GameActor m i a c
postActorAction mindexed wi = do 
  indexed <- mindexed
  return $ indexed {
      indexedWire = wi (indexedId indexed) . indexedWire indexed
    }

-- | Compose actor and wire, the wire is added at the beginning of actor controller
preActorAction :: Monad m => (i -> GameWire m c a) -> GameActor m i a b -> GameActor m i c b
preActorAction wi mindexed = do 
  indexed <- mindexed
  return $ indexed {
      indexedWire = indexedWire indexed . wi (indexedId indexed)
    }
