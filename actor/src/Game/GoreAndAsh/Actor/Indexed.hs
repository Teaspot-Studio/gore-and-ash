module Game.GoreAndAsh.Actor.Indexed(
    GameWireIndexed(..)
  , GameActor
  , updateIndexedWire
  ) where

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