module Game.GoreAndAsh.Actor.Collection(
    dynCollection
  , dDynCollection
  , DynCollection(..)
  , module ReExport
  ) where

import Control.Monad 
import Control.Wire
import Control.Wire.Unsafe.Event 
import Data.Filterable 
import Prelude hiding ((.), id)
import qualified Data.Foldable as F 

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor.API
import Game.GoreAndAsh.Actor.Indexed

import Game.GoreAndAsh.Actor.Collection.Data as ReExport

-- | Helper that performs monadic action over value of event or returns default value
-- Note: the function is isomorphic to @Data.Maybe.maybe@
onEvent :: Monad m => b -> Event a -> (a -> GameMonadT m b) -> GameMonadT m b
onEvent def e f = case e of 
  NoEvent -> return def 
  Event a -> f a 

-- | Makes dynamic collection of wires
-- First input of wire is input for each inner wire.
-- Second input is event for adding several wires to collection.
-- Third input is event for removing several wires from collection.
-- Wire returns list of outputs of inner wires.
--
-- Note: if ihibits one of the wires, it is removed from output result during its inhibition
dynCollection :: forall m i a b c c2 . (ActorMonad m, Eq i, DynCollection c, FilterConstraint c (GameWireIndexed m i a b), FilterConstraint c (Either () b), F.Foldable c2) => 
    (c (GameActor m i a b)) -- ^ Inital set of wires
    -> GameWire m (a, Event (c (GameActor m i a b)), Event (c2 i)) (c b)
dynCollection initialActors = mkGen $ \ds input -> do 
  arrs <- sequence initialActors
  go arrs ds input
  where 
  go :: c (GameWireIndexed m i a b)
    -> GameTime
    -> (a, Event (c (GameActor m i a b)), Event (c2 i))
    -> GameMonadT m (Either () (c b), GameWire m (a, Event (c (GameActor m i a b)), Event (c2 i)) (c b))
  go currentWires ds (a, addEvent, removeEvent) = do

    -- | Adding new wires
    newAddedWires <- onEvent currentWires addEvent $ \newActors -> do 
      addWires <- sequence newActors 
      return $ currentWires `concatDynColl` addWires

    -- | Removing wires
    newRemovedWires <- onEvent newAddedWires removeEvent $ \ids ->  
      return $ F.foldl' (\acc i -> fFilter ((/= i) . indexedId) acc) newAddedWires ids

    -- | Calculating outputs
    (bs, newWiresCntrls) <- liftM unzipDynColl $ mapM (\w -> stepWire w ds (Right a)) $ indexedWire <$> newRemovedWires
    let newWires = uncurry updateIndexedWire <$> (fmap const newWiresCntrls `zipDynColl` newRemovedWires)

    return $ length newWires `seq` (Right (rightsDynColl bs), mkGen $ go newWires)

-- | Makes dynamic collection of wires
-- First input of wire is input for each inner wire.
-- Second input is event for adding several wires to collection.
-- Third input is event for removing several wires from collection.
-- Wire returns list of outputs of inner wires.
--
-- Note: it is delayed version of dynCollection, removing and adding of agents performs on next step after current
-- Note: if ihibits one of the wires, it is removed from output result while it inhibits
dDynCollection :: forall m i a b c c2 . (ActorMonad m, Eq i, DynCollection c, FilterConstraint c (GameWireIndexed m i a b), FilterConstraint c (Either () b), F.Foldable c2) =>
    (c (GameActor m i a b)) -- ^ Inital set of wires
    -> GameWire m (a, Event (c (GameActor m i a b)), Event (c2 i)) (c b)
dDynCollection initialActors = mkGen $ \ds input -> do 
  arrs <- sequence initialActors
  go arrs ds input
  where 
  go :: c (GameWireIndexed m i a b)
    -> GameTime
    -> (a, Event (c (GameActor m i a b)), Event (c2 i))
    -> GameMonadT m (Either () (c b), GameWire m (a, Event (c (GameActor m i a b)), Event (c2 i)) (c b))
  go currentWires ds (a, addEvent, removeEvent) = do
    -- | Calculating outputs
    (bs, newWiresCntrls) <- liftM unzipDynColl $ mapM (\w -> stepWire w ds (Right a)) $ indexedWire <$> currentWires
    let newWires = uncurry updateIndexedWire <$> (fmap const newWiresCntrls `zipDynColl` currentWires)

    -- | Adding new wires
    newAddedWires <- onEvent newWires addEvent $ \newActors -> do 
      addWires <- sequence newActors 
      return $ newWires `concatDynColl` addWires

    -- | Removing wires
    newRemovedWires <- onEvent newAddedWires removeEvent $ \ids ->  
      return $ F.foldl' (\acc i -> fFilter ((/= i) . indexedId) acc) newAddedWires ids

    return $ length newRemovedWires `seq` (Right (rightsDynColl bs), mkGen $ go newRemovedWires)