module Game.GoreAndAsh.Core.Arrow(
    GameWire
  -- | Lifting monad to arrow
  , liftGameMonad
  , liftGameMonad1
  , liftGameMonad2
  , liftGameMonad3
  , liftGameMonad4
  , liftGameMonadOnce
  , liftGameMonad1Once
  , liftGameMonad2Once
  , liftGameMonad3Once
  , liftGameMonad4Once
  -- | Event functions
  , once'
  , mapE
  , filterE
  , Filterable(..)
  , KeyHashMap(..)
  , filterEG
  , filterEGM
  , filterJustE
  , filterJustLE
  , liftGameMonadEvent1
  , changes
  -- | Helpers
  , stateWire
  , chainWires
  , dispense
  , dDispense
  ) where

import Control.Monad (filterM)
import Control.Monad.Fix
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Hashable 
import Data.Maybe (fromJust, isJust)
import Prelude hiding (id, (.))
import qualified Data.Foldable as F 
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

import Game.GoreAndAsh.Core.Monad
import Game.GoreAndAsh.Core.Session

-- | Game wire with given API @m@ and input value @a@ and output value @b@
type GameWire m a b = Wire GameTime () (GameMonadT m) a b

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calclulated each frame.
liftGameMonad :: Monad m => GameMonadT m b -> GameWire m a b
liftGameMonad action = mkGen_ $ \ _ -> do 
  val <- action 
  return $ Right val

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calclulated each frame.
liftGameMonad1 :: Monad m => (a -> GameMonadT m b) -> GameWire m a b
liftGameMonad1 action = mkGen_ $ \ a -> do 
  val <- action a
  return $ Right val

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calclulated each frame.
liftGameMonad2 :: Monad m => (a -> b -> GameMonadT m c) -> GameWire m (a, b) c
liftGameMonad2 action = mkGen_ $ \ (a, b) -> do 
  val <- action a b
  return $ Right val

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calclulated each frame.
liftGameMonad3 :: Monad m => (a -> b -> c -> GameMonadT m d) -> GameWire m (a, b, c) d
liftGameMonad3 action = mkGen_ $ \ (a, b, c) -> do 
  val <- action a b c
  return $ Right val

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calclulated each frame.
liftGameMonad4 :: Monad m => (a -> b -> c -> d -> GameMonadT m e) -> GameWire m (a, b, c, d) e
liftGameMonad4 action = mkGen_ $ \ (a, b, c, d) -> do 
  val <- action a b c d
  return $ Right val

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calculated ONCE and next execution returns cached value
liftGameMonadOnce :: Monad m => GameMonadT m b -> GameWire m a b 
liftGameMonadOnce action = mkGen $ \_ _ -> do 
  val <- action 
  return (Right val, pure val)

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calculated ONCE and next execution returns cached value
liftGameMonad1Once :: Monad m => (a -> GameMonadT m b) -> GameWire m a b 
liftGameMonad1Once action = mkGen $ \_ a -> do 
  val <- action a
  return (Right val, pure val)

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calculated ONCE and next execution returns cached value
liftGameMonad2Once :: Monad m => (a -> b -> GameMonadT m c) -> GameWire m (a, b) c 
liftGameMonad2Once action = mkGen $ \_ (a, b) -> do 
  val <- action a b
  return (Right val, pure val)

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calculated ONCE and next execution returns cached value
liftGameMonad3Once :: Monad m => (a -> b -> c -> GameMonadT m d) -> GameWire m (a, b, c) d 
liftGameMonad3Once action = mkGen $ \_ (a, b, c) -> do 
  val <- action a b c
  return (Right val, pure val)

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calculated ONCE and next execution returns cached value
liftGameMonad4Once :: Monad m => (a -> b -> c -> d -> GameMonadT m e) -> GameWire m (a, b, c, d) e 
liftGameMonad4Once action = mkGen $ \_ (a, b, c, d) -> do 
  val <- action a b c d
  return (Right val, pure val)

-- | Pass through first occurence and then forget about event producer
-- Note: netwire once combinator still holds it event producer when event
-- is produced.
once' :: Monad m => GameWire m a (Event b) -> GameWire m a (Event b)
once' w = proc a -> do 
  e <- w -< a 
  drSwitch id -< (e, fmap (const never) e)

-- | Mapping events as a wire
mapE :: Monad m => (a -> b) -> GameWire m (Event a) (Event b)
mapE f = arr $ \e -> case e of 
  NoEvent -> NoEvent
  Event a -> Event $ f a 

-- | Generic filter for collections
class Filterable f where 
  -- | Test collection for emptiness
  fNull :: f a -> Bool 
  -- | Filter function for collection
  fFilter :: (a -> Bool) -> f a -> f a
  -- | Monad version of filter 
  fFilterM :: (Eq a, Hashable a, Monad m) => (a -> m Bool) -> f a -> m (f a)

instance Filterable [] where 
  fNull = null  
  fFilter = filter 
  fFilterM = filterM

instance Filterable S.Seq where 
  fNull = S.null  
  fFilter = S.filter 
  fFilterM p = F.foldlM (\xs x -> do
    f <- p x 
    return $! if f then xs S.|> x else xs) S.empty

-- | Wrapper around HashMap to Filterable instance over keys
newtype KeyHashMap v k = KeyHashMap { unKeyHashMap :: H.HashMap k v }

instance Filterable (KeyHashMap v) where
  fNull = H.null . unKeyHashMap
  fFilter p (KeyHashMap m) = KeyHashMap $ H.filterWithKey (\k _ -> p k) m
  fFilterM p (KeyHashMap m) = fmap KeyHashMap $ H.foldlWithKey' (\mxs k x -> do 
    xs <- mxs
    f <- p k 
    return $! if f then H.insert k x xs else xs) (return H.empty) m

instance (Eq k, Hashable k) => Filterable (H.HashMap k) where
  fNull = H.null  
  fFilter = H.filter 
  fFilterM p = H.foldlWithKey' (\mxs k x -> do 
    xs <- mxs
    f <- p x
    return $! if f then H.insert k x xs else xs) (return H.empty)

-- | Same as @filterE@ but for generic collections
filterEG :: (Foldable f, Filterable f, Monad m, Eq a, Hashable a)
  => (a -> Bool) -- ^ Predicate to test elements that are left in collection
  -> GameWire m (Event (f a)) (Event (f a)) -- ^ Wire that leaves only non empty collections
filterEG p = arr $ \e -> case e of 
  NoEvent -> NoEvent
  Event as -> let
    as' = fFilter p as
    in if fNull as' 
      then NoEvent
      else length as' `seq` Event as'

-- | Same as @filterEG@ but applicative
filterEGM :: (Foldable f, Filterable f, Monad m, Eq a, Hashable a)
  => (a -> GameMonadT m Bool) -- ^ Predicate to test elements that are left in collection
  -> GameWire m (Event (f a)) (Event (f a)) -- ^ Wire that leaves only non empty collections
filterEGM p = mkGen_ $ \e -> case e of 
  NoEvent -> return $! Right NoEvent
  Event as -> do
    as' <- fFilterM p as
    if fNull as' 
      then return $! Right NoEvent
      else return . Right $! length as' `seq` Event as'

-- | Filters only Just events
filterJustE :: Monad m => GameWire m (Event (Maybe a)) (Event a)
filterJustE = mapE fromJust . filterE isJust

-- | Filters only Just events in foldable struct
filterJustLE :: (Monad m, Filterable f, Functor f) => GameWire m (Event (f (Maybe a))) (Event (f a))
filterJustLE = mapE (fmap fromJust . fFilter isJust)

-- | Lifting game monad action to event processing arrow
liftGameMonadEvent1 :: Monad m => (a -> GameMonadT m b) -> GameWire m (Event a) (Event b)
liftGameMonadEvent1 = onEventM

-- | Loops output of wire to it input, first parameter is start value of state
stateWire :: MonadFix m => b -> GameWire m (a, b) b -> GameWire m a b
stateWire ib w = loop $ proc (a, b_) -> do 
  b <- delay ib -< b_ -- either it will hang
  b2 <- w -< (a, b)
  returnA -< (b2, b2)

-- | Sequence compose list of wires (right to left order)
chainWires :: Monad m => [GameWire m a a] -> GameWire m a a 
chainWires [] = id 
chainWires (w:ws) = w . chainWires ws

-- | Fires when input value changes
changes :: (Monad m, Eq a) => GameWire m a (Event a)
changes = mkPureN $ \a -> (Right $! Event a, go a)
  where
    go cura = mkPureN $ \a -> if a == cura 
      then (Right NoEvent, go cura)
      else a `seq` (Right $! Event a, go a)

-- | Infinitely dispense given elements and switches to next item on event
-- Note: is not defined on empty list.
-- Note: not delayed version, new item is returned on same frame when input event occurs
dispense :: (Monad m) => [a] -> GameWire m (Event b) a
dispense = go . cycle
  where
    go [] = error "dispense: empty list"
    go (a:as) = mkPureN $ \e -> case e of 
      NoEvent -> (Right a, go $ a:as)
      Event _ -> (Right $ head as, go as)

-- | Infinitely dispense given elements and switches to next item on event
-- Note: is not defined on empty list.
-- Note: delayed version, new item is returned on frame after input event occurs
dDispense :: (Monad m) => [a] -> GameWire m (Event b) a
dDispense = go . cycle
  where
    go [] = error "dDispense: empty list" 
    go (a:as) = mkPureN $ \e -> case e of 
      NoEvent -> (Right a, go $ a:as)
      Event _ -> (Right a, go as)
