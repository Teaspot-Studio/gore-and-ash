{-|
Module      : Game.GoreAndAsh.Core.Arrow
Description : Core operations with arrows.
Copyright   : (c) Anton Gushcha, 2015-2016
                  Oganyan Levon, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module defines 'GameWire' type as fundamental type for all applications arrows. Also
there are utilities for lifting 'GameMonadT' actions to 'GameWire', event processing helpers
and some other utilities.
-}
module Game.GoreAndAsh.Core.Arrow(
    GameWire
  -- * Lifting monad to arrow
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
  -- * Event functions
  , once'
  , mapE
  , filterE
  , filterEG
  , filterEGM
  , filterJustE
  , filterJustLE
  , liftGameMonadEvent1
  , changes
  -- * Helpers
  , stateWire
  , chainWires
  , dispense
  , dDispense
  -- * Time
  , deltaTime
  ) where

import Control.Monad.Fix
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Filterable
import Data.Maybe (fromJust, isJust)
import Prelude hiding (id, (.))

import Game.GoreAndAsh.Core.Monad
import Game.GoreAndAsh.Core.Session

-- | Game wire with given API 'm' and input value 'a' and output value 'b'.
--
-- Typically end point application defines a type synonyms:
--
-- @
-- -- | Arrow that is build over the monad stack
-- type AppWire a b = GameWire AppMonad a b
-- @
type GameWire m a b = Wire GameTime () (GameMonadT m) a b

-- | Takes game monad and wraps it into game wire.
--
-- Note: Result of wire is calclulated each frame.
liftGameMonad :: Monad m => GameMonadT m b -> GameWire m a b
liftGameMonad action = mkGen_ $ \ _ -> do 
  val <- action 
  return $ Right val

-- | Takes game monad and wraps it into game wire.
--
-- Note: Result of wire is calclulated each frame.
liftGameMonad1 :: Monad m => (a -> GameMonadT m b) -> GameWire m a b
liftGameMonad1 action = mkGen_ $ \ a -> do 
  val <- action a
  return $ Right val

-- | Takes game monad and wraps it into game wire.
--
-- Note: Result of wire is calclulated each frame.
liftGameMonad2 :: Monad m => (a -> b -> GameMonadT m c) -> GameWire m (a, b) c
liftGameMonad2 action = mkGen_ $ \ (a, b) -> do 
  val <- action a b
  return $ Right val

-- | Takes game monad and wraps it into game wire.
--
-- Note: Result of wire is calclulated each frame.
liftGameMonad3 :: Monad m => (a -> b -> c -> GameMonadT m d) -> GameWire m (a, b, c) d
liftGameMonad3 action = mkGen_ $ \ (a, b, c) -> do 
  val <- action a b c
  return $ Right val

-- | Takes game monad and wraps it into game wire.
--
-- Note: Result of wire is calclulated each frame.
liftGameMonad4 :: Monad m => (a -> b -> c -> d -> GameMonadT m e) -> GameWire m (a, b, c, d) e
liftGameMonad4 action = mkGen_ $ \ (a, b, c, d) -> do 
  val <- action a b c d
  return $ Right val

-- | Takes game monad and wraps it into game wire.
--
-- Note: Result of wire is calculated ONCE and next execution returns cached value
liftGameMonadOnce :: Monad m => GameMonadT m b -> GameWire m a b 
liftGameMonadOnce action = mkGen $ \_ _ -> do 
  val <- action 
  return (Right val, pure val)

-- | Takes game monad and wraps it into game wire.
--
-- Note: Result of wire is calculated ONCE and next execution returns cached value
liftGameMonad1Once :: Monad m => (a -> GameMonadT m b) -> GameWire m a b 
liftGameMonad1Once action = mkGen $ \_ a -> do 
  val <- action a
  return (Right val, pure val)

-- | Takes game monad and wraps it into game wire.
--
-- Note: Result of wire is calculated ONCE and next execution returns cached value
liftGameMonad2Once :: Monad m => (a -> b -> GameMonadT m c) -> GameWire m (a, b) c 
liftGameMonad2Once action = mkGen $ \_ (a, b) -> do 
  val <- action a b
  return (Right val, pure val)

-- | Takes game monad and wraps it into game wire.
--
-- Note: Result of wire is calculated ONCE and next execution returns cached value
liftGameMonad3Once :: Monad m => (a -> b -> c -> GameMonadT m d) -> GameWire m (a, b, c) d 
liftGameMonad3Once action = mkGen $ \_ (a, b, c) -> do 
  val <- action a b c
  return (Right val, pure val)

-- | Takes game monad and wraps it into game wire.
--
-- Note: Result of wire is calculated ONCE and next execution returns cached value
liftGameMonad4Once :: Monad m => (a -> b -> c -> d -> GameMonadT m e) -> GameWire m (a, b, c, d) e 
liftGameMonad4Once action = mkGen $ \_ (a, b, c, d) -> do 
  val <- action a b c d
  return (Right val, pure val)

-- | Pass through first occurence and then forget about event producer.
--
-- Note: netwire once combinator still holds it event producer when event
-- is produced.
once' :: Monad m => GameWire m a (Event b) -> GameWire m a (Event b)
once' w = proc a -> do 
  e <- w -< a 
  drSwitch id -< (e, fmap (const never) e)

-- | Mapping events as a wire.
--
-- It is semantically equal to:
--
-- >>> arr (fmap f)
mapE :: Monad m => (a -> b) -> GameWire m (Event a) (Event b)
mapE f = arr $ \e -> case e of 
  NoEvent -> NoEvent
  Event a -> Event $ f a 

-- | Same as 'filterE' but for generic 'Foldable' and 'Filterable'.
filterEG :: (Foldable f, Filterable f, FilterConstraint f a, Monad m)
  => (a -> Bool) -- ^ Predicate to test elements that are left in collection
  -> GameWire m (Event (f a)) (Event (f a)) -- ^ Wire that leaves only non empty collections
filterEG p = arr $ \e -> case e of 
  NoEvent -> NoEvent
  Event as -> let
    as' = fFilter p as
    in if fNull as' 
      then NoEvent
      else length as' `seq` Event as'

-- | Same as 'filterEG' but with monadic action.
filterEGM :: (Foldable f, Filterable f, FilterConstraint f a, Monad m)
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
--
-- Shortcut for:
--
-- >>> mapE fromJust . filterE isJust
filterJustE :: Monad m => GameWire m (Event (Maybe a)) (Event a)
filterJustE = mapE fromJust . filterE isJust

-- | Filters only Just events in foldable struct
filterJustLE :: (Monad m, Filterable f, FilterConstraint f (Maybe a), Functor f) => GameWire m (Event (f (Maybe a))) (Event (f a))
filterJustLE = mapE (fmap fromJust . fFilter isJust)

-- | Lifting game monad action to event processing arrow
--
-- Synonym for 'onEventM' from "Control.Wire.Core.Unsafe.Event".
liftGameMonadEvent1 :: Monad m => (a -> GameMonadT m b) -> GameWire m (Event a) (Event b)
liftGameMonadEvent1 = onEventM

-- | Loops output of wire to it input, first parameter is start value of state
--
-- Common combinator for build game actors.
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

-- | Infinitely dispense given elements and switches to next item on event.
--
-- Note: is not defined on empty list.
--
-- Note: not delayed version, new item is returned on same frame when input event occurs.
dispense :: (Monad m) => [a] -> GameWire m (Event b) a
dispense = go . cycle
  where
    go [] = error "dispense: empty list"
    go (a:as) = mkPureN $ \e -> case e of 
      NoEvent -> (Right a, go $ a:as)
      Event _ -> (Right $ head as, go as)

-- | Infinitely dispense given elements and switches to next item on event.
--
-- Note: is not defined on empty list.
--
-- Note: delayed version, new item is returned on frame after input event occurs.
dDispense :: (Monad m) => [a] -> GameWire m (Event b) a
dDispense = go . cycle
  where
    go [] = error "dDispense: empty list" 
    go (a:as) = mkPureN $ \e -> case e of 
      NoEvent -> (Right a, go $ a:as)
      Event _ -> (Right a, go as)

-- | Returns delta time scince last frame.
deltaTime :: (Fractional b, Monad m) => GameWire m a b 
deltaTime = mkSF $ \ds _ -> let t = realToFrac (dtime ds) in t `seq` (t, deltaTime)