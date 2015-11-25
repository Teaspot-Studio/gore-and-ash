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
  ) where

import Prelude hiding (id, (.))
import Control.Wire
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