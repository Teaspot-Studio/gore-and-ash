module Game.GoreAndAsh.Core.Monad(
    GameMonadT
  , GameContext(..)
  , evalGameMonad
  ) where

import Control.Monad.State.Strict

-- | Basic game monad transformer
-- Here goes all core API that accessable from each 
-- game object. All specific (mods etc) API should
-- be included in inner `m` monad.
newtype GameMonadT m a = GameMonadT { 
  runGameMonadT :: StateT GameContext m a
}

-- | Data that accessable to objects during
-- game simulation step.
data GameContext = GameContext {
  
}

instance Functor m => Functor (GameMonadT m) where 
  fmap f (GameMonadT m) = GameMonadT $ fmap f m

-- | Monad is needed as StateT Applicative instance requires it
instance Monad m => Applicative (GameMonadT m) where
  pure a = GameMonadT $ pure a
  (GameMonadT f) <*> (GameMonadT m) = GameMonadT $ f <*> m

instance Monad m => Monad (GameMonadT m) where 
  return = pure 
  (GameMonadT ma) >>= f = GameMonadT $ do 
    a <- ma
    runGameMonadT $ f a

-- | Runs game monad with given context
evalGameMonad :: GameMonadT m a -> GameContext -> m (a, GameContext)
evalGameMonad (GameMonadT m) ctx = runStateT m ctx