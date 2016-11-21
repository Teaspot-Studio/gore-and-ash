{-|
Module      : Game.GoreAndAsh.Core.Monad
Description : Definition of game monad and core modules.
Copyright   : (c) Anton Gushcha, 2015-2016
                  Oganyan Levon, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module defines 'GameMonadT' monad transformer as base monad for engine.
Also there is 'GameModule' class that must be implemented by all core modules. Finally 'ModuleStack'
type family is for user usage to compose all modules in single monad stack.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Core.Monad(
    ReflexMonad
  , GameMonad
  , GameModule(..)
  -- * Reexports
  , module Reflex
  , module Reflex.Host.App
  , module Reflex.Host.Class
  ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Proxy
import GHC.Generics (Generic)
import Reflex hiding (performEvent, performEvent_, getPostBuild, performEventAsync)
import Reflex.Host.App
import Reflex.Host.Class

-- | Capture usual requirements for FRP monad
type ReflexMonad t m = (Reflex t, MonadSample t m, MonadFix m, MonadIO m)

-- | Describes how to run core modules. Each core module must define
-- an instance of the class.
--
-- The class describes how the module is executed each game frame, which external
-- events are created and which respond to output of FRP network is done.
class GameModule t gm where
  -- | Execution of reactive subsystem of module
  runModule :: MonadAppHost t m => gm a -> m a
  -- | Place when some external resouce initialisation can be placed
  withModule :: Proxy t -> Proxy gm -> IO a -> IO a

-- | State of core.
--
-- At the moment it is empty, but left for future
-- extensions. For example, some introspection API
-- of enabled modules would be added.
data GameContext t = GameContext {

} deriving Generic

instance NFData (GameContext t)

-- | Create empty context
newGameContext :: GameContext t
newGameContext = GameContext

-- | Endpoint for application monad stack that captures engine features for reactivity.
--
-- [@t@] FRP engine implementation (needed by reflex). Almost always you should
-- just type 't' in all user code in the place and ignore it.
--
-- [@a@] Value carried by the monad.
--
-- Should be used in application monad stack as end monad:
--
-- @
-- type AppMonad t a = LoggingT t (ActorT t (GameMonad t)) a
-- @
newtype GameMonad t a = GameMonad {
  runGameMonad :: forall m . MonadAppHost t m => StateT (GameContext t) m a
}

instance Functor (GameMonad t) where
  fmap f (GameMonad m) = GameMonad $ fmap f m

-- | Monad is needed as StateT Applicative instance requires it
instance Applicative (GameMonad t) where
  pure a = GameMonad $ pure a
  (GameMonad f) <*> (GameMonad m) = GameMonad $ f <*> m

instance Monad (GameMonad t) where
  return = pure
  (GameMonad ma) >>= f = GameMonad $ do
    a <- ma
    runGameMonad $ f a

instance MonadFix (GameMonad t) where
  mfix f = GameMonad $ mfix (runGameMonad . f)

instance MonadIO (GameMonad t) where
  liftIO m = GameMonad $ liftIO m

-- | Endpoint for 'ModuleStack' that captures engine features for reactivity.
--
-- Should be used in 'ModuleStack' as end monad:
--
-- @
-- type AppMonad t a = LoggingT t (ActorT t (GameMonad t)) a
-- @
instance GameModule t (GameMonad t) where
  runModule m = do
    (a, _) <- runStateT (runGameMonad m) newGameContext
    return a
  withModule _ _ = id
  {-# INLINE runModule #-}
  {-# INLINE withModule #-}

instance MonadAppHost t m => MonadAppHost t (StateT s m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- lift getRunAppHost
    s <- get
    return $ \m -> runner $ evalStateT m s

  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

