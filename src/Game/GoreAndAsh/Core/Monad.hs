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
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Core.Monad(
    ReflexMonad
  , GameContext
  , GameMonad
  , GameModule(..)
  -- * Reexports
  , module Reflex
  , module Reflex.Host.App
  , module Reflex.Host.Class
  ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.RSS.Strict
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
class (ReflexHost t, Monad gm) => GameModule t gm where
  -- | Execution of reactive subsystem of module
  runModule :: gm a -> AppHost t a

  -- | Place when some external resouce initialisation can be placed
  withModule :: Proxy t -> Proxy gm -> IO a -> IO a

  -- -- | Gracefull cleanup of module resources
  -- cleanupModule :: MonadIO m => Proxy t -> Proxy gm -> m ()

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
-- type AppMonad t a = LoggingT (ActorT (GameMonad t)) a
-- @
newtype GameMonad t a = GameMonad {
  runGameMonad :: StateT (GameContext t) (AppHost t) a
}

instance ReflexHost t => Functor (GameMonad t) where
  fmap f (GameMonad m) = GameMonad $ fmap f m

-- | Monad is needed as StateT Applicative instance requires it
instance ReflexHost t => Applicative (GameMonad t) where
  pure a = GameMonad $ pure a
  (GameMonad f) <*> (GameMonad m) = GameMonad $ f <*> m

instance ReflexHost t => Monad (GameMonad t) where
  return = pure
  (GameMonad ma) >>= f = GameMonad $ do
    a <- ma
    runGameMonad $ f a

instance ReflexHost t => MonadFix (GameMonad t) where
  mfix f = GameMonad $ mfix (runGameMonad . f)

instance (MonadIO (HostFrame t), ReflexHost t) => MonadIO (GameMonad t) where
  liftIO m = GameMonad $ liftIO m

-- | Endpoint for 'ModuleStack' that captures engine features for reactivity.
--
-- Should be used in 'ModuleStack' as end monad:
--
-- @
-- type AppMonad t a = LoggingT (ActorT (GameMonad t)) a
-- @
instance ReflexHost t => GameModule t (GameMonad t) where
  runModule m = do
    (!a, _) <- runStateT (runGameMonad m) newGameContext
    return a
  withModule _ _ = id
  --cleanupModule _ _ _ = return ()

  {-# INLINE runModule #-}
  {-# INLINE withModule #-}
  --{-# INLINE cleanupModule #-}

instance ReflexHost t => MonadSample t (GameMonad t) where
  sample = GameMonad . sample

instance ReflexHost t => MonadHold t (GameMonad t) where
  hold            a b = GameMonad $ hold a b
  holdDyn         a b = GameMonad $ holdDyn a b
  holdIncremental a b = GameMonad $ holdIncremental a b

instance ReflexHost t => MonadSubscribeEvent t (GameMonad t) where
  subscribeEvent = GameMonad . subscribeEvent

instance ReflexHost t => MonadReflexCreateTrigger t (GameMonad t) where
  newEventWithTrigger = GameMonad . newEventWithTrigger
  newFanEventWithTrigger trigger = GameMonad $ newFanEventWithTrigger trigger

instance (MonadIO (HostFrame t), ReflexHost t) => MonadAppHost t (GameMonad t) where
  getFireAsync = GameMonad getFireAsync
  getRunAppHost = do
    runner <- GameMonad getRunAppHost
    return $ \m -> runner $ runGameMonad m
  performPostBuild_ = GameMonad . performPostBuild_
  liftHostFrame = GameMonad . liftHostFrame

-- TODO: move this to reflex-host
instance MonadAppHost t m => MonadAppHost t (StateT s m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- lift getRunAppHost
    s <- get
    return $ \m -> runner $ evalStateT m s
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadAppHost t m => MonadAppHost t (ReaderT s m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- lift getRunAppHost
    s <- ask
    return $ \m -> runner $ runReaderT m s
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadSample t m => MonadSample t (RSST r w s m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (RSST r w s m) where
  hold            a b = lift $ hold a b
  holdDyn         a b = lift $ holdDyn a b
  holdIncremental a b = lift $ holdIncremental a b

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (RSST r w s m) where
  subscribeEvent = lift . subscribeEvent

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (RSST r w s m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger trigger = lift $ newFanEventWithTrigger trigger

instance (Monoid w, MonadAppHost t m) => MonadAppHost t (RSST r w s m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- lift getRunAppHost
    r <- ask
    s <- get
    return $ \m -> runner . fmap (\(a, _, _) -> a) $ runRSST m r s
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame