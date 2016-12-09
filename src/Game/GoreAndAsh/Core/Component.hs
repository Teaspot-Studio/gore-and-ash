{-|
Module      : Game.GoreAndAsh.Core.Component
Description : Helpers for delaying creation of reflex components
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE RecursiveDo #-}
module Game.GoreAndAsh.Core.Component(
    Component
  , componentInfo
  , componentValue
  , DelayedComponent(..)
  , delayComponent
  , runComponent
  , runComponents
  , holdComponents
  , switchComponents
  ) where

import Reflex hiding (performEvent)
import Reflex.Host.App
import Reflex.Host.Class

import qualified Data.Traversable as T
import qualified Data.Foldable as F

-- | Built reflex component (FRP network and output of the component)
type Component t a = (AppInfo t, a)

-- | Get component description (how to execute it and which subscriptions it holds)
componentInfo :: Component t a -> AppInfo t
componentInfo = fst

-- | Get component return value (public API)
componentValue :: Component t a -> a
componentValue = snd

-- | Component could be either already built component or pending for creation
data DelayedComponent t a =
  -- | Holds component information and its return value
    BuiltComponent !(AppInfo t) !a
  -- | Holds 3-phase (see docs 'getRunAppHost') action for creating a component
  | NewComponent !(HostFrame t (HostFrame t (AppInfo t), a))

-- | Delay already created component (no-op)
delayComponent :: Component t a -> DelayedComponent t a
delayComponent (i, a) = BuiltComponent i a

-- | Execute non-built components
runComponent :: ReflexHost t => DelayedComponent t a -> HostFrame t (Component t a)
runComponent c = case c of
  BuiltComponent i a -> return (i, a)
  NewComponent m -> do
    (post, a) <- m
    i <- post
    return (i, a)

-- | Execute collection of components in host monad, but do not register the 'AppInfo' for this
-- action nor its postBuild actions.
-- Instead, the 'AppInfo' for this action is collected and returned.
--
-- For example, all 'performEvent_' calls inside the passed action will not actually be
-- performed, as long as the returned 'AppInfo' is not registered manually.
runComponents :: (MonadAppHost t m, T.Traversable f)
  => f (DelayedComponent t a)
  -> m (f (Component t a))
runComponents cmps = liftHostFrame $ T.traverse runComponent cmps

-- | Hold collection of components in host monad and produce dynamic of resulted 'AppInfo'
-- and values of the component. The function registers the produces app infos.
holdComponents :: (MonadAppHost t m, T.Traversable f)
  => f (DelayedComponent t a)
  -> Event t (f (DelayedComponent t a))
  -> m (Dynamic t (f (Component t a)))
holdComponents initCmps changedCmps = do
  initBuild <- runComponents initCmps
  let postActions = F.foldMap componentInfo initBuild
  aChanged <- switchComponents (pure postActions) changedCmps
  holdDyn initBuild aChanged

-- | Pefrorm and register components that are passed in event.
switchComponents :: (MonadAppHost t m, T.Traversable f)
  => HostFrame t (AppInfo t)
  -> Event t (f (DelayedComponent t a))
  -> m (Event t (f (Component t a)))
switchComponents initial event = do
  buildEvent :: Event t (f (Component t a)) <- performEvent $ T.traverse runComponent <$> event
  let infoEvent = ffor buildEvent $ F.foldMap componentInfo
  performPostBuild_ $ flip switchAppInfo infoEvent =<< initial
  return buildEvent