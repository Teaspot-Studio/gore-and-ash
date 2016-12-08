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
  ) where

import Reflex.Host.App
import Reflex.Host.Class

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
