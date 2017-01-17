{-|
Module      : Game.GoreAndAsh.Core.Collection
Description : Primitivies for dynamic collections of components.
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE RecursiveDo #-}
module Game.GoreAndAsh.Core.Collection(
    holdKeyCollection
  ) where

import Data.Map.Strict (Map)
import Reflex hiding (performEvent, performEvent_, getPostBuild, performEventAsync)
import Reflex.Host.App

import qualified Data.Map.Strict as M

-- | Construct dynamic collection of components with incremental update.
--
-- The function is likely your rescue if:
--
-- * You have components that can be indexed by some value.
--
-- * You want to add them dynamically.
--
-- * You want to remove them dynamically.
--
-- * You don't want to recreate old components when a new one is added.
holdKeyCollection :: forall t m k v a . (Ord k, MonadAppHost t m)
  => Map k v -- ^ Initial set of components
  -> Event t (Map k (Maybe v)) -- ^ Nothing entries delete component, Just ones create or replace
  -> (k -> v -> m a) -- ^ Constructor of widget
  -> m (Dynamic t (Map k a)) -- ^ Collected output of components
holdKeyCollection initMap updE makeItem = do
  let initial = M.mapWithKey makeItem initMap
      updateE = M.mapWithKey (\k -> fmap (makeItem k)) <$> updE
  holdKeyAppHost initial updateE
