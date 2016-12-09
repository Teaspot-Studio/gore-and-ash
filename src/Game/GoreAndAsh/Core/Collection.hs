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
import Game.GoreAndAsh.Core.Component
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
  run <- getRunAppHost
  let
    -- | Delay creation of a component
    delayMake :: k -> v -> DelayedComponent t a
    delayMake k v = NewComponent $ run $ makeItem k v

    -- | Run initial collection creation
    initCollection :: Map k (DelayedComponent t a)
    initCollection = M.mapWithKey delayMake initMap

    -- | Update collection of components (delete or insert/create) and delay construction
    -- of new ones.
    updateCollection :: Map k (DelayedComponent t a) -> k -> Maybe v -> Map k (DelayedComponent t a)
    updateCollection cmps k mv = case mv of
      Nothing -> M.delete k cmps
      Just v  -> M.insert k (delayMake k v) cmps

  -- Now lets create an update loop!
  rec
    dynComponents :: Dynamic t (Map k (Component t a)) <- holdComponents initCollection newComponentsE
    let
      newComponentsE :: Event t (Map k (DelayedComponent t a))
      newComponentsE = flip pushAlways updE $ \updM -> do
        curComponents <- sample (current dynComponents)
        let delayedComponents = fmap delayComponent curComponents
            newCollection = M.foldlWithKey updateCollection delayedComponents updM
        return newCollection

  return $ fmap componentValue <$> dynComponents
