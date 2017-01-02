module Game.GoreAndAsh.Core.Host(
    switchKeyAppInfo
  , holdKeyAppHost
  ) where

import Control.Monad ((>=>))
import Control.Monad.Fix
import Data.Filterable
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Semigroup.Applicative
import Reflex hiding (performEvent)
import Reflex.Host.App
import Reflex.Host.App.Internal
import Reflex.Host.Class

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M

-- | Helper to merge update map with current state
updateMap :: Ord k => Map k (Maybe a) -> Map k a -> Map k a
updateMap updMap curMap = M.foldlWithKey' go curMap updMap
  where
    go m k Nothing = M.delete k m
    go m k (Just v) = M.insert k v m

-- | Switch to a different 'AppInfo' whenever an 'Event' fires. Only the events of the
-- currently active application are performed. Unlike 'switchAppInfo' the primitive
-- allows partial update of FRP network.
--
-- This low-level primitive is used for implementing higher-level functions such as
-- 'switchKeyAppHost'.
switchKeyAppInfo :: forall t m k .
    (Reflex t, MonadHold t m, MonadFix m, Applicative (HostFrame t), Ord k)
  => Map k (AppInfo t) -- ^ Initial FRP application
  -- | Updates, 'Nothing' deletes specified key and 'Just' adds/overwrite given key
  -> Event t (Map k (Maybe (AppInfo t)))
  -- | Collected application info
  -> m (AppInfo t)
switchKeyAppInfo initialMap updatedMap = do
  -- calculate eventsToPerform events
  let initialPerforms :: Map k (Event t (AppPerformAction t))
      initialPerforms = fst <$> initialEvents

      performsEvent :: Event t (Map k (Maybe (Event t (AppPerformAction t))))
      performsEvent = fmap (fmap fst) <$> updateEvents

  toPerformMap <- switch . current . fmap mergeMap <$> foldDyn updateMap initialPerforms performsEvent
  let toPerform = getAp . F.foldMap Ap <$> toPerformMap

  -- calculate eventsToQuit
  let initialToQuit :: Map k (Event t ())
      initialToQuit = snd <$> initialEvents

      toQuitEvent :: Event t (Map k (Maybe (Event t ())))
      toQuitEvent = fmap (fmap snd) <$> updateEvents

  toQuitMap <- switch . current . fmap mergeMap <$> foldDyn updateMap initialToQuit toQuitEvent
  let toQuit = F.foldMap id <$> toQuitMap

  pure AppInfo {
      eventsToPerform = pure toPerform <> pure updatedTriggers
    , eventsToQuit    = pure toQuit
    , triggersToFire  = F.foldMap id initialTriggers
    }
  where
    initialEvents = fmap appInfoEvents initialMap
    updateEvents = fmap (fmap appInfoEvents) <$> updatedMap
    initialTriggers = fmap triggersToFire initialMap
    updatedTriggers = getAp . F.foldMap id . fmap triggersToFire . cutMaybes <$> updatedMap

-- | Helper to execute host action in one step (instead of usual 3 steps)
getRunWithPost :: MonadAppHost t m => m (m a -> HostFrame t (AppInfo t, a))
getRunWithPost = do
  run <- getRunAppHost
  return $ run >=> \(post, a) -> (,a) <$> post

-- | Switch to a different host action after an event fires. Only the 'AppInfo' of the
-- currently active application is registered. For example, 'performEvent' calls are only
-- executed for the currently active application. As soon as it is switched out and
-- replaced by a different application, they are no longer executed.
--
-- The first argument specifies the postBuild action that is used initially, before the
-- event fires the first time.
--
-- Whenever a switch to a new host action happens, the returned event is fired in the
-- next frame with the result of running it.
holdKeyAppHost :: forall t m a k . (MonadAppHost t m, Ord k)
  => Map k (m a)
  -> Event t (Map k (Maybe (m a)))
  -> m (Dynamic t (Map k a))
holdKeyAppHost initialMap event = do
  -- describe how to execute partial updates
  run <- getRunAppHost
  postRun <- getRunWithPost
  let
    executedEvent :: Event t (Map k (Maybe (HostFrame t (AppInfo t, a))))
    executedEvent = fmap (fmap postRun) <$> event

    liftedEvent :: Event t (HostFrame t (Map k (Maybe (AppInfo t, a))))
    liftedEvent = sequence . fmap sequence <$> executedEvent

  -- execute partial updates to split values and app infos
  updMap :: Event t (Map k (Maybe (AppInfo t, a))) <- performEvent liftedEvent
  let
    updInfoMap :: Event t (Map k (Maybe (AppInfo t)))
    updInfoMap = fmap (fmap fst) <$> updMap

    updValueMap :: Event t (Map k (Maybe a))
    updValueMap = fmap (fmap snd) <$> updMap

  -- construct initial values
  initial :: Map k (HostFrame t (AppInfo t), a) <- liftHostFrame . sequence $ run <$> initialMap
  let
    initialInfo :: HostFrame t (Map k (AppInfo t))
    initialInfo = sequence . fmap fst $ initial

    initialValues :: Map k a
    initialValues = fmap snd initial

  -- register partial updates
  performPostBuild_ $ flip switchKeyAppInfo updInfoMap =<< initialInfo

  -- collect computed outputs
  foldDyn updateMap initialValues updValueMap
