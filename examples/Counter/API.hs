module Counter.API(
    CounterMonad(..)
  , CounterT
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.IORef
import Data.Proxy
import Game.GoreAndAsh

-- | API of logging module that is used by game logic code
class (Reflex t, Monad m) => CounterMonad t m | m -> t where
  -- | Increment internal counter by 1 every tick
  incCounter :: Event t a -> m ()
  -- | Get value of counter
  getCounter :: m (Dynamic t Int)

-- | Internal state of module
data CounterState t = CounterState {
  stateVal :: !(IORef Int)
, stateNotify :: !(Int -> IO ())
, stateValEvent :: !(Event t Int)
}

-- | Helper to increment counter in internal state
incCounterState :: MonadIO m => CounterState t -> m ()
incCounterState CounterState{..} = do
  v <- liftIO $ atomicModifyIORef' stateVal (\v -> (v + 1, v + 1))
  liftIO $ stateNotify v

-- | Implementation basis of Counter API.
newtype CounterT t m a = CounterT { runCounterT :: ReaderT (CounterState t) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadReader (CounterState t))

-- | Implement our Counter API
instance {-# OVERLAPPING #-} (Monad m, Reflex t, MonadAppHost t m) => CounterMonad t (CounterT t m) where
  incCounter e = do
    st <- ask
    performEvent_ $ ffor e $ const $ incCounterState st
  getCounter = do
    CounterState{..} <- ask
    v <- liftIO $ readIORef stateVal
    holdDyn v stateValEvent
  {-# INLINE incCounter #-}
  {-# INLINE getCounter #-}

-- | Automatic lifting across monad stack
instance {-# OVERLAPPABLE #-} (Monad (mt m), CounterMonad t m, MonadTrans mt) => CounterMonad t (mt m) where
  incCounter = lift . incCounter
  getCounter = lift getCounter
  {-# INLINE incCounter #-}
  {-# INLINE getCounter #-}

-- | The instance registers external events and process reaction to output events
instance (GameModule t m, MonadIO (HostFrame t)) => GameModule t (CounterT t m) where
  runModule m = do
    (countEvent, countFire) <- newExternalEvent
    countRef <- liftIO $ newIORef 0
    let countFire' = void . countFire
    runModule $ runReaderT (runCounterT m) (CounterState countRef countFire' countEvent)
  withModule t _ = withModule t (Proxy :: Proxy m)
  {-# INLINE runModule #-}
  {-# INLINE withModule #-}

--------------------------------------------------------------------------------
-- Boilerplate
--------------------------------------------------------------------------------

instance MonadTrans (CounterT t) where
  lift = CounterT . lift

instance MonadSample t m => MonadSample t (CounterT t m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (CounterT t m) where
  hold            a b = lift $ hold a b
  holdDyn         a b = lift $ holdDyn a b
  holdIncremental a b = lift $ holdIncremental a b

instance MonadAppHost t m => MonadAppHost t (CounterT t m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- CounterT getRunAppHost
    return $ \m -> runner $ runCounterT m
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (CounterT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger a = lift $ newFanEventWithTrigger a

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (CounterT r m) where
  subscribeEvent = lift . subscribeEvent