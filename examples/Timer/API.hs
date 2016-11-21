module Timer.API(
    TimerMonad(..)
  , TimerT
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.Proxy
import Game.GoreAndAsh
import Data.Time

-- | API of logging module that is used by game logic code
class (Reflex t, Monad m) => TimerMonad t m | m -> t where
  -- | Get event that fires every n seconds
  tickEvery :: NominalDiffTime -> m (Event t ())

-- | Implementation basis of Timer API.
newtype TimerT t m a = TimerT { runTimerT :: m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance MonadTrans (TimerT t) where
  lift = TimerT

instance MonadSample t m => MonadSample t (TimerT t m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (TimerT t m) where
  hold            a b = lift $ hold a b
  holdDyn         a b = lift $ holdDyn a b
  holdIncremental a b = lift $ holdIncremental a b

instance MonadAppHost t m => MonadAppHost t (TimerT t m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- lift getRunAppHost
    return $ \m -> runner $ runTimerT m
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (TimerT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger a = lift $ newFanEventWithTrigger a

-- | Implement our Timer API
instance {-# OVERLAPPING #-} (Monad m, Reflex t, MonadAppHost t m) => TimerMonad t (TimerT t m) where
  tickEvery t = do
    (tickEvent, fireTick) <- newExternalEvent
    _ <- liftIO . forkIO $ ticker fireTick
    return tickEvent
    where
    ticker fire = do
      threadDelay (ceiling $ (realToFrac t :: Double) * 1000000)
      res <- fire ()
      if res then ticker fire
        else return ()
  {-# INLINE tickEvery #-}

-- | Automatic lifting across monad stack
instance {-# OVERLAPPABLE #-} (Monad (mt m), TimerMonad t m, MonadTrans mt) => TimerMonad t (mt m) where
  tickEvery = lift . tickEvery
  {-# INLINE tickEvery #-}

-- | The instance registers external events and process reaction to output events
instance (GameModule t m) => GameModule t (TimerT t m) where
  runModule m = runModule $ runTimerT m
  withModule t _ = withModule t (Proxy :: Proxy m)

  {-# INLINE runModule #-}
  {-# INLINE withModule #-}