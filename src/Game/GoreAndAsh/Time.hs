{-|
Module      : Game.GoreAndAsh.Time
Description : Simple API for timer events
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE RecursiveDo #-}
module Game.GoreAndAsh.Time(
    TimerMonad(..)
  , tickOnce
  , alignWithFps
  , TimerT
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Thread.Delay as TD
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Data.IORef
import Data.Proxy
import Data.Time
import Game.GoreAndAsh

-- | API of logging module that is used by game logic code
class (Reflex t, MonadFix m) => TimerMonad t m | m -> t where
  -- | Get event that fires every n seconds.
  --
  -- Note: the event will tick even there are no subscriders to it. Use
  -- 'tickEveryUntil' if you want to free underlying thread.
  tickEvery :: NominalDiffTime -> m (Event t ())
  -- | Get event that fires every n seconds. Second event cease ticking.
  tickEveryUntil :: NominalDiffTime -> Event t a -> m (Event t ())
  -- | Get event that fires only after given event with specified delay
  delayBy :: NominalDiffTime -> Event t a -> m (Event t a)

-- | Generate an event after given time once
tickOnce :: TimerMonad t m => NominalDiffTime -> m (Event t ())
tickOnce dt = do
  rec e <- tickEveryUntil dt e
  return e

-- | Fire event not frequently as given frame per second ratio.
alignWithFps :: (TimerMonad t m, MonadAppHost t m)
  => Int -- ^ FPS (frames per second)
  -> Event t a -- ^ Event that frequently changes
  -> m (Event t a) -- ^ Event that changes are aligned with FPS
alignWithFps fps ea = do
  fpsE <- tickEvery . realToFrac $ 1 / (fromIntegral fps :: Double)
  ref <- liftIO $ newIORef Nothing
  performEvent_ $ ffor ea $ liftIO . atomicWriteIORef ref . Just
  alignedE <- performEvent $ ffor fpsE $ const $
    liftIO $ atomicModifyIORef' ref $ \v -> (Nothing, v)
  return $ fmapMaybe id alignedE


-- | Implementation basis of Timer API.
newtype TimerT t m a = TimerT { runTimerT :: IdentityT m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

-- | Unwrap newtype aka execute the monad layer
evalTimerT :: TimerT t m a -> m a
evalTimerT = runIdentityT . runTimerT

-- | Implement our Timer API
instance {-# OVERLAPPING #-} (MonadFix m, Reflex t, MonadAppHost t m) => TimerMonad t (TimerT t m) where
  tickEvery t = do
    (tickEvent, fireTick) <- newExternalEvent
    _ <- liftIO . forkIO $ ticker fireTick
    return tickEvent
    where
    ticker fire = do
      TD.delay (ceiling $ (realToFrac t :: Rational) * 1000000)
      res <- fire ()
      if res then ticker fire
        else return ()
  {-# INLINE tickEvery #-}

  tickEveryUntil t ceaseE = do
    (tickEvent, fireTick) <- newExternalEvent
    tid <- liftIO . forkIO $ ticker fireTick
    _ <- performEvent $ ffor ceaseE $ const $ liftIO $ killThread tid
    return tickEvent
    where
    ticker fire = do
      TD.delay (ceiling $ (realToFrac t :: Rational) * 1000000)
      res <- fire ()
      if res then ticker fire
        else return ()
  {-# INLINE tickEveryUntil #-}

  delayBy t e = performEventAsync $ ffor e $ \a -> do
    TD.delay (ceiling $ (realToFrac t :: Rational) * 1000000)
    return a
  {-# INLINE delayBy #-}

-- | Automatic lifting across monad stack
instance {-# OVERLAPPABLE #-} (MonadFix (mt m), TimerMonad t m, MonadTrans mt) => TimerMonad t (mt m) where
  tickEvery = lift . tickEvery
  tickEveryUntil a b = lift $ tickEveryUntil a b
  delayBy a b = lift $ delayBy a b
  {-# INLINE tickEvery #-}
  {-# INLINE tickEveryUntil #-}
  {-# INLINE delayBy #-}

-- | The instance registers external events and process reaction to output events
instance (GameModule t m) => GameModule t (TimerT t m) where
  type ModuleOptions t (TimerT t m) = ModuleOptions t m
  runModule opts m = runModule opts $ evalTimerT m
  withModule t _ = withModule t (Proxy :: Proxy m)

  {-# INLINE runModule #-}
  {-# INLINE withModule #-}

--------------------------------------------------------------------------------
-- Boilerplate
--------------------------------------------------------------------------------

instance MonadTrans (TimerT t) where
  lift = TimerT . IdentityT

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
    return $ \m -> runner $ evalTimerT $ m
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (TimerT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger a = lift $ newFanEventWithTrigger a

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (TimerT t m) where
  subscribeEvent = lift . subscribeEvent

instance MonadThrow m => MonadThrow (TimerT t m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (TimerT t m) where
  catch ma h = lift $ catch (evalTimerT ma) (evalTimerT . h)

instance MonadMask m => MonadMask (TimerT t m) where
  mask m = TimerT $ mask $ \u -> runTimerT (m $ q u)
    where
    q :: (forall b . IdentityT m b -> IdentityT m b) -> TimerT t m a -> TimerT t m a
    q u m' = TimerT $ u (runTimerT m')
  uninterruptibleMask m = TimerT $ uninterruptibleMask $ \u -> runTimerT (m $ q u)
    where
    q :: (forall b . IdentityT m b -> IdentityT m b) -> TimerT t m a -> TimerT t m a
    q u m' = TimerT $ u (runTimerT m')

instance MonadError e m => MonadError e (TimerT t m) where
  throwError = lift . throwError
  catchError ma h = lift $ catchError (evalTimerT ma) (evalTimerT . h)

instance MonadTransControl (TimerT t) where
  type StT (TimerT t) a = StT IdentityT a
  liftWith = defaultLiftWith TimerT runTimerT
  restoreT = defaultRestoreT TimerT

instance MonadBase b m => MonadBase b (TimerT t m) where
  liftBase = TimerT . liftBase

instance (MonadBaseControl b m) => MonadBaseControl b (TimerT t m) where
  type StM (TimerT t m) a = ComposeSt (TimerT t) m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM