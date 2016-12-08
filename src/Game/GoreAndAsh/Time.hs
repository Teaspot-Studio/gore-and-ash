{-|
Module      : Game.GoreAndAsh.Time
Description : Simple API for timer events
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Time(
    TimerMonad(..)
  , TimerT
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Data.Proxy
import Game.GoreAndAsh
import Data.Time

-- | API of logging module that is used by game logic code
class (Reflex t, Monad m) => TimerMonad t m | m -> t where
  -- | Get event that fires every n seconds
  tickEvery :: NominalDiffTime -> m (Event t ())

-- | Implementation basis of Timer API.
newtype TimerT t m a = TimerT { runTimerT :: IdentityT m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

-- | Unwrap newtype aka execute the monad layer
evalTimerT :: TimerT t m a -> m a
evalTimerT = runIdentityT . runTimerT

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