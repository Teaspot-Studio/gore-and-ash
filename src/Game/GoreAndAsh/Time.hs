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
  , AlignWithFps(..)
  , tickEveryN
  , TimerT
  ) where

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
import Game.GoreAndAsh.Core.ExternalRef
import Game.GoreAndAsh.Core.Monad
import GHC.Event hiding (Event)

-- | API of logging module that is used by game logic code
class (Reflex t, MonadFix m) => TimerMonad t m | m -> t where
  -- | Get event that fires every n seconds.
  --
  -- Note: the event will tick even there are no subscriders to it. Use
  -- 'tickEveryUntil' if you want to free underlying thread.
  tickEvery :: NominalDiffTime -> m (Event t ())
  -- | Generate an event after given time once
  tickOnce :: NominalDiffTime -> m (Event t ())
  -- | Get event that fires every n seconds. Second event cease ticking.
  tickEveryUntil :: NominalDiffTime -> Event t a -> m (Event t ())
  -- | Get event that fires only after given event with specified delay
  delayBy :: NominalDiffTime -> Event t a -> m (Event t a)

-- | Holds shared functions between Event and Dynamic to align occurences with time
class AlignWithFps r where
  -- | Fire event not frequently as given frame per second ratio. Starts global
  -- counter, so first occurence of resulted event can be delayed by 1/fps seconds.
  -- The resulted event fires with last occurence of original event during 1/fps
  -- interval.
  alignWithFps :: (TimerMonad t m, MonadAppHost t m)
    => Int -- ^ FPS (frames per second)
    -> r t a -- ^ Event/Dynamic that frequently changes
    -> m (r t a) -- ^ Event/Dynamic that changes are aligned with FPS

  -- | Fire event not frequently as given frame per second ratio. Doesn't delay
  -- first occurence, only remembers last event occurence after 1/fps time
  -- interval, it is refired at end of the interval.
  limitRate :: (TimerMonad t m, MonadAppHost t m)
    => Int -- ^ FPS or rate
    -> r t a -- ^ Event/Dynamic which occurences you want to limit
    -> m (r t a) -- ^ Event/Dynamic that changes are filtered with given rate

instance AlignWithFps Event where
  alignWithFps fps ea = do
    fpsE <- tickEvery . realToFrac $ 1 / (fromIntegral fps :: Double)
    ref <- liftIO $ newIORef Nothing
    performEvent_ $ ffor ea $ liftIO . atomicWriteIORef ref . Just
    alignedE <- performEvent $ ffor fpsE $ const $
      liftIO $ atomicModifyIORef' ref $ \v -> (Nothing, v)
    return $ fmapMaybe id alignedE

  limitRate fps ea = do
    -- Create variable with gate flag
    ref <- liftIO $ newIORef False
    refLast <- liftIO $ newIORef Nothing -- Store last occurence to reemit it
    -- Pass values when gate is open
    msilencedE <- performEvent $ ffor ea $ \v -> do
      silenced <- liftIO $ readIORef ref
      if silenced
        then do
          liftIO $ writeIORef refLast (Just v)
          return Nothing
        else return $ Just v
    let silencedE = fmapMaybe id msilencedE -- remove Nothing from occurences
    -- Close gate for dt after each final occurence
    rec
      lastEDyn <- holdAppHost (pure never) $ ffor resultE $ const $ do
        liftIO $ writeIORef ref True
        let dt = realToFrac $ 1 / (fromIntegral fps :: Double)
        releaseE <- tickOnce dt
        lastE <- performEvent $ ffor releaseE $ const $ liftIO $ do
          writeIORef ref False
          atomicModifyIORef' refLast $ \v -> (Nothing, v)
        return $ fmapMaybe id lastE -- remove Nothing occurences
      let resultE = leftmost [silencedE, switchPromptlyDyn lastEDyn]
    return resultE

instance AlignWithFps Dynamic where
  alignWithFps fps da = do
    a <- sample . current $ da
    ea <- alignWithFps fps $ updated da
    holdDyn a ea

  limitRate fps da = do
    a <- sample . current $ da
    ea <- limitRate fps $ updated da
    holdDyn a ea

-- | Same as 'tickEvery' but stops after n ticks.
tickEveryN :: (TimerMonad t m, MonadAppHost t m)
  => NominalDiffTime -- ^ Tick interval
  -> Int -- ^ How many ticks to do
  -> Event t a -- ^ Additional stop event
  -> m (Event t Int) -- ^ Event that fires at tick and cary tick number
tickEveryN dt n userStopE
  | n <= 0    = return never
  | otherwise = do
    ref <- newExternalRef 0
    let stopE = fforMaybe (externalEvent ref) $ \i -> if i >= n-1 then Just () else Nothing
    e <- tickEveryUntil dt $ leftmost [stopE, const () <$> userStopE]
    performEvent $ ffor e $ const $ modifyExternalRef ref $ \i -> (i+1, i+1)

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
    _ <- ticker fireTick
    return tickEvent
    where
    ticker fire = liftIO $ do
      tm <- getSystemTimerManager
      let dt = ceiling $ (realToFrac t :: Double) * 1000000
          go = do
            _ <- fire ()
            void $ registerTimeout tm dt go
      registerTimeout tm dt go
  {-# INLINE tickEvery #-}

  tickOnce t = do
    (tickEvent, fireTick) <- newExternalEvent
    tm <- liftIO getSystemTimerManager
    let dt = ceiling $ (realToFrac t :: Double) * 1000000
    _ <- liftIO $ registerTimeout tm dt $ void $ fireTick ()
    return tickEvent
  {-# INLINE tickOnce #-}

  tickEveryUntil t ceaseE = do
    (tickEvent, fireTick) <- newExternalEvent
    stopRef <- liftIO $ newIORef False
    performEvent_ $ ffor ceaseE $ const $ liftIO $ writeIORef stopRef True
    _ <- ticker fireTick stopRef
    return tickEvent
    where
    ticker fire stopRef = liftIO $ do
      tm <- getSystemTimerManager
      let dt = ceiling $ (realToFrac t :: Double) * 1000000
          go = do
            _<- fire ()
            stop <- liftIO $ readIORef stopRef
            unless stop $ void $ registerTimeout tm dt go
      registerTimeout tm dt go
  {-# INLINE tickEveryUntil #-}

  delayBy t e = performEventAsync $ ffor e $ \a -> do
    TD.delay (ceiling $ (realToFrac t :: Rational) * 1000000)
    return a
  {-# INLINE delayBy #-}

-- | Automatic lifting across monad stack
instance {-# OVERLAPPABLE #-} (MonadFix (mt m), TimerMonad t m, MonadTrans mt) => TimerMonad t (mt m) where
  tickEvery = lift . tickEvery
  tickOnce = lift . tickOnce
  tickEveryUntil a b = lift $ tickEveryUntil a b
  delayBy a b = lift $ delayBy a b
  {-# INLINE tickEvery #-}
  {-# INLINE tickOnce #-}
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