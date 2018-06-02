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
{-# LANGUAGE StandaloneDeriving #-}
module Game.GoreAndAsh.Time(
    tickEvery
  , tickOnce
  , tickEveryUntil
  , delayBy
  , AlignWithFps(..)
  , tickEveryN
  ) where

import Control.Concurrent
import Control.Concurrent.Thread.Delay as TD
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Data.Functor (void)
import Data.IORef
import Data.Proxy
import Data.Time
import Game.GoreAndAsh.Core.ExternalRef
import Game.GoreAndAsh.Core.Monad
import GHC.Event hiding (Event)

-- | Get event that fires every n seconds.
--
-- Note: the event will tick even there are no subscriders to it. Use
-- 'tickEveryUntil' if you want to free underlying thread.
tickEvery :: (TriggerEvent t m, MonadIO m) => NominalDiffTime -> m (Event t ())
tickEvery t = do
  (tickEvent, fireTick) <- newTriggerEvent
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

-- | Generate an event after given time once
tickOnce :: (TriggerEvent t m, MonadIO m) => NominalDiffTime -> m (Event t ())
tickOnce t = do
  (tickEvent, fireTick) <- newTriggerEvent
  tm <- liftIO getSystemTimerManager
  let dt = ceiling $ (realToFrac t :: Double) * 1000000
  _ <- liftIO $ registerTimeout tm dt $ void $ fireTick ()
  return tickEvent

-- | Get event that fires every n seconds. Second event cease ticking.
tickEveryUntil :: (TriggerEvent t m, PerformEvent t m, MonadIO m, MonadIO (Performable m)) => NominalDiffTime -> Event t a -> m (Event t ())
tickEveryUntil t ceaseE = do
  (tickEvent, fireTick) <- newTriggerEvent
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

-- | Get event that fires only after given event with specified delay.
--
-- Note: can wait for greater intervals than original reflex 'delay'
delayBy :: (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m)) => NominalDiffTime -> Event t a -> m (Event t a)
delayBy t e = performEventAsync $ ffor e $ \a ret -> void . liftIO . forkIO $ do
  TD.delay (ceiling $ (realToFrac t :: Rational) * 1000000)
  ret a
{-# INLINE delayBy #-}

-- | Holds shared functions between Event and Dynamic to align occurences with time
class AlignWithFps r where
  -- | Fire event not frequently as given frame per second ratio. Starts global
  -- counter, so first occurence of resulted event can be delayed by 1/fps seconds.
  -- The resulted event fires with last occurence of original event during 1/fps
  -- interval.
  alignWithFps :: MonadGame t m
    => Int -- ^ FPS (frames per second)
    -> r t a -- ^ Event/Dynamic that frequently changes
    -> m (r t a) -- ^ Event/Dynamic that changes are aligned with FPS

  -- | Fire event not frequently as given frame per second ratio. Doesn't delay
  -- first occurence, only remembers last event occurence after 1/fps time
  -- interval, it is refired at end of the interval.
  limitRate :: MonadGame t m
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
      lastEDyn <- networkHold (pure never) $ ffor resultE $ const $ do
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
  {-# INLINE alignWithFps #-}

  limitRate fps da = do
    a <- sample . current $ da
    ea <- limitRate fps $ updated da
    holdDyn a ea
  {-# INLINE limitRate #-}

-- | Same as 'tickEvery' but stops after n ticks.
tickEveryN :: (MonadHold t m, TriggerEvent t m, PerformEvent t m, MonadIO m, MonadIO (Performable m))
  => NominalDiffTime -- ^ Tick interval
  -> Int -- ^ How many ticks to do
  -> Event t a -- ^ Additional stop event
  -> m (Event t Int) -- ^ Event that fires at tick and cary tick number
tickEveryN dt n userStopE
  | n <= 0    = return never
  | otherwise = do
    ref <- newExternalRef 0
    let stopE = fforMaybe (externalEvent ref) $ \i -> if i >= n-1 then Just () else Nothing
    e <- tickEveryUntil dt $ leftmost [stopE, void userStopE]
    performEvent $ ffor e $ const $ modifyExternalRef ref $ \i -> (i+1, i+1)
