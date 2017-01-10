{-|
Module      : Game.GoreAndAsh.Core.Delay
Description : Helpers for delaying events and dynamics.
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Core.Delay(
    Delay(..)
  , lookPast
  , linearInterpolate
  , simpleInterpolate
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Time
import Game.GoreAndAsh.Core.Monad
import Game.GoreAndAsh.Time -- TODO: move this to core

-- | Defines operation of delaying reactive primitivies.
class Delay r where
  -- | Delay event/dynamic first occurence and return fixed value instead
  delay :: MonadAppHost t m => a -> r t a -> m (r t a)

-- | Delay first occurence of event by given value
instance Delay Event where
  delay a e = do
    ref <- liftIO $ newIORef a
    (e', fire) <- newExternalEvent
    performEvent_ $ ffor e $ \v -> liftIO $ do
      oldV <- atomicModifyIORef' ref $ \oldV -> (v, oldV)
      _ <- fire oldV
      return ()
    return e'

-- | Delay first occurence of dynamic by given value
instance Delay Dynamic where
  delay a d = do
    e' <- delay a (updated d)
    holdDyn a e'
  {-# INLINE delay #-}

-- | Saves value of dynamic each interval and fire them after the end of interval
--
-- Note that the function doesn't replay all changes, only certain points in past
-- that are saved at start of each interval.
lookPast :: (TimerMonad t m, MonadAppHost t m)
  => NominalDiffTime -- ^ Save each n seconds
  -> a -- ^ Initial value (before first delayed value)
  -> Event t a -- ^ When fires, the stored past value is replaced by payload.
  -> Dynamic t a -- ^ Original dynamic
  -> m (Dynamic t a) -- ^ The delayed values
lookPast dt v0 ea da = do
  tickE <- tickEvery dt
  ref <- liftIO $ newIORef v0
  (e', fire) <- newExternalEvent
  performEvent_ $ ffor ea $ liftIO . atomicWriteIORef ref
  performEvent_ $ ffor tickE $ const $ do
    v <- sample . current $ da
    liftIO $ do
      oldV <- atomicModifyIORef' ref $ \oldV -> (v, oldV)
      _ <- fire oldV
      return ()
  holdDyn v0 e'

-- | Make linear interpolation of dynamic value. Each time the value updates, start
-- transition between old value and new value.
linearInterpolate :: forall t m a . (MonadAppHost t m, TimerMonad t m, Fractional a)
  => Int -- ^ Number of intermediate values
  -> NominalDiffTime -- ^ Time interval in which all interpolation have to be
  -> Dynamic t a -- ^ Value that updates should be interpolated
  -> m (Dynamic t a) -- ^ Interpolated result, that is delayed by interval of interpolation
linearInterpolate n dt da = do
  v0 <- sample . current $ da
  oldDa <- delay v0 da
  let
    stepper :: Event t (m (Dynamic t a))
    stepper = step oldDa (const () <$> updated da) <$> updated da
  join <$> holdAppHost (return da) stepper
  where
    step :: Dynamic t a -> Event t () -> a -> m (Dynamic t a)
    step oldDa stopE v1 = do
      v0 <- sample . current $ oldDa
      simpleInterpolate n dt v0 v1 stopE

-- | Perform simple linear interpolation betwen start and end positions.
simpleInterpolate :: forall t m a . (MonadAppHost t m, TimerMonad t m, Fractional a)
  => Int -- ^ Number of intermediate values
  -> NominalDiffTime -- ^ Time interval in which all interpolation have to be
  -> a -- ^ Start value
  -> a -- ^ End value
  -> Event t () -- ^ Optional stop event
  -> m (Dynamic t a) -- ^ Interpolated result
simpleInterpolate n dt v0 v1 stopE = do
  let dt' = realToFrac $ (realToFrac dt :: Double) / fromIntegral n
      dv  = (v1 - v0) / fromIntegral n
  tickE <- tickEveryN dt' n stopE
  let calc i v = if i >= n-1 then v1 else v + dv
  foldDyn calc v0 tickE