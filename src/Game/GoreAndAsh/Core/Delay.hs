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
  ) where

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