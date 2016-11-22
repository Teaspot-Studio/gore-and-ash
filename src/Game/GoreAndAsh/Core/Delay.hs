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
  ) where

import Control.Monad.IO.Class
import Data.IORef
import Game.GoreAndAsh.Core.Monad

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