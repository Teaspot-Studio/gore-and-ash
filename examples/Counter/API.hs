module Counter.API(
    CounterMonad(..)
  , CounterT
  , runCounterT
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Data.Proxy
import Game.GoreAndAsh

-- | API of logging module that is used by game logic code
class (Reflex t, Monad m) => CounterMonad t m | m -> t where
  -- | Increment internal counter by 1 every tick
  incCounter :: Event t a -> m ()
  -- | Get value of counter
  getCounter :: m (Dynamic t Int)

-- | Shortcut for implementation of 'CounterMonad'
type CounterT t = ReaderT (ExternalRef t Int)

-- | Execute counter layer
runCounterT :: MonadGame t m => CounterT t m a -> m a
runCounterT m = do
  ref <- newExternalRef 0
  runReaderT m ref

-- | Implement our Counter API
instance {-# OVERLAPPING #-} (Monad m, Reflex t, MonadGame t m) => CounterMonad t (ReaderT (ExternalRef t Int) m) where
  incCounter e = do
    ref <- ask
    performEvent_ $ ffor e $ const $ modifyExternalRef ref (\a -> (a+1, ()))
  getCounter = do
    ref <- ask
    externalRefDynamic ref
  {-# INLINE incCounter #-}
  {-# INLINE getCounter #-}

-- | Automatic lifting across monad stack
instance {-# OVERLAPPABLE #-} (Monad (mt m), CounterMonad t m, MonadTrans mt) => CounterMonad t (mt m) where
  incCounter = lift . incCounter
  getCounter = lift getCounter
  {-# INLINE incCounter #-}
  {-# INLINE getCounter #-}
