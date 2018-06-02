module Logger.API(
    LoggerMonad(..)
  , LoggerEnv(..)
  , LoggerT
  , runLoggerT
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.Proxy
import Game.GoreAndAsh

-- | API of logging module that is used by game logic code
class (Reflex t, Monad m) => LoggerMonad t m | m -> t where
  -- | Get event that fires when user enters a message
  inputMessage :: m (Event t String)
  -- | Write to console when the event fires
  outputMessage :: Event t String -> m ()

-- | Input events of logger module
data LoggerEnv t = LoggerEnv {
  loggerInput :: Event t String
}

-- | Shortcut for implementation of 'LoggerMonad'
type LoggerT t = ReaderT (LoggerEnv t)

-- | Execution of logger layer in monad stack
runLoggerT :: MonadGame t m => LoggerT t m a -> m a
runLoggerT m = do
  (inputEvent, inputFire) <- newTriggerEvent
  _ <- liftIO . forkIO . forever $ getLine >>= inputFire
  runReaderT m $ LoggerEnv inputEvent

-- | Implement our logger API
instance {-# OVERLAPPING #-} (MonadIO m, MonadGame t m) => LoggerMonad t (ReaderT (LoggerEnv t) m) where
  inputMessage = asks loggerInput
  outputMessage e = void . performEvent $ ffor e $ liftIO . putStrLn
  {-# INLINE inputMessage #-}
  {-# INLINE outputMessage #-}

-- | Automatic lifting across monad stack
instance {-# OVERLAPPABLE #-} (MonadIO (mt m), LoggerMonad t m, MonadTrans mt) => LoggerMonad t (mt m) where
  inputMessage = lift inputMessage
  outputMessage = lift . outputMessage
  {-# INLINE inputMessage #-}
  {-# INLINE outputMessage #-}
