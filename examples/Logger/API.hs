module Logger.API(
    LoggerMonad(..)
  , LoggerT
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

-- | Implementation basis of logger API.
newtype LoggerT t m a = LoggerT { runLoggerT :: ReaderT (LoggerEnv t) m a}
  deriving (Functor, Applicative, Monad, MonadReader (LoggerEnv t), MonadIO, MonadFix)

-- | Execute actions in 'LoggerT'
evalLoggerT :: LoggerT t m a -> LoggerEnv t -> m  a
evalLoggerT m i = runReaderT (runLoggerT m) i

-- | Implement our logger API
instance {-# OVERLAPPING #-} (MonadIO m, MonadAppHost t m) => LoggerMonad t (LoggerT t m) where
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

-- | The instance registers external events and process reaction to output events
instance (MonadIO (HostFrame t), GameModule t m) => GameModule t (LoggerT t m) where
  type ModuleOptions t (LoggerT t m) = ModuleOptions t m

  runModule opts m = do
    (inputEvent, inputFire) <- newExternalEvent
    _ <- liftIO . forkIO . forever $ getLine >>= inputFire
    let env = LoggerEnv inputEvent
    runModule opts $ evalLoggerT m env

  withModule t _ = withModule t (Proxy :: Proxy m)

  {-# INLINE runModule #-}
  {-# INLINE withModule #-}

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

instance MonadTrans (LoggerT t) where
  lift = LoggerT . lift

instance MonadSample t m => MonadSample t (LoggerT t m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (LoggerT t m) where
  hold            a b = lift $ hold a b
  holdDyn         a b = lift $ holdDyn a b
  holdIncremental a b = lift $ holdIncremental a b

instance MonadAppHost t m => MonadAppHost t (LoggerT t m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- lift getRunAppHost
    r <- ask
    return $ \m -> runner $ evalLoggerT m r
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (LoggerT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger a = lift $ newFanEventWithTrigger a

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (LoggerT r m) where
  subscribeEvent = lift . subscribeEvent
