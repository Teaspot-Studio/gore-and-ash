module Logger.API(
    LoggerMonad(..)
  , LoggerT
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.RSS.Strict
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Proxy
import Game.GoreAndAsh

-- | API of logging module that is used by game logic code
class (Reflex t, Monad m) => LoggerMonad t m | m -> t where
  -- | Get event that fires when user enters a message
  inputMessage :: m (Event t String)
  -- | Write to console when the event fires
  outputMessage :: Event t String -> m ()

-- | Input events of logger module
type LoggerInput t = Event t String
-- | Output events of logger module
newtype LoggerOutput t = LoggerOutput (Event t String)

-- | When two output events are coincinence, simpli merge them in one message
instance Reflex t => Monoid (LoggerOutput t) where
  mempty = LoggerOutput never
  (LoggerOutput a) `mappend` (LoggerOutput b) = LoggerOutput $ mergeWith (++) [a, b]

-- | Implementation basis of logger API. Note: use of writer on state to avoid
-- broken writer in mtl.
newtype LoggerT t m a = LoggerT { runLoggerT :: RSST (LoggerInput t) (LoggerOutput t) () m a}
  deriving (Functor, Applicative, Monad, MonadReader (LoggerInput t), MonadWriter (LoggerOutput t)
    , MonadIO, MonadFix)

-- | Execute actions in 'LoggerT'
evalLoggerT :: (Monad m, Reflex t) => LoggerT t m a -> LoggerInput t -> m (a, LoggerOutput t)
evalLoggerT m i = fmap (\(a, _, o) -> (a, o)) $ runRSST (runLoggerT m) i ()

-- | Implement our logger API
instance {-# OVERLAPPING #-} (Monad m, Reflex t) => LoggerMonad t (LoggerT t m) where
  inputMessage = ask
  outputMessage e = tell $ LoggerOutput e
  {-# INLINE inputMessage #-}
  {-# INLINE outputMessage #-}

-- | The instance registers external events and process reaction to output events
instance (MonadIO (HostFrame t), GameModule t m) => GameModule t (LoggerT t m) where
  runModule m = do
    (inputEvent, inputFire) <- newExternalEvent
    _ <- liftIO . forkIO . forever $ getLine >>= inputFire
    (a, LoggerOutput outputEvent) <- runModule $ evalLoggerT m inputEvent
    performEvent_ $ fmap (liftIO . putStrLn) outputEvent
    return a

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
    return $ \m -> runner $ fmap fst $ evalLoggerT m r
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (LoggerT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger a = lift $ newFanEventWithTrigger a

-- | Automatic lifting across monad stack
instance {-# OVERLAPPABLE #-} (Monad (mt m), LoggerMonad t m, MonadTrans mt) => LoggerMonad t (mt m) where
  inputMessage = lift inputMessage
  outputMessage = lift . outputMessage
  {-# INLINE inputMessage #-}
  {-# INLINE outputMessage #-}
