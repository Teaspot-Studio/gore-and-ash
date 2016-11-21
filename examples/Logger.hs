module Main where

import Game.GoreAndAsh
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy

class (Reflex t, Monad m) => LoggerMonad t m | m -> t where
  inputMessage :: m (Event t String)
  outputMessage :: Event t String -> m ()

newtype LoggerT t m a = LoggerT { runLoggerT :: Event t String -> m (a, Event t String) }
  deriving (Functor)

instance (Monad m, Reflex t) => Applicative (LoggerT t m) where
  pure a = LoggerT $ const $ pure (a, never)
  mf <*> ma = LoggerT $ \e -> do
    (f, fe) <- runLoggerT mf e
    (a, ae) <- runLoggerT ma e
    return (f a, mergeWith (++) [fe, ae])

instance (Monad m, Reflex t) => Monad (LoggerT t m) where
  return = pure
  ma >>= f = LoggerT $ \e -> do
    (a, ae) <- runLoggerT ma e
    (b, be) <- runLoggerT (f a) e
    return (b, mergeWith (++) [ae, be])

instance (Monad m, Reflex t) => LoggerMonad t (LoggerT t m) where
  inputMessage = LoggerT $ \e -> return (e, never)
  outputMessage e = LoggerT $ const $ return ((), e)

instance (MonadIO (HostFrame t), GameModule t m) => GameModule t (LoggerT t m) where
  runModule m = do
    (inputEvent, inputFire) <- newExternalEvent
    _ <- liftIO . forkIO . forever $ getLine >>= inputFire
    (a, outputEvent) <- runModule $ runLoggerT m inputEvent
    performEvent_ $ fmap (liftIO . putStrLn) outputEvent
    return a

  withModule t _ = withModule t (Proxy :: Proxy m)

  {-# INLINE runModule #-}
  {-# INLINE withModule #-}

-- | Application monad that is used for implementation of game API
type AppMonad = LoggerT Spider (GameMonad Spider)

-- The application should be generic in the host monad that is used
app :: LoggerMonad t m => m ()
app = do
  msgE <- inputMessage
  outputMessage $ fmap (\msg -> "You said: " ++ msg) msgE

main :: IO ()
main = runSpiderHost $ hostApp $ runModule (app :: AppMonad ())