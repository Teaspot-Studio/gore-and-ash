{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Logging.Module(
    LoggingT(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Fix 
import Control.Monad.State.Strict
import qualified Data.Sequence as S
import qualified Data.Text.IO as T

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging.State

newtype LoggingT s m a = LoggingT { runLoggingT :: StateT (LoggingState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (LoggingState s), MonadFix, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance GameModule m s => GameModule (LoggingT s m) (LoggingState s) where 
  type ModuleState (LoggingT s m) = LoggingState s
  runModule (LoggingT m) s = do
    ((a, s'), nextState) <- runModule (runStateT m s) (loggingNextState s)
    printAllMsgs s'
    return (a, s' { 
        loggingMsgs = S.empty
      , loggingNextState = nextState 
      })
    where 
      printAllMsgs LoggingState{..} = liftIO $ mapM_ T.putStrLn loggingMsgs      

  newModuleState = do
    s <- newModuleState
    return $ LoggingState {
        loggingMsgs = S.empty
      , loggingNextState = s
      }

  withModule _ = id
  cleanupModule _ = return ()