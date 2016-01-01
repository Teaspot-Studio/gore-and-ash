module Game.GoreAndAsh.Logging.API(
    LoggingMonad(..)
  -- | Arrow API
  , logA
  , logALn
  , logE
  , logELn

  -- | Every frame
  , logInfoA
  , logWarnA
  , logErrorA
  -- | Event based
  , logInfoE
  , logWarnE
  , logErrorE

  -- | Event tracing
  , traceEvent
  , traceEventShow
  ) where

import Control.Monad.State.Strict
import Control.Wire
import Data.Text
import Prelude hiding (id, (.))
import qualified Data.Sequence as S 
import TextShow 

import Game.GoreAndAsh
import Game.GoreAndAsh.Logging.State
import Game.GoreAndAsh.Logging.Module

-- | Low level API for module
class Monad m => LoggingMonad m where 
  putMsgM :: Text -> m ()
  putMsgLnM :: Text -> m ()

instance {-# OVERLAPPING #-} Monad m => LoggingMonad (LoggingT s m) where
  putMsgM t = do 
    cntx <- get 
    let newMsgs = case S.viewr $ loggingMsgs cntx of 
          S.EmptyR -> loggingMsgs cntx S.|> t
          (s' S.:> t') -> s' S.|> (t' <> t)
    put $ cntx { loggingMsgs = newMsgs }

  putMsgLnM t = do 
    cntx <- get 
    put $ cntx { loggingMsgs = loggingMsgs cntx S.|> t }

instance {-# OVERLAPPABLE #-} (Monad (mt m), LoggingMonad m, MonadTrans mt) => LoggingMonad (mt m) where 
  putMsgM = lift . putMsgM
  putMsgLnM = lift . putMsgLnM

-- | Put message to console on every frame without newline
logA :: LoggingMonad m => GameWire m Text ()
logA = liftGameMonad1 putMsgM

-- | Put message to console on every frame
logALn :: LoggingMonad m => GameWire m Text ()
logALn = liftGameMonad1 putMsgLnM

-- | Put message to console on event without newline
logE :: LoggingMonad m => GameWire m (Event Text) (Event ())
logE = liftGameMonadEvent1 putMsgM

-- | Put message to console on event
logELn :: LoggingMonad m => GameWire m (Event Text) (Event ())
logELn = liftGameMonadEvent1 putMsgLnM

-- | Put info msg to console
logInfoA :: LoggingMonad m => GameWire m Text ()
logInfoA = logALn . arr ("Info: " <>)

-- | Put warn msg to console
logWarnA :: LoggingMonad m => GameWire m Text ()
logWarnA = logALn . arr ("Info: " <>)

-- | Put error msg to console
logErrorA :: LoggingMonad m => GameWire m Text ()
logErrorA = logALn . arr ("Info: " <>)

-- | Put info msg to console on event
logInfoE :: LoggingMonad m => GameWire m (Event Text) (Event ())
logInfoE = logELn . mapE ("Info: " <>)

-- | Put warn msg to console on event
logWarnE :: LoggingMonad m => GameWire m (Event Text) (Event ())
logWarnE = logELn . mapE ("Info: " <>)

-- | Put error msg to console on event
logErrorE :: LoggingMonad m => GameWire m (Event Text) (Event ())
logErrorE = logELn . mapE ("Info: " <>)

-- | Prints event with given function
traceEvent :: LoggingMonad m => (a -> Text) -> GameWire m (Event a) (Event ())
traceEvent f = logELn . mapE f

-- | Prints event 
traceEventShow :: (TextShow a, LoggingMonad m) => GameWire m (Event a) (Event ())
traceEventShow = traceEvent showt