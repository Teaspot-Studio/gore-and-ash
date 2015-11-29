module Game.GoreAndAsh.Logging.State(
    LoggingState(..)
  ) where

import qualified Data.Sequence as S
import Data.Text 
import GHC.Generics (Generic)
import Control.DeepSeq 

-- | Inner state of logger
data LoggingState s = LoggingState {
  loggingMsgs :: !(S.Seq Text)
, loggingNextState :: !s
} deriving (Generic)

instance NFData s => NFData (LoggingState s)