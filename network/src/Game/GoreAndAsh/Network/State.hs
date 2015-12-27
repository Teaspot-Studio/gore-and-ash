module Game.GoreAndAsh.Network.State(
    NetworkState(..)
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq 

-- | Inner state of network layer
data NetworkState s = NetworkState {
  networkNextState :: !s
} deriving (Generic)

instance NFData s => NFData (NetworkState s)