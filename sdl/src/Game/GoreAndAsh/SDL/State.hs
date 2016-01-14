module Game.GoreAndAsh.SDL.State(
    SDLState(..)
  , emptySDLState
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq 

-- | Inner state of logger
data SDLState s = SDLState {
  sdlNextState :: !s
} deriving (Generic)

instance NFData s => NFData (SDLState s)

-- | Creates empty module state
emptySDLState :: s -> SDLState s 
emptySDLState s = SDLState {
    sdlNextState = s
  }