module Game.Player(
    Player(..)
  , playerWire
  ) where

import Control.DeepSeq
import Control.Wire
import Data.Text
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.Core
import Game.GoreAndAsh.GLFW 
import Game.GoreAndAsh.Logging

data Player = Player {
  playerPos :: !(V2 Float)
, playerColor :: !(V3 Float) 
, playerRot :: !Float  
} deriving (Generic)

instance NFData Player 

playerWire :: Player -> AppWire a Player 
playerWire initialPlayer = loop $ proc (_, p_) -> do 
  p <- delay initialPlayer -< p_ 
  -- traceEvent (pack . show) . keyPressed Key'W -< ()
  traceEvent (pack . show) . mouseButtonPressed MouseButton'1 -< ()
  -- traceEvent (pack . show) . mousePositionChange -< ()
  traceEvent (pack . show) . windowSize -< ()
  traceEvent (pack . show) . mouseScroll -< ()
  forceNF -< (p, p)