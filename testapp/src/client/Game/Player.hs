module Game.Player(
    Player(..)
  , PlayerId(..)
  , PlayerMessage(..)
  , playerWire
  ) where

import Control.DeepSeq
import Control.Wire
import Data.Text
import Data.Typeable 
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.Core
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.GLFW 
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network

data Player = Player {
  playerId :: !PlayerId
, playerPos :: !(V2 Float)
, playerColor :: !(V3 Float) 
, playerRot :: !Float
, playerPeer :: !Peer 
} deriving (Generic)

instance NFData Player 

newtype PlayerId = PlayerId { unPlayerId :: Int } deriving (Eq, Show, Generic)
instance NFData PlayerId 

data PlayerMessage = PlayerMessageStub deriving (Typeable, Generic)
instance NFData PlayerMessage 

instance ActorMessage PlayerId where
  type ActorMessageType PlayerId = PlayerMessage
  toCounter = unPlayerId
  fromCounter = PlayerId 

playerWire :: (PlayerId -> Player) -> AppActor PlayerId a Player 
playerWire initialPlayer = stateActor initialPlayer process $ \_ -> proc (_, p) -> do 
  -- traceEvent (pack . show) . keyPressed Key'W -< ()
  traceEvent (pack . show) . mouseButtonPressed MouseButton'1 -< ()
  -- traceEvent (pack . show) . mousePositionChange -< ()
  traceEvent (pack . show) . windowSize -< ()
  traceEvent (pack . show) . mouseScroll -< ()
  forceNF -< p
  where
    process :: PlayerId -> Player -> PlayerMessage -> Player 
    process _ p _ = p