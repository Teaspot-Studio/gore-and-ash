module Game.Player(
    Player(..)
  , PlayerId(..)
  , PlayerMessage(..)
  , playerWire
  ) where

import Control.DeepSeq
import Control.Wire
import Control.Wire.Unsafe.Event
--import Data.Text
import Data.Typeable 
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.Core
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.GLFW 
--import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync

import Game.Player.Shared

data Player = Player {
  playerId :: !PlayerId
, playerPos :: !(V2 Double)
, playerColor :: !(V3 Double) 
, playerRot :: !Double
, playerSpeed :: !Double
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

instance NetworkMessage PlayerId where 
  type NetworkMessageType PlayerId = PlayerNetMessage
  
playerWire :: (PlayerId -> Player) -> AppActor PlayerId a Player 
playerWire initialPlayer = actorMaker $ \i -> proc (_, p) -> do 
  -- traceEvent (pack . show) . keyPressed Key'W -< ()
  --traceEvent (pack . show) . mouseButtonPressed MouseButton'1 -< ()
  -- traceEvent (pack . show) . mousePositionChange -< ()
  --traceEvent (pack . show) . windowSize -< ()
  --traceEvent (pack . show) . mouseScroll -< ()

  forceNF . controlPlayer i (playerPeer $ initialPlayer i) -< p
  where
    actorMaker = netStateActor initialPlayer process 
      playerPeer 1 netProcess

    process :: PlayerId -> Player -> PlayerMessage -> Player 
    process _ p _ = p

    netProcess :: PlayerId -> ChannelID -> Player -> PlayerNetMessage -> Player 
    netProcess _ _ p msg = case msg of 
      NetMsgPlayerPos x y -> p { playerPos = V2 x y }
      NetMsgPlayerRot r -> p { playerRot = r }
      NetMsgPlayerColor r g b -> p { playerColor = V3 r g b }
      NetMsgPlayerSpeed v -> p { playerSpeed = v }
 
    controlPlayer :: PlayerId -> Peer -> AppWire Player Player
    controlPlayer pid peer = 
        movePlayer pid peer (V2 1 0) Key'Left 
      . movePlayer pid peer (V2 (-1) 0) Key'Right
      . movePlayer pid peer (V2 0 1) Key'Down
      . movePlayer pid peer (V2 0 (-1)) Key'Up

    movePlayer :: PlayerId -> Peer -> V2 Double -> Key -> AppWire Player Player
    movePlayer pid peer dv k = proc p -> do 
      e <- keyPressing k -< ()
      let newPlayer = p {
            playerPos = playerPos p + dv * V2 (playerSpeed p) (playerSpeed p)
          }
          posMsg = let V2 x y = playerPos newPlayer in NetMsgPlayerPos x y
      peerSendIndexed peer (ChannelID 0) pid UnreliableMessage -< const posMsg <$> e
      returnA -< event p (const newPlayer) e