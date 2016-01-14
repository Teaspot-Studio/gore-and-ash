module Game.Player(
    Player(..)
  , PlayerId(..)
  , PlayerMessage(..)
  , playerActor
  ) where

import Control.DeepSeq
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Typeable 
import Data.Word 
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.Core
import Game.GoreAndAsh
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.SDL 
import Game.GoreAndAsh.Sync

import Consts
import Game.Camera 
import Game.Player.Shared

--import SDL 
import Linear.Affine
import Foreign.C.Types

data Player = Player {
  playerId :: !PlayerId
, playerPos :: !(V2 Double)
, playerColor :: !(V3 Double) 
, playerRot :: !Double
, playerSpeed :: !Double
, playerPeer :: !Peer 
} deriving (Generic, Show)

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
  
playerActor :: PlayerId -> Peer -> AppActor PlayerId Camera Player 
playerActor i peer = actorMaker $ proc (c, p) -> do 
  peerSendIndexed peer (ChannelID 0) i ReliableMessage . now -< NetMsgPlayerRequest
  liftGameMonad2 renderPlayer -< (p, c)
  forceNF . controlPlayer i -< p
  where
    actorMaker = netStateActorFixed i initialPlayer process 
      (playerPeer initialPlayer) 1 netProcess

    initialPlayer = Player {
        playerId = i 
      , playerPos = 0
      , playerColor = V3 1 0 0
      , playerRot = 0
      , playerSpeed = 0.5
      , playerPeer = peer
      }

    process :: Player -> PlayerMessage -> Player 
    process p _ = p

    netProcess :: ChannelID -> Player -> PlayerNetMessage -> Player 
    netProcess _ p msg = case msg of 
      NetMsgPlayerPos x y -> p { playerPos = V2 x y }
      NetMsgPlayerRot r -> p { playerRot = r }
      NetMsgPlayerColor r g b -> p { playerColor = V3 r g b }
      NetMsgPlayerSpeed v -> p { playerSpeed = v }
      NetMsgPlayerRequest -> p 

    controlPlayer :: PlayerId -> AppWire Player Player
    controlPlayer pid = 
        movePlayer pid (V2 1 0) ScancodeLeft 
      . movePlayer pid (V2 (-1) 0) ScancodeRight
      . movePlayer pid (V2 0 1) ScancodeDown
      . movePlayer pid (V2 0 (-1)) ScancodeUp

    movePlayer :: PlayerId -> V2 Double -> Scancode -> AppWire Player Player
    movePlayer pid dv k = proc p -> do 
      e <- keyPress k -< ()
      let newPlayer = p {
            playerPos = playerPos p + dv * V2 (playerSpeed p) (playerSpeed p)
          }
          posMsg = let V2 x y = playerPos newPlayer in NetMsgPlayerPos x y
      peerSendIndexed peer (ChannelID 0) pid UnreliableMessage -< const posMsg <$> e
      returnA -< event p (const newPlayer) e

-- | Function of rendering player
renderPlayer :: MonadSDL m => Player -> Camera -> GameMonadT m ()
renderPlayer Player{..} Camera{..} = do  
  mwr <- sdlGetWindowM mainWindowName
  case mwr of 
    Nothing -> return ()
    Just (_, r) -> do 
      rendererDrawColor r $= transColor playerColor 
      fillRect r $ Just square
  where
    transColor :: V3 Double -> V4 Word8
    transColor (V3 r g b) = V4 (round $ r * 255) (round $ g * 255) (round $ b * 255) 255

    square :: Rectangle CInt
    square = Rectangle (P $ V2 100 100) (V2 100 100)