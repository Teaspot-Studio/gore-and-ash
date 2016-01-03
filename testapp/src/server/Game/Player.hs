module Game.Player(
    Player(..)
  , PlayerId(..)
  , PlayerMessage
  , playerActor
  ) where

import Control.DeepSeq
import Control.Wire
import Data.Hashable
import Data.Typeable 
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.Core
import Game.GoreAndAsh
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Network 

import qualified Data.ByteString as BS

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
instance Hashable PlayerId 

data PlayerMessage = PlayerMessageStub deriving (Typeable, Generic)
instance NFData PlayerMessage 

instance ActorMessage PlayerId where
  type ActorMessageType PlayerId = PlayerMessage
  toCounter = unPlayerId
  fromCounter = PlayerId 

playerActor :: (PlayerId -> Player) -> AppActor PlayerId a Player 
playerActor initialPlayer = stateActor initialPlayer process $ \_ -> proc (_, p) -> do 
  peers <- peersConnected -< ()
  rSwitch (pure ()) -< ((), peersWire <$> peers)

  forceNF -< p
  where
    process :: PlayerId -> Player -> PlayerMessage -> Player 
    process _ p _ = p 

    mkMessage _ = Message ReliableMessage BS.empty

    peersWire peers = proc _ -> do 
      sequenceA ((\p -> peerSend p (ChannelID 0)) <$> peers) . mapE mkMessage . now -< ()
      returnA -< ()