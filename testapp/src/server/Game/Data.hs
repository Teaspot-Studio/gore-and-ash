module Game.Data(
    Game(..)
  , PlayerMap
  , PlayerPeerMap
  , BulletsMap
  ) where

import Control.DeepSeq
import GHC.Generics (Generic)
import Prelude hiding (id, (.))
import qualified Data.HashMap.Strict as H 

import Game.Bullet.Data
import Game.Player.Data
import Game.Shared

import Game.GoreAndAsh.Network

type PlayerMap = H.HashMap PlayerId Player
type PlayerPeerMap = H.HashMap Peer PlayerId 
type BulletsMap = H.HashMap BulletId Bullet 

data Game = Game {
  gameId :: !GameId
, gamePlayers :: !PlayerMap
, gamePlayerPeers :: !PlayerPeerMap
, gameBullets :: !BulletsMap
} deriving (Generic)

instance NFData Game 