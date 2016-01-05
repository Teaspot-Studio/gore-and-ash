module Game.Data(
    Game(..)
  , PlayerMap
  , PlayerPeerMap
  ) where

import Control.DeepSeq
import GHC.Generics (Generic)
import Prelude hiding (id, (.))
import qualified Data.HashMap.Strict as H 

import Game.Player.Data
import Game.Shared

import Game.GoreAndAsh.Network

type PlayerMap = H.HashMap PlayerId Player
type PlayerPeerMap = H.HashMap Peer PlayerId 

data Game = Game {
  gameId :: !GameId
, gamePlayers :: !PlayerMap
, gamePlayerPeers :: !PlayerPeerMap
} deriving (Generic)

instance NFData Game 