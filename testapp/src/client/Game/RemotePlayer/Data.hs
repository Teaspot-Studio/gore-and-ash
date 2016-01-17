module Game.RemotePlayer.Data(
    RemotePlayer(..)
  ) where

import GHC.Generics
import Control.DeepSeq
import Linear

import Game.Player.Data 

data RemotePlayer = RemotePlayer {
  remotePlayerId :: !PlayerId 
, remotePlayerPos :: !(V2 Double)
, remotePlayerRot :: !Double 
, remotePlayerCol :: !(V3 Double)
, remotePlayerSize :: !Double
, remotePlayerSpeed :: !Double
} deriving Generic

instance NFData RemotePlayer
