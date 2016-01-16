module Game.Player.Shared(
    PlayerNetMessage(..)
  ) where

import Control.DeepSeq
import Data.Serialize 
import GHC.Generics (Generic)
import Linear 

data PlayerNetMessage = 
    NetMsgPlayerPos !Double !Double
  | NetMsgPlayerRot !Double
  | NetMsgPlayerColor !Double !Double !Double
  | NetMsgPlayerSpeed !Double
  | NetMsgPlayerSize !Double
  | NetMsgPlayerRequest
  | NetMsgPlayerFire !(V2 Double)
  deriving (Generic, Show)

instance NFData PlayerNetMessage
instance Serialize PlayerNetMessage