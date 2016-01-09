module Game.Player.Shared(
    PlayerNetMessage(..)
  , playerActorId
  ) where

import Control.DeepSeq
import Data.Serialize
import Data.Word
import GHC.Generics (Generic)

playerActorId :: Word64
playerActorId = 1

data PlayerNetMessage = 
    NetMsgPlayerPos !Double !Double
  | NetMsgPlayerRot !Double
  | NetMsgPlayerColor !Double !Double !Double
  | NetMsgPlayerSpeed !Double
  | NetMsgPlayerRequest
  deriving (Generic, Show)

instance NFData PlayerNetMessage
instance Serialize PlayerNetMessage