module Game.Player.Shared(
    PlayerNetMessage(..)
  ) where

import Control.DeepSeq
import Data.Serialize
import GHC.Generics (Generic)

data PlayerNetMessage = 
    NetMsgPlayerPos !Float !Float
  | NetMsgPlayerRot !Float
  | NetMsgPlayerColor !Float !Float !Float
  | NetMsgPlayerSpeed !Float
  deriving (Generic)

instance NFData PlayerNetMessage
instance Serialize PlayerNetMessage