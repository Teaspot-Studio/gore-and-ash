module Game.Bullet.Shared(
    BulletNetMessage(..)
  ) where

import Control.DeepSeq
import Data.Serialize 
import GHC.Generics (Generic)

data BulletNetMessage = 
    NetMsgBulletStub
  deriving (Generic, Show)

instance NFData BulletNetMessage
instance Serialize BulletNetMessage