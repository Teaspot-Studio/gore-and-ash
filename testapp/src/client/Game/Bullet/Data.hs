module Game.Bullet.Data(
    BulletId(..)
  , Bullet(..)
  , BulletMessage(..)
  , BulletNetMessage
  ) where

import Control.DeepSeq
import Data.Hashable
import Data.Typeable 
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Sync

import Game.Player.Data 
import Game.Bullet.Shared

data Bullet = Bullet {
  bulletId :: !BulletId
, bulletPos :: !(V2 Double)
, bulletVel :: !(V2 Double)
, bulletOwner :: !PlayerId
} deriving (Generic)

instance NFData Bullet 

newtype BulletId = BulletId { unBulletId :: Int } deriving (Eq, Show, Generic) 
instance NFData BulletId 
instance Hashable BulletId 

data BulletMessage = BulletMessageStub deriving (Typeable, Generic)
instance NFData BulletMessage 

instance ActorMessage BulletId where
  type ActorMessageType BulletId = BulletMessage
  toCounter = unBulletId
  fromCounter = BulletId
  
instance NetworkMessage BulletId where 
  type NetworkMessageType BulletId = BulletNetMessage