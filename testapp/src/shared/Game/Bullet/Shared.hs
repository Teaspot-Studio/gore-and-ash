module Game.Bullet.Shared(
    BulletNetMessage(..)
  , isBulletNetPos
  , isBulletNetVel
  , isBulletNetOwner
  , isBulletRequestState
  ) where

import Control.DeepSeq
import Data.Serialize 
import GHC.Generics (Generic)
import Linear

data BulletNetMessage = 
    BulletNetPos !(V2 Double)
  | BulletNetVel !(V2 Double)
  | BulletNetOwner !Int
  | BulletRequestState 
  deriving (Generic, Show)

instance NFData BulletNetMessage
instance Serialize BulletNetMessage

isBulletNetPos :: BulletNetMessage -> Bool 
isBulletNetPos m = case m of 
  BulletNetPos _ -> True
  _ -> False

isBulletNetVel :: BulletNetMessage -> Bool 
isBulletNetVel m = case m of 
  BulletNetVel _ -> True
  _ -> False

isBulletNetOwner :: BulletNetMessage -> Bool 
isBulletNetOwner m = case m of 
  BulletNetOwner _ -> True
  _ -> False

isBulletRequestState :: BulletNetMessage -> Bool 
isBulletRequestState m = case m of 
  BulletRequestState -> True
  _ -> False
