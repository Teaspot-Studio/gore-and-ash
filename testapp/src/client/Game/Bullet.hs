{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.Bullet(
    module ReExport
  , bulletActor
  ) where

import Control.Wire 
import Prelude hiding (id, (.))

import Game.Bullet.Data as ReExport
import Game.Core 
import Game.Data 

import Game.GoreAndAsh
import Game.GoreAndAsh.Sync 
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Logging 

instance RemoteActor BulletId Bullet where
  type RemoteActorState BulletId = Bullet
  type RemoteActorId Bullet = BulletId

bulletActor :: BulletId -> AppActor BulletId Game Bullet 
bulletActor i = makeFixedActor i $ stateWire initalBullet mainController
  where
  initalBullet = Bullet {
      bulletId = i
    , bulletPos = 0
    , bulletVel = 0
    , bulletOwner = fromCounter (-1)
    }

  mainController :: AppWire (Game, Bullet) Bullet
  mainController = proc (_, b) -> do 
    traceEvent (const "Bullet created") . now -< ()
    returnA -< b