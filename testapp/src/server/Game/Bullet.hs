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

bulletActor :: (BulletId -> Bullet) -> AppActor BulletId Game Bullet 
bulletActor initalBullet = makeActor $ \i -> stateWire (initalBullet i) $ mainController i
  where

  mainController :: BulletId -> AppWire (Game, Bullet) Bullet
  mainController _ = proc (_, b) -> do 
    traceEvent (const "Bullet created") . now -< ()
    returnA -< b