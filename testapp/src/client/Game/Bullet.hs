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
import Game.GoreAndAsh.Network 

import Graphics.Bullet

instance RemoteActor BulletId Bullet where
  type RemoteActorState BulletId = Bullet
  type RemoteActorId Bullet = BulletId

bulletActor :: Peer -> BulletId -> AppActor BulletId Game Bullet 
bulletActor peer i = makeFixedActor i $ stateWire initalBullet mainController
  where
  initalBullet = Bullet {
      bulletId = i
    , bulletPos = 0
    , bulletVel = 0
    , bulletOwner = fromCounter (-1)
    }

  mainController :: AppWire (Game, Bullet) Bullet
  mainController = proc (g, b) -> do
    liftGameMonad3 renderBullet -< (bulletPos b, bulletVel b, gameCamera g)
    forceNF . clientSync bulletSync peer i -< b

  bulletSync :: FullSync AppMonad BulletId Bullet
  bulletSync = Bullet 
    <$> pure i 
    <*> serverSide 0 bulletPos 
    <*> serverSide 1 bulletVel 
    <*> serverSide 2 bulletOwner