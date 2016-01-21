{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.Bullet(
    module ReExport
  , bulletActor
  ) where

import Control.Wire 
import Data.Serialize
import Data.Word 
import Linear 
import Prelude hiding (id, (.))
import qualified Data.HashMap.Strict as H

import Game.Bullet.Data as ReExport
import Game.Core 
import Game.Data 
import Game.Player.Data
import Game.Shared 

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor 
import Game.GoreAndAsh.Sync 

instance RemoteActor BulletId Bullet where
  type RemoteActorState BulletId = Bullet
  type RemoteActorId Bullet = BulletId

bulletActor :: (BulletId -> Bullet) -> AppActor BulletId Game Bullet 
bulletActor initalBullet = makeActor $ \i -> stateWire (initalBullet i) $ mainController i
  where

  mainController :: BulletId -> AppWire (Game, Bullet) Bullet
  mainController i = proc (g, b) -> do
    forceNF . serverSync bulletSync i . processBullet -< (b, g)
    where

    -- | Actual bullet logic
    processBullet :: AppWire (Bullet, Game) Bullet 
    processBullet = proc (b, g) -> do 
      actorSend globalGameId . at bulletLifespan -< GameDeleteBullet i
      playersShot -< (b, gamePlayers g)
      dt <- deltaTime -< ()
      let newPos = bulletPos b + V2 dt dt * bulletVel b 
      returnA -< b {
          bulletPos = newPos
        } 
      where 
      -- | Test all players was shot
      playersShot :: AppWire (Bullet, PlayerMap) ()
      playersShot = liftGameMonad2 $ \b ps -> mapM_ (playerShot b) . H.elems $! ps 

      -- | Test single player was shot
      playerShot :: Bullet -> Player -> GameMonadT AppMonad ()
      playerShot b p = do 
        let V2 px py = playerPos p 
            V2 bx by = bulletPos b
            cond = abs (px - bx) <= playerSize p && abs (py - by) <= playerSize p
        if cond then do 
            actorSendM globalGameId . GameDeleteBullet $! i
            actorSendM (playerId p) . PlayerShotMessage . bulletOwner $! b
          else return ()

    bulletSync :: FullSync AppMonad BulletId Bullet
    bulletSync = Bullet 
      <$> pure i 
      <*> fsync 0 bulletPos 
      <*> fsync 1 bulletVel 
      <*> fsync 2 bulletOwner
      where
        fsync :: (Eq a, Serialize a) 
          => Word64 -> (Bullet -> a) -> Sync AppMonad BulletId Bullet a
        fsync fi f = condSync (fieldChanges f &> periodic 4 . arr f) f $ serverSide fi f 