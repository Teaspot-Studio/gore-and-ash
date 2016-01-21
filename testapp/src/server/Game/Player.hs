module Game.Player(
    Player(..)
  , PlayerId(..)
  , PlayerMessage
  , playerActor
  ) where

import Control.Wire
import Control.Wire.Unsafe.Event (event)
import Data.Text (pack)
import Linear
import Prelude hiding (id, (.))

import Game.Bullet.Data
import Game.Core
import Game.Data
import Game.Player.Data
import Game.Shared

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network 
import Game.GoreAndAsh.Sync

playerActor :: (PlayerId -> Player) -> AppActor PlayerId Game Player 
playerActor initialPlayer = makeActor $ \i -> stateWire (initialPlayer i) $ mainController i
  where
  mainController i = proc (g, p) -> do
    p2 <- peerProcessIndexedM peer (ChannelID 0) i netProcess -< p
    (_, p3) <- peerProcessIndexedM peer (ChannelID 0) globalGameId globalNetProcess -< (g, p2)
    forceNF . serverSync playerSync i . playerShot -< p3
    where
    -- | Shortcut for peer
    peer = playerPeer $ initialPlayer i

    -- | Handle when player is shot
    playerShot :: AppWire Player Player
    playerShot = proc p -> do 
      emsg <- actorMessages i isPlayerShotMessage -< ()
      let newPlayer = p {
          playerPos = 0
        }
      returnA -< event p (const newPlayer) emsg

    -- | Process player specific net messages
    netProcess :: Player -> PlayerNetMessage -> GameMonadT AppMonad Player 
    netProcess p msg = case msg of 
      NetMsgPlayerFire v -> do 
        let d = normalize v 
            v2 a = V2 a a
            pos = playerPos p + d * v2 (playerSize p * 1.5)
            vel = d * v2 bulletSpeed
        putMsgLnM $ "Fire bullet at " <> pack (show pos) <> " with velocity " <> pack (show vel)
        actorSendM globalGameId $ GameSpawnBullet pos vel $ playerId p
        return p 

    -- | Process global net messages from given peer (player)
    globalNetProcess :: (Game, Player) -> GameNetMessage -> GameMonadT AppMonad (Game, Player)
    globalNetProcess (g, p) msg = case msg of 
      PlayerRequestId -> do
        peerSendIndexedM peer (ChannelID 0) globalGameId ReliableMessage $ 
          PlayerResponseId (toCounter i) (toCounter $ gameBulletColId g) (toCounter $ gamePlayerColId g)
        return (g, p)
      _ -> do 
        putMsgLnM $ pack $ show msg
        return (g, p) 

    playerSync :: FullSync AppMonad PlayerId Player 
    playerSync = Player 
      <$> pure i 
      <*> clientSide peer 0 playerPos
      <*> clientSide peer 1 playerColor
      <*> clientSide peer 2 playerRot
      <*> clientSide peer 3 playerSpeed
      <*> clientSide peer 4 playerSize
      <*> pure peer