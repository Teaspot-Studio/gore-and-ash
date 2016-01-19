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
import qualified Data.HashMap.Strict as H 

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
    (p4, changed) <- playerShot -< p3
    notifyAboutChanges i -< (g, p4, changed)
    updateClientsState i -< (g, p4)
    forceNF -< p4
    where
    -- | Shortcut for peer
    peer = playerPeer $ initialPlayer i

    -- | Handle when player is shot
    playerShot :: AppWire Player (Player, Bool) 
    playerShot = proc p -> do 
      emsg <- actorMessages i isPlayerShotMessage -< ()
      let newPlayer = p {
          playerPos = 0
        }
      returnA -< event (p, False) (const (newPlayer, True)) emsg

    -- | Sends full state of player to given peer and actor id
    sendFullData :: Peer -> Player -> GameMonadT AppMonad ()
    sendFullData peer1 p = do 
      let sendF mkMsg = peerSendIndexedM peer1 (ChannelID 0) (playerId p) ReliableMessage $ mkMsg p
      sendF ((\(V2 x y) -> NetMsgPlayerPos x y) . playerPos)
      sendF (NetMsgPlayerRot . playerRot)
      sendF ((\(V3 x y z) -> NetMsgPlayerColor x y z) . playerColor)
      sendF (NetMsgPlayerSpeed . playerSpeed)
      sendF (NetMsgPlayerSize . playerSize)

    -- | Process player specific net messages
    netProcess :: Player -> PlayerNetMessage -> GameMonadT AppMonad Player 
    netProcess p msg = case msg of 
      NetMsgPlayerPos cx cy -> do
        let v = V2 cx cy
        if norm (v - playerPos p) > 1.05 * playerSpeed p 
          then do 
            let V2 sx sy = playerPos p
            peerSendIndexedM (playerPeer p) (ChannelID 0) (playerId p) ReliableMessage 
              $ NetMsgPlayerPos sx sy
            return p 
          else return $ p { playerPos = v }

      NetMsgPlayerRot r -> return $ p { playerRot = r }
      NetMsgPlayerColor r g b -> return $ p { playerColor = V3 r g b }
      NetMsgPlayerSpeed v -> return $ p { playerSpeed = v }
      NetMsgPlayerSize s -> return $ p { playerSize = s }
      NetMsgPlayerRequest -> sendFullData (playerPeer p) p >> return p
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
          PlayerResponseId (toCounter i) (toCounter $ gameBulletColId g)
        return (g, p)
      PlayerRequestOthers -> do 
        notifyAboutSpawn i g 
        notifyAboutOtherPlayers i g 
        return (g, p)
      PlayerRequestData ri -> do 
        let pid = PlayerId ri 
        case H.lookup pid $ gamePlayers g of 
          Nothing -> return (g, p)
          Just p2 -> do 
            sendFullData (playerPeer p) p2
            return (g, p)
      _ -> do 
        putMsgLnM $ pack $ show msg
        return (g, p) 

    -- | When crated notify other players about spawn
    notifyAboutSpawn :: PlayerId -> Game -> GameMonadT AppMonad ()
    notifyAboutSpawn pid Game{..} = do 
      let ps = filter ((/= pid) . playerId) $ H.elems gamePlayers
          msgs = (\p -> (playerPeer p, PlayerSpawn $ toCounter pid)) <$> ps
      putMsgLnM $ "Notify another players about new player " <> pack (show $ playerId <$> ps)
      mapM_ (\(pr, msg) -> peerSendIndexedM pr (ChannelID 0) gameId ReliableMessage msg) msgs

    -- | When created notify client-side about other players on server
    notifyAboutOtherPlayers :: PlayerId -> Game -> GameMonadT AppMonad ()
    notifyAboutOtherPlayers pid Game{..} = do 
      let ps = filter ((/= pid) . playerId) $ H.elems gamePlayers
          msgs = (\p -> PlayerSpawn . toCounter $! playerId p) <$> ps
      putMsgLnM $ "Notify another players about new player " <> pack (show $ playerId <$> ps)
      mapM_ (peerSendIndexedM peer (ChannelID 0) gameId ReliableMessage) msgs

    -- | When player properties changes, spam about them to other players
    notifyAboutChanges :: PlayerId -> AppWire (Game, Player, Bool) ()
    notifyAboutChanges pid = proc (Game{..}, p, isNotifySelf) -> do 
      let ps = if isNotifySelf then H.elems gamePlayers
               else filter ((/= pid) . playerId) $ H.elems gamePlayers

      if isNotifySelf 
        then do 
          fieldChanges ReliableMessage pid changes playerPos (\(V2 x y) -> NetMsgPlayerPos x y) -< (p, ps)
          fieldChanges ReliableMessage pid changes playerRot NetMsgPlayerRot -< (p, ps)
          fieldChanges ReliableMessage pid changes playerColor (\(V3 x y z) -> NetMsgPlayerColor x y z) -< (p, ps)
          fieldChanges ReliableMessage pid changes playerSpeed NetMsgPlayerSpeed -< (p, ps)
          fieldChanges ReliableMessage pid changes playerSize NetMsgPlayerSize -< (p, ps)
        else do 
          fieldChanges UnreliableMessage pid changes playerPos (\(V2 x y) -> NetMsgPlayerPos x y) -< (p, ps)
          fieldChanges UnreliableMessage pid changes playerRot NetMsgPlayerRot -< (p, ps)
          fieldChanges UnreliableMessage pid changes playerColor (\(V3 x y z) -> NetMsgPlayerColor x y z) -< (p, ps)
          fieldChanges UnreliableMessage pid changes playerSpeed NetMsgPlayerSpeed -< (p, ps)
          fieldChanges UnreliableMessage pid changes playerSize NetMsgPlayerSize -< (p, ps)

      returnA -< ()


    -- | Periodically sends all state of player to clients
    updateClientsState :: PlayerId -> AppWire (Game, Player) ()
    updateClientsState pid = proc (Game{..}, p) -> do 
      let ps = H.elems gamePlayers

      fieldChanges UnreliableMessage pid emake playerPos (\(V2 x y) -> NetMsgPlayerPos x y) -< (p, ps)
      fieldChanges UnreliableMessage pid emake playerRot NetMsgPlayerRot -< (p, ps)
      fieldChanges UnreliableMessage pid emake playerColor (\(V3 x y z) -> NetMsgPlayerColor x y z) -< (p, ps)
      fieldChanges UnreliableMessage pid emake playerSpeed NetMsgPlayerSpeed -< (p, ps)
      fieldChanges UnreliableMessage pid emake playerSize NetMsgPlayerSize -< (p, ps)

      returnA -< ()
      where
        emake = periodic 4 -- period of update

    -- | Helper that sends updates about specific player field to given set of players
    fieldChanges :: Eq a => MessageType -> PlayerId -> (AppWire a (Event a)) -> (Player -> a) -> (a -> PlayerNetMessage) -> AppWire (Player, [Player]) ()
    fieldChanges mt pid eventGen fieldGetter fieldMessage = proc (p, ps) -> do 
      let field = fieldGetter p
      efield <- eventGen -< field
      let msgs = (\rp -> (playerPeer rp, pid, fieldMessage field)) <$> ps
      peerSendIndexedManyDyn (ChannelID 0) mt -< const msgs <$> efield 
      returnA -< ()