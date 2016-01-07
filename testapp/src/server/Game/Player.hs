module Game.Player(
    Player(..)
  , PlayerId(..)
  , PlayerMessage
  , playerActor
  ) where

import Control.Wire
import Data.Text (pack)
import Linear
import Prelude hiding (id, (.))
import qualified Data.HashMap.Strict as H 

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
playerActor initialPlayer = actorMaker mainController
  where
  -- | Helper to make actor
  actorMaker = netStateActor initialPlayer process 
    playerPeer 1 netProcess

  -- | Process local messages between local actors
  process :: PlayerId -> Player -> PlayerMessage -> Player 
  process _ p _ = p 

  -- | Process player specific net messages
  netProcess :: PlayerId -> ChannelID -> Player -> PlayerNetMessage -> Player 
  netProcess _ _ p msg = case msg of 
    NetMsgPlayerPos x y -> p { playerPos = V2 x y }
    NetMsgPlayerRot r -> p { playerRot = r }
    NetMsgPlayerColor r g b -> p { playerColor = V3 r g b }
    _ -> p 

  mainController i = proc (g, p) -> do
    p2 <- peerProcessIndexedM peer (ChannelID 0) globalGameId globalNetProcess -< p
    notifyAboutSpawn i -< g
    notifyAboutOtherPlayers i -< g
    notifyAboutChanges i -< (g, p2)
    forceNF -< p2
    where
    -- | Shortcut for peer
    peer = playerPeer $ initialPlayer i


    -- | Process global net messages from given peer (player)
    globalNetProcess :: Player -> GameNetMessage -> GameMonadT AppMonad Player
    globalNetProcess p msg = case msg of 
      PlayerRequestId -> do
        peerSendIndexedM peer (ChannelID 0) globalGameId ReliableMessage $ 
          PlayerResponseId $ toCounter i
        return p
      _ -> do 
        putMsgLnM $ pack $ show msg
        return p 

    -- | When crated notify other players about spawn
    notifyAboutSpawn :: PlayerId -> AppWire Game ()
    notifyAboutSpawn pid = proc Game{..} -> do 
      e <- now -< ()
      let ps = filter ((/= pid) . playerId) $ H.elems gamePlayers
          msgs = (\p -> (playerPeer p, gameId, PlayerSpawn $ toCounter pid)) <$> ps
      traceEvent (\ps -> "Notify another players about new player " <> pack (show $ playerId <$> ps)) -< const ps <$> e
      peerSendIndexedManyDyn (ChannelID 0) ReliableMessage -< const msgs <$> e
      returnA -< ()

    -- | When created notify client-side about other players on server
    notifyAboutOtherPlayers :: PlayerId -> AppWire Game ()
    notifyAboutOtherPlayers pid = proc Game{..} -> do
      e <- now -< ()
      let ps = filter ((/= pid) . playerId) $ H.elems gamePlayers
          msgs = (\p -> (peer, gameId, PlayerSpawn . toCounter $! playerId p)) <$> ps
      traceEvent (\ps -> "Notify spawned player about other players " <> pack (show $ playerId <$> ps)) -< const ps <$> e
      peerSendIndexedManyDyn (ChannelID 0) ReliableMessage -< const msgs <$> e 
      returnA -< ()

    -- | When player properties changes, spam about them to other players
    notifyAboutChanges :: PlayerId -> AppWire (Game, Player) ()
    notifyAboutChanges pid = proc (Game{..}, p) -> do 
      let ps = filter ((/= pid) . playerId) $ H.elems gamePlayers
      
      epos <- changes -< playerPos p
      let msgs = (\rp -> (playerPeer rp, pid, let V2 x y = playerPos p in NetMsgPlayerPos x y)) <$> ps
      traceEvent (\ps -> "Notify another players about new pos " <> pack (show $ playerId <$> ps)) -< const ps <$> epos
      peerSendIndexedManyDyn (ChannelID 0) UnreliableMessage -< const msgs <$> epos 

      returnA -< ()