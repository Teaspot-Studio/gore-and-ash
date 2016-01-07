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
  actorMaker = stateActor initialPlayer process

  -- | Process local messages between local actors
  process :: PlayerId -> Player -> PlayerMessage -> Player 
  process _ p _ = p 

  mainController i = proc (g, p) -> do
    p2 <- peerProcessIndexedM peer (ChannelID 0) i netProcess -< p
    (_, p3) <- peerProcessIndexedM peer (ChannelID 0) globalGameId globalNetProcess -< (g, p2)
    notifyAboutChanges i -< (g, p3)
    forceNF -< p3
    where
    -- | Shortcut for peer
    peer = playerPeer $ initialPlayer i

    -- | Process player specific net messages
    netProcess :: Player -> PlayerNetMessage -> GameMonadT AppMonad Player 
    netProcess p msg = case msg of 
      NetMsgPlayerPos x y -> return $ p { playerPos = V2 x y }
      NetMsgPlayerRot r -> return $ p { playerRot = r }
      NetMsgPlayerColor r g b -> return $ p { playerColor = V3 r g b }
      NetMsgPlayerSpeed v -> return $ p { playerSpeed = v }

    -- | Process global net messages from given peer (player)
    globalNetProcess :: (Game, Player) -> GameNetMessage -> GameMonadT AppMonad (Game, Player)
    globalNetProcess (g, p) msg = case msg of 
      PlayerRequestId -> do
        peerSendIndexedM peer (ChannelID 0) globalGameId ReliableMessage $ 
          PlayerResponseId $ toCounter i
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
            let sendF mkMsg = peerSendIndexedM (playerPeer p) (ChannelID 0) pid ReliableMessage $ mkMsg p2
            sendF ((\(V2 x y) -> NetMsgPlayerPos x y) . playerPos)
            sendF (NetMsgPlayerRot . playerRot)
            sendF ((\(V3 x y z) -> NetMsgPlayerColor x y z) . playerColor)
            sendF (NetMsgPlayerSpeed . playerSpeed)
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
    notifyAboutChanges :: PlayerId -> AppWire (Game, Player) ()
    notifyAboutChanges pid = proc (Game{..}, p) -> do 
      let ps = filter ((/= pid) . playerId) $ H.elems gamePlayers
      
      fieldChanges pid playerPos (\(V2 x y) -> NetMsgPlayerPos x y) -< (p, ps)
      fieldChanges pid playerRot NetMsgPlayerRot -< (p, ps)
      fieldChanges pid playerColor (\(V3 x y z) -> NetMsgPlayerColor x y z) -< (p, ps)
      fieldChanges pid playerSpeed NetMsgPlayerSpeed -< (p, ps)

      returnA -< ()

    fieldChanges :: Eq a => PlayerId -> (Player -> a) -> (a -> PlayerNetMessage) -> AppWire (Player, [Player]) ()
    fieldChanges pid fieldGetter fieldMessage = proc (p, ps) -> do 
      let field = fieldGetter p
      efield <- changes -< field
      let msgs = (\rp -> (playerPeer rp, pid, fieldMessage field)) <$> ps
      peerSendIndexedManyDyn (ChannelID 0) UnreliableMessage -< const msgs <$> efield 
      returnA -< ()
