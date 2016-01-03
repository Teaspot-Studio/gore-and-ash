{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game(
    mainWire
  , Game(..)
  ) where

import Control.DeepSeq
import Control.Wire
import Data.Text (pack)
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))
import qualified Data.HashMap.Strict as H 

import Game.Core
import Game.Player

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor 
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network

type PlayerMap = H.HashMap PlayerId Player
type PlayerPeerMap = H.HashMap Peer PlayerId 

data Game = Game {
  gamePlayers :: !PlayerMap
, gamePlayerPeers :: !PlayerPeerMap
} deriving (Generic)

instance NFData Game 

mainWire :: AppWire a Game
mainWire = stateWire initGame $ proc (_, g) -> do 
  forceNF . processPlayers -< g
  where 
    -- | Game at start of the simulation
    initGame = Game {
        gamePlayers = H.empty
      , gamePlayerPeers = H.empty
      }

    -- | Handles process of players connection and disconnections
    processPlayers :: AppWire Game Game
    processPlayers = proc g -> do 
      addEvent <- mapE (fmap spawnPlayer) . peersConnected -< ()
      disEvent <- peersDisconnected -< ()
      remEvent <- filterJustLE -< fmap (despawnPlayer g) <$> disEvent

      traceEvent (const "New player connected") -< addEvent -- Player id is not ready yet :(
      traceEvent (\i -> "Player " <> (pack . show) i <> " disconnected") -< remEvent

      ps <- dynCollection [] -< ((), addEvent, remEvent)
      returnA -< g {
          gamePlayers = H.fromList $ fmap playerId  ps `zip` ps
        , gamePlayerPeers = H.fromList $ fmap playerPeer ps `zip` fmap playerId ps
        }

    -- | Spawns new player from peer (creates new arrow)
    spawnPlayer :: Peer -> AppActor PlayerId a Player
    spawnPlayer p = playerActor $ \i -> Player {
        playerId = i
      , playerPos = 0
      , playerColor = V3 1 0 0
      , playerRot = 0
      , playerPeer = p
      }

    -- | Detects player id by peer for despawning
    despawnPlayer :: Game -> Peer -> Maybe PlayerId
    despawnPlayer Game{..} p = H.lookup p gamePlayerPeers