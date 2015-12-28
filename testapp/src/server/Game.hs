{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game(
    mainWire
  , Player(..)
  , Game(..)
  , AppMonad
  ) where

import Control.DeepSeq
import Control.Monad.Trans.Class
import Control.Wire
import Game.GoreAndAsh
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network

-- | First layer is logging layer with endpoint state 
type Layer1 = LoggingT IOState IO
-- | State of first layer that is used within chain
type Layer1State = LoggingState IOState
-- | Final layer is network layer with state of underneath layers
type AppMonad = NetworkT Layer1State Layer1
-- | Arrow that is build over the monad stack
type AppWire a b = GameWire AppMonad a b
  
instance LoggingMonad AppMonad where 
  putMsgM = lift . putMsgM
  putMsgLnM = lift . putMsgLnM

data Player = Player {
  playerPos :: !(V2 Float)
, playerColor :: !(V3 Float) 
, playerRot :: !Float  
} deriving (Generic)

instance NFData Player 

data Game = Game {
  gamePlayer :: !Player
} deriving (Generic)

instance NFData Game 

mainWire :: AppWire a Game
mainWire = Game
  <$> playerWire initialPlayer
  where 
    initialPlayer = Player 0 (V3 1 0 0) 0

playerWire :: Player -> AppWire a Player 
playerWire initialPlayer = loop $ proc (_, p_) -> do 
  p <- delay initialPlayer -< p_
  forceNF -< (p, p)