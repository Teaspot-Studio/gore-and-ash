{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game(
    mainWire
  , Player(..)
  , Game(..)
  , AppMonad
  , AppWire
  ) where

import Control.DeepSeq
import Control.Wire
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.Core

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