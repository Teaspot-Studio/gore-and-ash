{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game(
    mainWire
  , Game(..)
  ) where

import Control.DeepSeq
import Control.Wire
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.Core
import Game.Player

import Game.GoreAndAsh.Actor 

data Game = Game {
  gamePlayer :: !Player
} deriving (Generic)

instance NFData Game 

mainWire :: AppWire a Game
mainWire = Game
  <$> runActor' (playerWire initialPlayer)
  where 
    initialPlayer = Player 0 (V3 1 0 0) 0