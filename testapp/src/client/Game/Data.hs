{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.Data(

  ) where

import Game.Shared

import Game.GoreAndAsh.Actor 
import Game.GoreAndAsh.Sync 

data GameMessage 

instance ActorMessage GameId where
  type ActorMessageType GameId = GameMessage 
  toCounter = unGameId
  fromCounter = GameId

instance NetworkMessage GameId where 
  type NetworkMessageType GameId = GameNetMessage
