module Game.Shared(
    GameId(..)
  , GameNetMessage(..)
  , globalGameId
  , isPlayerSpawn
  , isPlayerDespawn
  , isPlayerRequestId
  , isPlayerResponseId
  , isPlayerRequestOthers
  ) where

import Control.DeepSeq
import GHC.Generics
import Data.Serialize

-- | Fake id for game global space
newtype GameId = GameId { unGameId :: Int } deriving (Eq, Generic)

instance NFData GameId 

-- | Statically known game id
globalGameId :: GameId 
globalGameId = GameId 0 

-- | Generic messages that are not binded to specific actor
data GameNetMessage = 
    PlayerSpawn !Int
  | PlayerDespawn !Int 
  | PlayerRequestId
  | PlayerResponseId !Int
  | PlayerRequestOthers
  | PlayerRequestData !Int -- Id of player we want info about
  deriving (Generic, Show)

instance NFData GameNetMessage
instance Serialize GameNetMessage

isPlayerSpawn :: GameNetMessage -> Bool
isPlayerSpawn m = case m of 
  PlayerSpawn _ -> True 
  _ -> False

isPlayerDespawn :: GameNetMessage -> Bool
isPlayerDespawn m = case m of 
  PlayerDespawn _ -> True 
  _ -> False

isPlayerRequestId :: GameNetMessage -> Bool 
isPlayerRequestId m = case m of 
  PlayerRequestId -> True 
  _ -> False

isPlayerResponseId :: GameNetMessage -> Bool 
isPlayerResponseId m = case m of 
  PlayerResponseId _ -> True 
  _ -> False

isPlayerRequestOthers :: GameNetMessage -> Bool 
isPlayerRequestOthers m = case m of 
  PlayerRequestOthers -> True 
  _ -> False