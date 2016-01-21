module Game.Shared(
    GameId(..)
  , GameNetMessage(..)
  , globalGameId
  , isPlayerRequestId
  , isPlayerResponseId
  -- | Helpers
  , mapFromSeq
  ) where

import Control.DeepSeq
import Data.Hashable 
import Data.Serialize
import GHC.Generics
import qualified Data.Foldable as F 
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

-- | Fake id for game global space
newtype GameId = GameId { unGameId :: Int } deriving (Eq, Generic)

instance NFData GameId 

-- | Statically known game id
globalGameId :: GameId 
globalGameId = GameId 0 

-- | Generic messages that are not binded to specific actor
data GameNetMessage =
    PlayerRequestId
  | PlayerResponseId !Int !Int !Int -- ^ Id of player and bullets, players collections
  deriving (Generic, Show)

instance NFData GameNetMessage
instance Serialize GameNetMessage

isPlayerRequestId :: GameNetMessage -> Bool 
isPlayerRequestId m = case m of 
  PlayerRequestId -> True 
  _ -> False

isPlayerResponseId :: GameNetMessage -> Bool 
isPlayerResponseId m = case m of 
  PlayerResponseId _ _ _ -> True 
  _ -> False

-- | Construct HashMap from sequence
mapFromSeq :: (Hashable i, Eq i) => S.Seq (i, a) -> H.HashMap i a 
mapFromSeq = F.foldl' (\acc (i, a) -> H.insert i a acc) H.empty