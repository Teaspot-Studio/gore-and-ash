{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Network.State(
    NetworkState(..)
  , Host
  , Peer
  , B.ChannelID(..)
  , Packet(..)
  ) where

import Control.DeepSeq 
import Data.Hashable
import Foreign
import GHC.Generics (Generic)
import Network.ENet
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S
import qualified Network.ENet.Bindings as B

-- | Server side connection
type Host = Ptr B.Host 
-- | Client side connection
type Peer = Ptr B.Peer

instance Hashable Peer where
  hashWithSalt s ptr = hashWithSalt s i 
    where 
      i :: Int
      i = fromIntegral $ ptrToIntPtr ptr

instance Hashable B.ChannelID where
  hashWithSalt s (B.ChannelID i) = hashWithSalt s i 

-- | Inner state of network layer
data NetworkState s = NetworkState {
  networkHost :: !(Maybe Host)
, networkPeers :: !(H.HashMap Peer ())
, networkConnectedPeers :: ![Peer]
, networkMessages :: !(H.HashMap (Peer, B.ChannelID) (S.Seq Packet))
, networkNextState :: !s
} deriving (Generic)

instance NFData (Ptr a) where 
  rnf p = p `seq` ()

instance NFData s => NFData (NetworkState s)

instance NFData B.ChannelID where 
  rnf (B.ChannelID i) = i `seq` ()

instance NFData Packet where 
  rnf (Packet f b) = f `deepseq` b `deepseq` ()