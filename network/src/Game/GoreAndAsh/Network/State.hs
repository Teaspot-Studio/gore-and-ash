{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Network.State(
    NetworkState(..)
  , Host
  , Peer
  ) where

import Control.DeepSeq 
import Foreign
import GHC.Generics (Generic)
-- import Network.ENet.Host
import qualified Network.ENet.Bindings as B

-- | Server side connection
type Host = Ptr B.Host 
-- | Client side connection
type Peer = Ptr B.Peer

-- | Inner state of network layer
data NetworkState s = NetworkState {
  networkHost :: Maybe Host
, networkPeers :: [Peer]
, networkNextState :: !s
} deriving (Generic)

instance NFData (Ptr a) where 
  rnf p = p `seq` ()

instance NFData s => NFData (NetworkState s)