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
import qualified Data.HashMap.Strict as H 
import Data.Hashable 

-- | Server side connection
type Host = Ptr B.Host 
-- | Client side connection
type Peer = Ptr B.Peer

instance Hashable Peer where
  hashWithSalt s ptr = hashWithSalt s i 
    where 
      i :: Int
      i = fromIntegral $ ptrToIntPtr ptr
      
-- | Inner state of network layer
data NetworkState s = NetworkState {
  networkHost :: !(Maybe Host)
, networkPeers :: !(H.HashMap Peer ())
, networkConnectedPeers :: ![Peer]
, networkNextState :: !s
} deriving (Generic)

instance NFData (Ptr a) where 
  rnf p = p `seq` ()

instance NFData s => NFData (NetworkState s)