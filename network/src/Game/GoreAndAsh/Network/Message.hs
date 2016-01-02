module Game.GoreAndAsh.Network.Message(
    Message(..)
  , MessageType(..)
  , messageToPacket
  , packetToMessage
  ) where

import Control.DeepSeq
import GHC.Generics
import Network.ENet
import qualified Data.BitSet.Generic as B
import qualified Data.ByteString as BS 
import qualified Network.ENet.Bindings as B

-- | Strategy how given message is delivered to remote host
data MessageType = 
    ReliableMessage -- ^ TCP like, ordered reliable delivery
  | UnreliableMessage -- ^ Unrelieable, sequenced but fragments are sent with reliability
  | UnsequencedMessage -- ^ Unreliable and unsequenced (not sort while receiving)
  | UnreliableFragmentedMessage -- ^ Unreliable, sequenced sent with fragments sent within unreliable method
  | UnsequencedFragmentedMessage -- ^ Unreliable, unsequenced with fragments sent within unreliable method
  | ReceivedMessage -- ^ Message type is dropped as it is incoming message
  deriving (Eq, Ord, Bounded, Enum, Show, Generic)

instance NFData MessageType 

-- | Converts high-level message type to bits option for ENet
messageTypeToBits :: MessageType -> PacketFlagSet
messageTypeToBits t = case t of 
  ReliableMessage -> B.singleton B.Reliable
  UnsequencedMessage -> B.singleton B.Unsequenced
  UnsequencedFragmentedMessage -> B.UnreliableFragment `B.insert` B.singleton B.Unsequenced
  UnreliableMessage -> B.empty
  UnreliableFragmentedMessage -> B.singleton B.UnreliableFragment
  ReceivedMessage -> B.empty

-- | Message that has individual options about reliability 
data Message = Message {
  messageType :: !MessageType
, messagePayload :: !BS.ByteString
} deriving (Generic)

instance NFData Message 

-- | Convert message to internal ENet packet
messageToPacket :: Message -> Packet 
messageToPacket (Message mt p) = Packet (messageTypeToBits mt) p

-- | Convert arrived ENet packet to message type (not intended to use to send messages as message type is dropped)
packetToMessage :: Packet -> Message 
packetToMessage (Packet _ p) = Message ReceivedMessage p