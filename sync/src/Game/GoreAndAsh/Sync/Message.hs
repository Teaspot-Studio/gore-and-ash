module Game.GoreAndAsh.Sync.Message(
    NetworkMessage(..)
  , peerIndexedMessages
  ) where

import Control.Wire
import Data.Maybe 
import Data.Serialize
import Data.Word
import qualified Data.ByteString as BS 
import Prelude hiding ((.), id)

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network 

-- | Extension for actor message, messages that are sent to remote host
class ActorMessage i => NetworkMessage i where
  -- | Corresponding message payload for @i@ identifier, usually ADT
  type NetworkMessageType i :: *

-- | Fires when network messages for specific actor has arrived
peerIndexedMessages :: forall m i a . (NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
  => Peer -- ^ Which peer we are listening
  -> ChannelID -- ^ Which channel we are listening
  -> i -- ^ ID of actor 
  -> GameWire m a (Event [NetworkMessageType i]) -- ^ Messages that are addressed to the actor
peerIndexedMessages p chid i = filterE (not . null) 
  . mapE (catMaybes . fmap parse . filter hasId)
  . peerMessages p chid
  where
    parse :: BS.ByteString -> Maybe (NetworkMessageType i)
    parse bs = case decode bs of 
      Left _ -> Nothing
      Right m -> Just m

    hasId :: BS.ByteString -> Bool
    hasId bs 
      | BS.length bs < 4 = False
      | otherwise = fromIntegral (mkWord32 $ BS.take 4 bs) == toCounter i

    mkWord32 :: BS.ByteString -> Word32 
    mkWord32 bs = b4 + b3 * 256 + b2 * 256 * 256  + b1 * 256 * 256 * 256
      where [b1, b2, b3, b4] = fromIntegral <$> BS.unpack bs