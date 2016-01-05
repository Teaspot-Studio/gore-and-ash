module Game.GoreAndAsh.Actor.Message(
    ActorMessage(..)
  ) where

import Data.Typeable

-- | The typeclass separates message API's of different type of actors
--  
-- In general you don't want to have one global type to handle all possible types of messages,
-- it will break modularity. Thats why you creates (with newtype) separate types of ids for
-- each actor and statically binds message type (usually algebraic type) to the id.
--
-- The class implies that your id is some integer type, but it could be not. Just provide way
-- to stable convertion of you id to integer and vice-versa.
class Typeable objectId => ActorMessage objectId where 
  -- | Binded message type, mailbox with id type of objectId would accept only this message type
  type ActorMessageType objectId :: * 
  -- | Convertion from global counter. Don't use it in client code as it could break type safety.
  fromCounter :: Int -> objectId
  -- | Convertion to global counter. Don't use it in client code as it could break type safety.
  toCounter :: objectId -> Int