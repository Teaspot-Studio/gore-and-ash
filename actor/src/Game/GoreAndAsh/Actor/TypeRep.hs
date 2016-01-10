module Game.GoreAndAsh.Actor.TypeRep(
    HashableTypeRep
  , toHashableTypeRep
  , fromHashableTypeRep
  , hashableTypeRep
  ) where

import Control.DeepSeq
import Data.Hashable 
import Data.Typeable
import GHC.Generics 

-- | Wrapper around TypeRep that supports Hashable and Eq, that are performed over type name
-- Note: the implentation is choosen to support equality of actors between several applications
newtype HashableTypeRep = HashableTypeRep { unHashableTypeRep :: TypeRep }
  deriving (Generic, Typeable)

instance Eq HashableTypeRep where 
  (HashableTypeRep a) == (HashableTypeRep b) = show a == show b

instance Show HashableTypeRep where 
  showsPrec i = showsPrec i . unHashableTypeRep

instance Hashable HashableTypeRep where
  hashWithSalt salt (HashableTypeRep tr) = salt `hashWithSalt` show tr

toHashableTypeRep :: TypeRep -> HashableTypeRep
toHashableTypeRep = HashableTypeRep

fromHashableTypeRep :: HashableTypeRep -> TypeRep 
fromHashableTypeRep = unHashableTypeRep

hashableTypeRep :: forall proxy a . Typeable a => proxy a -> HashableTypeRep
hashableTypeRep = HashableTypeRep . typeRep

instance NFData HashableTypeRep where
  rnf (HashableTypeRep tr) = rnfTypeRep tr