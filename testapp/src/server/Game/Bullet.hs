{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.Bullet(
    module ReExport
  , bulletActor
  ) where

import Control.Wire 
import Prelude hiding (id, (.))
import qualified Data.Sequence as S 

import Game.Bullet.Data as ReExport
import Game.Bullet.Shared 
import Game.Core 
import Game.Data 

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor 
import Game.GoreAndAsh.Network 
import Game.GoreAndAsh.Sync 

instance RemoteActor BulletId Bullet where
  type RemoteActorState BulletId = Bullet
  type RemoteActorId Bullet = BulletId

bulletActor :: (BulletId -> Bullet) -> AppActor BulletId Game Bullet 
bulletActor initalBullet = makeActor $ \i -> stateWire (initalBullet i) $ mainController i
  where

  mainController :: BulletId -> AppWire (Game, Bullet) Bullet
  mainController i = proc (_, b) -> do
    syncOnRequest -< b
    syncChanges -< b 
    syncPeriodic -< b
    forceNF -< b
    where

    -- | Sends full state of actor to given peer
    sendFullData :: Peer -> Bullet -> GameMonadT AppMonad ()
    sendFullData peer b = do 
      let sendF mkMsg = peerSendIndexedM peer (ChannelID 0) i ReliableMessage $ mkMsg b
      sendF (BulletNetPos . bulletPos)
      sendF (BulletNetVel . bulletVel)
      sendF (BulletNetOwner . toCounter . bulletOwner)

    -- | If one of peers wants full sync, send all data to it
    syncOnRequest :: AppWire Bullet ()
    syncOnRequest = onPeers $ \peers -> proc b -> do 
      sequenceA (syncOnRequestPeer <$> peers) -< b
      returnA -< ()

    -- | If peer wants full sync, send all data to it
    syncOnRequestPeer :: Peer -> AppWire Bullet ()
    syncOnRequestPeer peer = proc b -> do 
      e <- filterMsgs isBulletRequestState . peerIndexedMessages peer (ChannelID 0) i -< ()
      liftGameMonadEvent1 (sendFullData peer) -< const b <$> e 
      returnA -< ()

    -- | When bullet properties changes, spam about them to peers
    syncChanges :: AppWire Bullet ()
    syncChanges = onPeers $ \peers -> proc b -> do 
      fieldChanges peers changes bulletPos BulletNetPos -< b
      fieldChanges peers changes bulletVel BulletNetVel -< b
      fieldChanges peers changes bulletOwner (BulletNetOwner . toCounter) -< b

    -- | Periodically sends all state of bullet to peers
    syncPeriodic :: AppWire Bullet ()
    syncPeriodic = onPeers $ \peers -> proc b -> do  
      fieldChanges peers emake bulletPos BulletNetPos -< b
      fieldChanges peers emake bulletVel BulletNetVel -< b
      fieldChanges peers emake bulletOwner (BulletNetOwner . toCounter) -< b
      where
        emake = periodic 4 -- period of update

    -- | Helper that sends updates about specific player field to given set of players
    fieldChanges :: Eq a => S.Seq Peer -> (AppWire a (Event a)) -> (Bullet -> a) -> (a -> BulletNetMessage) -> AppWire Bullet ()
    fieldChanges peers eventGen fieldGetter fieldMessage = proc b -> do 
      let field = fieldGetter b
      efield <- eventGen -< field
      let msgs = (, i, fieldMessage field) <$> peers
      peerSendIndexedManyDyn (ChannelID 0) UnreliableMessage -< const msgs <$> efield 
      returnA -< ()