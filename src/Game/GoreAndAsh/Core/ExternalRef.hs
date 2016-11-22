{-|
Module      : Game.GoreAndAsh.Core.ExternalRef
Description : Helper for efficient state storage as external memory reference.
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Core.ExternalRef(
    ExternalRef(..)
  , newExternalRef
  , readExternalRef
  , writeExternalRef
  , modifyExternalRef
  , modifyExternalRefM
  , externalRefBehavior
  , externalRefDynamic
  ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Data.IORef
import Game.GoreAndAsh.Core.Monad
import GHC.Generics

-- | Holds value of type `a` and provides ways for notifying FRP network about
-- changes of the variable.
--
-- This abstraction is helpful for storing counters, lists of internal resources
-- and so on. It is designed to be updated from outputs of FRP network, not from
-- outer world.
data ExternalRef t a = ExternalRef {
  -- | Storage of value
  externalRef   :: !(IORef a)
  -- | Event that fires when value is changed
, externalEvent :: !(Event t a)
  -- | Method of updating valu of previous event
, externalFire  :: !(a -> IO Bool)
} deriving (Generic)

instance NFData (ExternalRef t a) where
  rnf ExternalRef{..} =
    externalRef `deepseq`
    externalEvent `seq`
    externalFire `seq`
    ()

-- | Creation of external ref in host monad
newExternalRef :: MonadAppHost t m => a -> m (ExternalRef t a)
newExternalRef a = do
  ref <- liftIO $ newIORef a
  (e, fire) <- newExternalEvent
  return $ ExternalRef ref e fire

-- | Read current value of external reference
readExternalRef :: MonadIO m => ExternalRef t a -> m a
readExternalRef ExternalRef{..} = liftIO $ readIORef externalRef

-- | Write new value to external reference and notify FRP network.
-- The function evaluates the value to WNF.
writeExternalRef :: MonadIO m => ExternalRef t a -> a -> m ()
writeExternalRef ExternalRef{..} a = do
  a `seq` liftIO (writeIORef externalRef a)
  _ <- liftIO $ externalFire a
  return ()

-- | Atomically modify an external ref and notify FRP network.
-- The function evaluates the value to WNF.
modifyExternalRef :: MonadIO m => ExternalRef t a -> (a -> (a, b)) -> m b
modifyExternalRef ExternalRef{..} f = do
  (a, b) <- liftIO $ atomicModifyIORef' externalRef $
    \a -> let (a', b) = f a in (a', (a', b))
  _ <- liftIO $ externalFire a
  return b

-- | Modify (not atomically) an external ref and notify FRP network.
-- The function evaluates the value to WNF.
modifyExternalRefM :: MonadIO m => ExternalRef t a -> (a -> m (a, b)) -> m b
modifyExternalRefM ExternalRef{..} f = do
  a <- liftIO $ readIORef externalRef
  (a', b) <- f a
  liftIO $ do
    writeIORef externalRef a'
    _ <- externalFire a
    return ()
  return b

-- | Construct a behavior from external reference
externalRefBehavior :: MonadAppHost t m => ExternalRef t a -> m (Behavior t a)
externalRefBehavior ExternalRef{..} = do
  a <- liftIO $ readIORef externalRef
  hold a externalEvent

externalRefDynamic :: MonadAppHost t m => ExternalRef t a -> m (Dynamic t a)
externalRefDynamic ExternalRef{..} = do
  a <- liftIO $ readIORef externalRef
  holdDyn a externalEvent