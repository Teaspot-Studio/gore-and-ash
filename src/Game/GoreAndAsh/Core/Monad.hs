{-|
Module      : Game.GoreAndAsh.Core.Monad
Description : Definition of game monad and core modules.
Copyright   : (c) Anton Gushcha, 2015-2018
                  Oganyan Levon, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module defines basement for FRP engine on base of reflex framework. The most
useful context is 'MonadGame' that defines API of each game module.

Example of usage:
@
app :: forall t m . MonadGame t m => m ()
app = ...

main :: IO ()
main = runGM app
@
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Core.Monad(
    GM
  , GMSpider
  , runGM
  , EmbedNetwork
  , NetworksChan
  , runGMWithExternal
  , MonadGameConstraints
  , MonadGame(..)
  -- * Reexports
  , module Reflex
  , module Reflex.Collection
  , module Reflex.Host.Class
  , module Reflex.Network
  -- * Helpers for host monad
  , wrapError
  , dontCare
  , fcutMaybe
  , fcutEither
  , fkeepNothing
  , fkeepLeft
  , performNetwork
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad (void)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Control.Monad.STM
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.RSS.Strict
import Control.Monad.Writer
import Data.Dependent.Sum (DSum (..))
import Data.Either
import Data.Functor.Identity
import Data.IORef
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Proxy
import Data.Semigroup.Applicative
import GHC.Generics (Generic)
import Reflex
import Reflex.Class as Reexport
import Reflex.Collection
import Reflex.Host.Class
import Reflex.Network
import Reflex.NotReady.Class
import Reflex.PerformEvent.Base
import Reflex.PerformEvent.Class as Reexport
import Reflex.PostBuild.Base
import Reflex.PostBuild.Class as Reexport
import Reflex.Spider (Global, Spider, SpiderHost, runSpiderHost)
import Reflex.TriggerEvent.Base
import Reflex.TriggerEvent.Class as Reexport

import qualified Data.Map.Strict as M
import qualified Reflex.Spider.Internal as R

-- | Channel for events that should be fired in another thread
type EventChannel t = Chan [DSum (EventTriggerRef t) TriggerInvocation]
-- | Shortcut for event + fire action
type EventWithTrigger t a = (Event t a, a -> IO ())

-- | State of core.
data GameContext t = GameContext {
  envExit         :: !(EventWithTrigger t ()) -- ^ Event that indicates that main thread should exit
, envMainThread   :: !(TChan (IO ())) -- ^ Actions that should be executed in main thread
} deriving Generic

-- | Create empty context
newGameContext :: (TriggerEvent t m, MonadIO m) => TChan (IO ()) -> m (GameContext t)
newGameContext mtchan = do
  exitEv <- newTriggerEvent
  pure GameContext {
      envExit       = exitEv
    , envMainThread = mtchan
    }

-- | Implementation of reactive network of game engine. You usually don't need
-- to use this type explicetly, only abstract type classes such as 'MonadHold'
-- or 'MonadSample'.
--
-- Should be used in application monad stack as end monad:
--
-- @
-- type AppMonad a = LoggingT (ActorT (GM Spider (SpiderHost Global))) a
-- @
type GM t (m :: * -> *) = ReaderT (GameContext t) (PostBuildT t (TriggerEventT t (PerformEventT t m)))

-- | Shortcut for game monad instantiated for Spider FRP engine
--
-- @
-- type AppMonad a = LoggingT (ActorT GMSpider) a
-- @
type GMSpider = GM Spider (SpiderHost Global)

-- | Run main reactive network of server, exits when 'getExitEvent' resulted event is fired.
runGM :: MonadIO m => GMSpider a -> m a
runGM ma = do
  extchan <- liftIO newChan
  runGMWithExternal extchan ma

-- | Run main reactive network of server, exits when 'getExitEvent' resulted event is fired.
--
-- Channel for self embedding allows to integrate new created FRP networks from other threads.
runGMWithExternal :: forall m a . MonadIO m => NetworksChan Spider GMSpider -> GMSpider a -> m a
runGMWithExternal extchan ma = liftIO $ do
  events <- newChan
  exitVar <- newEmptyTMVarIO
  actionChan <- newTChanIO
  (a, fc) <- runGM' $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    a <- flip runTriggerEventT events $ do
      env <- newGameContext actionChan
      performEvent_ $ ffor (fst . envExit $ env) $ const $ liftIO . atomically $ putTMVar exitVar ()
      let ma' = do
            selfEmbedNetworks extchan
            ma
      runPostBuildT (runReaderT ma' env) postBuild
    pure (a, postBuildTriggerRef)
  processAsyncEvents events fc
  -- Start listening either for exit or for main thread actions
  let
    listenExit = do
      takeTMVar exitVar
      pure $ Left ()
    listenAction = do
      ma <- readTChan actionChan
      pure $ Right ma
    mainThreadJob = do
      res <- liftIO . atomically $ orElse listenExit listenAction
      case res of
        Left _ -> pure ()
        Right action -> do
          action
          mainThreadJob
  mainThreadJob
  pure a

-- | Internal helper for 'runGM'
runGM' :: PerformEventT Spider (SpiderHost Global) (a, IORef (Maybe (EventTrigger Spider ()))) -> IO (a, FireCommand Spider (SpiderHost Global))
runGM' ma = runSpiderHost $ do
  ((result, postBuildTriggerRef), fc@(FireCommand fire)) <- hostPerformEventT ma
  mPostBuildTrigger <- liftIO $ readIORef postBuildTriggerRef
  forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ pure ()
  pure (result, fc)

-- | Run separate thread that fires events from channel
processAsyncEvents :: EventChannel Spider -> FireCommand Spider (SpiderHost Global) -> IO ()
processAsyncEvents events fireCommand = void . forkIO . forever $ do
  ers <- readChan events
  _ <- runSpiderHost $ do
    mes <- liftIO $ forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
      me <- readIORef er
      pure $ (\e -> e :=> Identity a) <$> me
    triggerFires mes fireCommand
    liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
  pure ()
  where
    triggerFires mes (FireCommand fire) = void $ fire (catMaybes mes) $ pure ()

-- | Enumeration of type classes that 'MonadGame' should implement
type MonadGameConstraints t m =
  ( Adjustable t m
  , MonadFix m
  , MonadHold t m
  , MonadIO (HostFrame t)
  , MonadIO (Performable m)
  , MonadIO m
  , MonadRef (HostFrame t)
  , MonadReflexCreateTrigger t m
  , MonadSample t (Performable m)
  , NotReady t m
  , PerformEvent t m
  , PostBuild t m
  , PrimMonad (HostFrame t)
  , Ref m ~ IORef, Ref (HostFrame t) ~ IORef
  , ReflexHost t
  , TriggerEvent t m
  )

-- | Abstract interface for server side reactive networks. The wrapper is designed
-- as shorcut of set of reflex type classes. Instead of:
-- @
-- foo :: (MonadHold t m, Adjustable t m) => m ()
-- @
--
-- You can write:
-- @
-- foo :: MonadGame t m => m ()
-- @
--
-- [@t@] FRP engine implementation (needed by reflex). Almost always you should
-- just type 't' in all user code in the place and ignore it.
class MonadGameConstraints t m => MonadGame t m where
  -- | Get event that fires when some part of program required an exit
  getExitEvent :: m (Event t ())
  -- | Setup event, which fire is considered as signal to exit from program
  postExitEvent :: Event t () -> m ()
  -- | Send IO action in main thread to execute it there (important for OpenGL)
  performInMainThread :: Event t (IO a) -> m (Event t a)
  -- | Send IO action in main thread to execute it there (important for OpenGL)
  performInMainThread_ :: Event t (IO ()) -> m ()

instance {-# OVERLAPPING #-} MonadGameConstraints t m => MonadGame t (ReaderT (GameContext t) m) where
  getExitEvent = asks (fst . envExit)
  {-# INLINE getExitEvent #-}
  postExitEvent e = do
    fire <- asks $ snd . envExit
    performEvent_ $ ffor e $ const $ liftIO $ fire ()
  {-# INLINE postExitEvent #-}
  performInMainThread e = performEventAsync $ ffor e $ \ma ret -> do
    chan <- asks envMainThread
    liftIO . atomically $ writeTChan chan $ ret =<< ma
  {-# INLINE performInMainThread #-}
  performInMainThread_ e = performEvent_ $ ffor e $ \ma -> do
    chan <- asks envMainThread
    liftIO . atomically $ writeTChan chan ma
  {-# INLINE performInMainThread_ #-}

instance {-# OVERLAPPABLE #-} MonadGame t m => MonadGame t (ReaderT e m) where
  getExitEvent = lift getExitEvent
  {-# INLINE getExitEvent #-}
  postExitEvent = lift . postExitEvent
  {-# INLINE postExitEvent #-}
  performInMainThread = lift . performInMainThread
  {-# INLINE performInMainThread #-}

-- | TODO: check that the implementation is correct int context of game engine
-- TODO: possible reimplementation of Adjustable
instance NotReady Spider (PerformEventT Spider (SpiderHost Global)) where
  notReadyUntil e = pure ()
  notReady = pure ()

-- | FRP network that must be embedded to main network
type EmbedNetwork t m = m (Event t ())
-- | Channel with subnetworks that must be encorporated into main network
type NetworksChan t m = Chan (EmbedNetwork t m)

-- | Maintains collection of networks with feature of network self deleting
selfEmbedNetworks :: forall t m . (MonadGame t m)
  => NetworksChan t m -> m ()
selfEmbedNetworks networksChan = do
  (chanE, chanFire) <- newTriggerEvent
  _ <- liftIO . forkIO . forever $ chanFire =<< readChan networksChan
  rec activeNetworks :: Dynamic t (Map Int (Event t ())) <- listWithKeyShallowDiff mempty updNetworkE $ \_ ma _ -> ma
      let
        delsEvent :: Event t (Map Int ())
        delsEvent = switch . current $ mergeMap <$> activeNetworks

        networkCount :: Dynamic t Int
        networkCount = length <$> activeNetworks

        newServetE :: Event t (Map Int (EmbedNetwork t m))
        newServetE = uncurry M.singleton <$> attach (current networkCount) chanE

        updNetworkE :: Event t (Map Int (Maybe (EmbedNetwork t m)))
        updNetworkE = (fmap (const Nothing) <$> delsEvent) <> (fmap Just <$> newServetE)
  pure ()

-- | Helper to catch occurrences of error 'e' into 'Either'. Usually this is
-- needed for event generator that can throw, but you want to provide an 'Either'
-- in an event for end user.
wrapError :: (MonadCatch m, Exception e) => m a -> m (Either e a)
wrapError ma = (Right <$> ma) `catch` (return . Left)

-- | Rethrow errors in host monad, reverse of 'wrapError' when an end user doesn't
-- care about errors.
dontCare :: (MonadGame t m, Exception e)
  => Event t (Either e a) -> m (Event t a)
dontCare e = fmap (fmapMaybe id) $ performEvent $ ffor e $ \case
  Left err -> do
    liftIO $ print err
    pure Nothing
  Right a -> pure $ Just a

-- | Helper to pass through only a 'Just' values
fcutMaybe :: Reflex t => Event t (Maybe a) -> Event t a
fcutMaybe = fmap fromJust . ffilter isJust

-- | Helper to pass through only a 'Nothing' values
fkeepNothing :: Reflex t => Event t (Maybe a) -> Event t ()
fkeepNothing = void . ffilter isNothing

-- | Helper to pass through only a 'Right' values
fcutEither :: Reflex t => Event t (Either e a) -> Event t a
fcutEither = fmap (\(Right a) -> a) . ffilter isRight

-- | Helper to pass through only a 'Left' values
fkeepLeft :: Reflex t => Event t (Either e a) -> Event t e
fkeepLeft = fmap (\(Left e) -> e) . ffilter isLeft

-- | Wrapper around 'networkHold' that allows to perform network rebuild on event
performNetwork :: MonadGame t m => Event t (m a) -> m (Event t a)
performNetwork me = do
  resDyn <- networkHold (pure $ error "performNetwork: forced initial result, impossible happened!") me
  pure $ updated resDyn
