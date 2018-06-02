{-|
Module      : Game.GoreAndAsh.Core.Monad
Description : Definition of game monad and core modules.
Copyright   : (c) Anton Gushcha, 2015-2016
                  Oganyan Levon, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module defines 'GameMonadT' monad transformer as base monad for engine.
Also there is 'GameModule' class that must be implemented by all core modules. Finally 'ModuleStack'
type family is for user usage to compose all modules in single monad stack.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Core.Monad(
    GM
  , GMSpider
  , runGM
  , MonadGameConstraints
  , MonadGame
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
  ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Monad (void)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.RSS.Strict
import Control.Monad.Writer
import Data.Dependent.Sum (DSum (..))
import Data.Either
import Data.Functor.Identity
import Data.IORef
import Data.Maybe
import Data.Proxy
import Data.Semigroup.Applicative
import GHC.Generics (Generic)
import Reflex
import Reflex.Class as Reexport
import Reflex.Collection
import Reflex.Host.Class
import Reflex.Network
import Reflex.PerformEvent.Base
import Reflex.PerformEvent.Class as Reexport
import Reflex.PostBuild.Base
import Reflex.PostBuild.Class as Reexport
import Reflex.Spider (Global, Spider, SpiderHost, runSpiderHost)
import Reflex.Spider.Internal (SpiderTimeline)
import Reflex.TriggerEvent.Base
import Reflex.TriggerEvent.Class as Reexport

import qualified Reflex.Spider.Internal as R

-- | Channel for events that should be fired in another thread
type EventChannel t = Chan [DSum (EventTriggerRef t) TriggerInvocation]
-- | Shortcut for event + fire action
type EventWithTrigger t a = (Event t a, a -> IO ())

-- | State of core.
data GameContext t = GameContext {
  envExit :: !(EventWithTrigger t ()) -- ^ Event that indicates that main thread should exit
} deriving Generic

-- | Create empty context
newGameContext :: TriggerEvent t m => m (GameContext t)
newGameContext = do
  exitEv <- newTriggerEvent
  pure GameContext {
      envExit = exitEv
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
runGM ma = liftIO $ do
  events <- newChan
  exitVar <- newEmptyMVar
  (a, fc) <- runGM' $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    a <- flip runTriggerEventT events $ do
      env <- newGameContext
      performEvent_ $ ffor (fst . envExit $ env) $ const $ liftIO $ putMVar exitVar ()
      runPostBuildT (runReaderT ma env) postBuild
    pure (a, postBuildTriggerRef)
  processAsyncEvents events fc
  takeMVar exitVar
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
  , MonadIO (Performable m)
  , MonadIO m
  , MonadReflexCreateTrigger t m
  , MonadSample t (Performable m)
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  , ReflexHost t
  , MonadRef (HostFrame t)
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

instance {-# OVERLAPPING #-} MonadGameConstraints t m => MonadGame t (ReaderT (GameContext t) m) where
  getExitEvent = asks (fst. envExit)
  {-# INLINE getExitEvent #-}
  postExitEvent e = do
    fire <- asks $ snd . envExit
    performEvent_ $ ffor e $ const $ liftIO $ fire ()
  {-# INLINE postExitEvent #-}

instance {-# OVERLAPPABLE #-} MonadGame t m => MonadGame t (ReaderT e m) where
  getExitEvent = lift getExitEvent
  {-# INLINE getExitEvent #-}
  postExitEvent = lift . postExitEvent
  {-# INLINE postExitEvent #-}

-- -- | Internal representation of 'GameMonad'
-- type GameMonadStack t = ReaderT (GameContext t) (AppHost t)
--
-- -- | Endpoint for application monad stack that captures engine features for reactivity.
-- --
-- -- [@t@] FRP engine implementation (needed by reflex). Almost always you should
-- -- just type 't' in all user code in the place and ignore it.
-- --
-- -- [@a@] Value carried by the monad.
-- --
-- -- Should be used in application monad stack as end monad:
-- --
-- -- @
-- -- type AppMonad t a = LoggingT (ActorT (GameMonad t)) a
-- -- @
-- newtype GameMonad t a = GameMonad {
--   runGameMonad :: GameMonadStack t a
-- }
--
-- instance ReflexHost t => Functor (GameMonad t) where
--   fmap f (GameMonad m) = GameMonad $ fmap f m
--
-- -- | Monad is needed as StateT Applicative instance requires it
-- instance ReflexHost t => Applicative (GameMonad t) where
--   pure a = GameMonad $ pure a
--   (GameMonad f) <*> (GameMonad m) = GameMonad $ f <*> m
--
-- instance ReflexHost t => Monad (GameMonad t) where
--   return = pure
--   (GameMonad ma) >>= f = GameMonad $ do
--     a <- ma
--     runGameMonad $ f a
--
-- instance ReflexHost t => MonadFix (GameMonad t) where
--   mfix f = GameMonad $ mfix (runGameMonad . f)
--
-- instance (MonadIO (HostFrame t), ReflexHost t) => MonadIO (GameMonad t) where
--   liftIO m = GameMonad $ liftIO m
--
-- instance (ReflexHost t, MonadIO (HostFrame t)) => MonadBase IO (AppHost t) where
--   liftBase = liftHostFrame . liftIO
--
-- instance MonadBase IO (SpiderHost s) where
--   liftBase = R.SpiderHost . liftIO
--
-- instance MonadBaseControl IO (SpiderHost s) where
--   type StM (SpiderHost s) a = a
--   liftBaseWith ma = do
--     s <- R.SpiderHost ask
--     liftBase $ ma (flip runReaderT s . R.unSpiderHost)
--   restoreM = return
--
-- instance MonadBase IO (R.SpiderHostFrame s) where
--   liftBase = R.SpiderHostFrame . liftIO
--
-- instance MonadBaseControl IO (R.SpiderHostFrame s) where
--   type StM (R.SpiderHostFrame s) a = a
--   liftBaseWith ma =
--     liftBase $ ma (R.unEventM . R.runSpiderHostFrame)
--   restoreM = return
--
-- -- | TODO: move this to reflex-host (need to deep look in AppHost monad to prevent skolem problems)
-- instance (MonadBaseControl IO (HostFrame t), MonadIO (HostFrame t), ReflexHost t) => MonadBaseControl IO (AppHost t) where
--   type StM (AppHost t) a = StM (HostFrame t) (HostFrame t (AppInfo t), a)
--   liftBaseWith (ma :: RunInBase (AppHost t) IO -> IO a) = do
--     env <- AppHost ask
--     let rearrange (a, Ap m) = (m, a)
--     liftHostFrame $ liftBaseWith $ \(runnerIO :: RunInBase (HostFrame t) IO) -> do
--       let runner :: RunInBase (AppHost t) IO
--           runner (AppHost m) = runnerIO (rearrange <$> evalRSST m env ())
--       liftIO $ ma runner
--   restoreM stma = do
--     (hst, a) <- liftHostFrame $ restoreM stma
--     performPostBuild_ hst
--     return a
--
-- instance (MonadIO (HostFrame t), ReflexHost t) => MonadBase IO (GameMonad t) where
--   liftBase = GameMonad . liftIO
--
-- instance (MonadBaseControl IO (HostFrame t), MonadIO (HostFrame t), ReflexHost t) => MonadBaseControl IO (GameMonad t) where
--   type StM (GameMonad t) a = StM (GameMonadStack t) a
--   liftBaseWith ma = do
--     GameMonad $ liftBaseWith $ \runner -> ma (runner . runGameMonad)
--   restoreM stma = GameMonad $ restoreM stma
--
--
-- instance ReflexHost t => MonadSample t (GameMonad t) where
--   sample = GameMonad . sample
--
-- instance ReflexHost t => MonadHold t (GameMonad t) where
--   hold            a b = GameMonad $ hold a b
--   holdDyn         a b = GameMonad $ holdDyn a b
--   holdIncremental a b = GameMonad $ holdIncremental a b
--   buildDynamic    a b = GameMonad $ buildDynamic a b
--   headE           a   = GameMonad $ headE a
--
-- instance ReflexHost t => MonadSubscribeEvent t (GameMonad t) where
--   subscribeEvent = GameMonad . subscribeEvent
--
-- instance ReflexHost t => MonadReflexCreateTrigger t (GameMonad t) where
--   newEventWithTrigger = GameMonad . newEventWithTrigger
--   newFanEventWithTrigger trigger = GameMonad $ newFanEventWithTrigger trigger
--
-- instance (MonadIO (HostFrame t), ReflexHost t) => MonadAppHost t (GameMonad t) where
--   getFireAsync = GameMonad getFireAsync
--   getRunAppHost = do
--     runner <- GameMonad getRunAppHost
--     return $ \m -> runner $ runGameMonad m
--   performPostBuild_ = GameMonad . performPostBuild_
--   liftHostFrame = GameMonad . liftHostFrame
--
-- instance MonadThrow (R.EventM a) where
--   throwM = R.EventM . throwM
--
-- instance MonadThrow (R.SpiderHostFrame a) where
--   throwM = R.SpiderHostFrame . throwM
--
-- instance MonadThrow m => MonadThrow (RSST r w s m) where
--   throwM = lift . throwM
--
-- instance (MonadThrow (HostFrame t), ReflexHost t) => MonadThrow (AppHost t) where
--   throwM = AppHost . throwM
--
-- instance (MonadThrow (HostFrame t), ReflexHost t) => MonadThrow (GameMonad t) where
--   throwM = GameMonad . throwM
--
-- instance MonadCatch (R.EventM a) where
--   catch (R.EventM m) f = R.EventM $ m `catch` (R.unEventM . f)
--
-- instance MonadCatch (R.SpiderHostFrame a) where
--   catch (R.SpiderHostFrame m) f = R.SpiderHostFrame $ m `catch` (R.runSpiderHostFrame . f)
--
-- instance (Monoid w, MonadCatch m) => MonadCatch (RSST r w s m) where
--   catch m f = do
--     r <- ask
--     s <- get
--     (a, s', w) <- lift $ runRSST m r s `catch` (\e -> runRSST (f e) r s)
--     put s'
--     tell w
--     return a
--
-- instance (MonadCatch (HostFrame t), ReflexHost t) => MonadCatch (AppHost t) where
--   catch (AppHost m) f = AppHost $ m `catch` (unAppHost . f)
--
-- instance (MonadCatch (HostFrame t), ReflexHost t) => MonadCatch (GameMonad t) where
--   catch (GameMonad m) f = GameMonad $ m `catch` (runGameMonad . f)
--
-- instance (Monoid w, MonadMask m) => MonadMask (RSST r w s m) where
--   mask m = do
--     r <- ask
--     s <- get
--     (a, s', w) <- lift $ mask $ \u -> runRSST (m $ q u) r s
--     put s'
--     tell w
--     return a
--     where
--       q :: (forall b . m b -> m b) -> RSST r w s m a -> RSST r w s m a
--       q u m' = do
--         r <- ask
--         s <- get
--         (a, s', w) <- lift $ u (runRSST m' r s)
--         put s'
--         tell w
--         return a
--
--   uninterruptibleMask m = do
--     r <- ask
--     s <- get
--     (a, s', w) <- lift $ uninterruptibleMask $ \u -> runRSST (m $ q u) r s
--     put s'
--     tell w
--     return a
--     where
--       q :: (forall b . m b -> m b) -> RSST r w s m a -> RSST r w s m a
--       q u m' = do
--         r <- ask
--         s <- get
--         (a, s', w) <- lift $ u (runRSST m' r s)
--         put s'
--         tell w
--         return a
--
-- instance MonadMask (R.EventM s) where
--   mask m = R.EventM $ mask $ \u -> R.unEventM (m $ q u)
--     where
--     q :: (forall b . IO b -> IO b) -> R.EventM s a -> R.EventM s a
--     q u m' = R.EventM $ u (R.unEventM m')
--   uninterruptibleMask m = R.EventM $ uninterruptibleMask $ \u -> R.unEventM (m $ q u)
--     where
--     q :: (forall b . IO b -> IO b) -> R.EventM s a -> R.EventM s a
--     q u m' = R.EventM $ u (R.unEventM m')
--
-- instance MonadMask (R.SpiderHostFrame s) where
--   mask m = R.SpiderHostFrame $ mask $ \u -> R.runSpiderHostFrame (m $ q u)
--     where
--     q :: (forall b . R.EventM s b -> R.EventM s b) -> R.SpiderHostFrame s a -> R.SpiderHostFrame s a
--     q u m' = R.SpiderHostFrame $ u (R.runSpiderHostFrame m')
--   uninterruptibleMask m = R.SpiderHostFrame $ uninterruptibleMask $ \u -> R.runSpiderHostFrame (m $ q u)
--     where
--     q :: (forall b . R.EventM s b -> R.EventM s b) -> R.SpiderHostFrame s a -> R.SpiderHostFrame s a
--     q u m' = R.SpiderHostFrame $ u (R.runSpiderHostFrame m')
--
-- type AppHostStack t = RSST (AppEnv t) (Ap (HostFrame t) (AppInfo t)) () (HostFrame t)
--
-- instance (MonadMask (HostFrame t), ReflexHost t) => MonadMask (AppHost t) where
--   mask m = AppHost $ mask $ \u -> unAppHost (m $ q u)
--     where
--     q :: (forall b . AppHostStack t b -> AppHostStack t b) -> AppHost t a -> AppHost t a
--     q u m' = AppHost $ u (unAppHost m')
--   uninterruptibleMask m = AppHost $ uninterruptibleMask $ \u -> unAppHost (m $ q u)
--     where
--     q :: (forall b . AppHostStack t b -> AppHostStack t b) -> AppHost t a -> AppHost t a
--     q u m' = AppHost $ u (unAppHost m')
--
-- instance (MonadMask (HostFrame t), ReflexHost t) => MonadMask (GameMonad t) where
--   mask m = GameMonad $ mask $ \u -> runGameMonad (m $ q u)
--     where
--     q :: (forall b . GameMonadStack t b -> GameMonadStack t b) -> GameMonad t a -> GameMonad t a
--     q u m' = GameMonad $ u (runGameMonad m')
--   uninterruptibleMask m = GameMonad $ uninterruptibleMask $ \u -> runGameMonad (m $ q u)
--     where
--     q :: (forall b . GameMonadStack t b -> GameMonadStack t b) -> GameMonad t a -> GameMonad t a
--     q u m' = GameMonad $ u (runGameMonad m')
--
-- -- TODO: move this to reflex-host
-- instance MonadAppHost t m => MonadAppHost t (StateT s m) where
--   getFireAsync = lift getFireAsync
--   getRunAppHost = do
--     runner <- lift getRunAppHost
--     s <- get
--     return $ \m -> runner $ evalStateT m s
--   performPostBuild_ = lift . performPostBuild_
--   liftHostFrame = lift . liftHostFrame
--
-- instance MonadAppHost t m => MonadAppHost t (ReaderT s m) where
--   getFireAsync = lift getFireAsync
--   getRunAppHost = do
--     runner <- lift getRunAppHost
--     s <- ask
--     return $ \m -> runner $ runReaderT m s
--   performPostBuild_ = lift . performPostBuild_
--   liftHostFrame = lift . liftHostFrame
--
-- instance MonadSample t m => MonadSample t (RSST r w s m) where
--   sample = lift . sample
--
-- instance MonadHold t m => MonadHold t (RSST r w s m) where
--   hold            a b = lift $ hold a b
--   holdDyn         a b = lift $ holdDyn a b
--   holdIncremental a b = lift $ holdIncremental a b
--   buildDynamic    a b = lift $ buildDynamic a b
--   headE           a   = lift $ headE a
--
-- instance MonadSubscribeEvent t m => MonadSubscribeEvent t (RSST r w s m) where
--   subscribeEvent = lift . subscribeEvent
--
-- instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (RSST r w s m) where
--   newEventWithTrigger = lift . newEventWithTrigger
--   newFanEventWithTrigger trigger = lift $ newFanEventWithTrigger trigger
--
-- instance (Monoid w, MonadAppHost t m) => MonadAppHost t (RSST r w s m) where
--   getFireAsync = lift getFireAsync
--   getRunAppHost = do
--     runner <- lift getRunAppHost
--     r <- ask
--     s <- get
--     return $ \m -> runner . fmap (\(a, _, _) -> a) $ runRSST m r s
--   performPostBuild_ = lift . performPostBuild_
--   liftHostFrame = lift . liftHostFrame
--
-- instance MonadSample t m => MonadSample t (IdentityT m) where
--   sample = lift . sample
--
-- instance MonadHold t m => MonadHold t (IdentityT m) where
--   hold            a b = lift $ hold a b
--   holdDyn         a b = lift $ holdDyn a b
--   holdIncremental a b = lift $ holdIncremental a b
--   buildDynamic    a b = lift $ buildDynamic a b
--   headE           a   = lift $ headE a
--
-- instance MonadSubscribeEvent t m => MonadSubscribeEvent t (IdentityT m) where
--   subscribeEvent = lift . subscribeEvent
--
-- instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (IdentityT m) where
--   newEventWithTrigger = lift . newEventWithTrigger
--   newFanEventWithTrigger trigger = lift $ newFanEventWithTrigger trigger
--
-- instance (MonadAppHost t m) => MonadAppHost t (IdentityT m) where
--   getFireAsync = lift getFireAsync
--   getRunAppHost = do
--     runner <- lift getRunAppHost
--     return $ \m -> runner $ runIdentityT m
--   performPostBuild_ = lift . performPostBuild_
--   liftHostFrame = lift . liftHostFrame

-- | Helper to catch occurrences of error 'e' into 'Either'. Usually this is
-- needed for event generator that can throw, but you want to provide an 'Either'
-- in an event for end user.
wrapError :: (MonadCatch m, Exception e) => m a -> m (Either e a)
wrapError ma = (Right <$> ma) `catch` (return . Left)

-- | Rethrow errors in host monad, reverse of 'wrapError' when an end user doesn't
-- care about errors.
dontCare :: (MonadGame t m, MonadThrow (Performable m), Exception e)
  => Event t (Either e a) -> m (Event t a)
dontCare e = performEvent $ ffor e $ \case
  Left err -> throwM err
  Right a -> return a

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
