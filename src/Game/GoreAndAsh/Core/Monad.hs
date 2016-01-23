{-|
Module      : Game.GoreAndAsh.Core.Monad
Description : Definition of game monad and core modules.
Copyright   : (c) Anton Gushcha, 2015-2016
                  Oganyan Levon, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module defines 'GameMonadT' monad transformer as base monad for all arrows of ther engine.
Also there is 'GameModule' class that must be implemented by all core modules. Finally 'ModuleStack'
type family is for user usage to compose all modules in single monad stack.
-}
module Game.GoreAndAsh.Core.Monad(
    GameMonadT
  , GameContext(..)
  , newGameContext
  , evalGameMonad
  , GameModule(..)
  , IOState
  , IdentityState
  , ModuleStack
  ) where

import Control.DeepSeq
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Functor.Identity
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)

-- | Basic game monad transformer which wraps core modules.
--
-- Here goes all core API that accessable from each 
-- game object. All specific (mods etc) API should
-- be included in inner `m` monad.
--
-- [@m@] Core modules monads stacked up here.
--
-- [@a@] Value caried by the monad.
--
-- The monad is used to create new arrows, there a 90% chances
-- that you will create your own arrows. You could use "Control.Wire.Core"
-- module and especially 'mkGen', 'mkGen_' and 'mkSFN' functions to create
-- new arrows.
newtype GameMonadT m a = GameMonadT { 
  runGameMonadT :: StateT GameContext m a
} deriving (MonadThrow, MonadCatch, MonadMask)

-- | State of core.
--
-- At the moment it is empty, but left for future
-- extensions. For example, some introspection API
-- of enabled modules would be added.
data GameContext = GameContext {
  
} deriving Generic

instance NFData GameContext

-- | Create empty context
newGameContext :: GameContext 
newGameContext = GameContext

instance Functor m => Functor (GameMonadT m) where 
  fmap f (GameMonadT m) = GameMonadT $ fmap f m

-- | Monad is needed as StateT Applicative instance requires it
instance Monad m => Applicative (GameMonadT m) where
  pure a = GameMonadT $ pure a
  (GameMonadT f) <*> (GameMonadT m) = GameMonadT $ f <*> m

instance Monad m => Monad (GameMonadT m) where 
  return = pure 
  (GameMonadT ma) >>= f = GameMonadT $ do 
    a <- ma
    runGameMonadT $ f a

instance MonadFix m => MonadFix (GameMonadT m) where
  mfix f = GameMonadT $ mfix (runGameMonadT . f)

instance MonadTrans GameMonadT where 
  lift = GameMonadT . lift

instance MonadIO m => MonadIO (GameMonadT m) where 
  liftIO = GameMonadT . liftIO

-- | Runs game monad with given context
evalGameMonad :: GameMonadT m a -> GameContext -> m (a, GameContext)
evalGameMonad (GameMonadT m) ctx = runStateT m ctx

-- | Describes how to run core modules. Each core module must define
-- an instance of the class.
--
-- The class describes how the module is executed each game frame
-- and how to pass its own state to the next state.
--
-- The state 's' must be unique for each game module.
--
-- 'GameMonadT' has 'm' parameter that should implement the class.
--
-- Typical backbone of new core module:
--
-- @
--   -- | State of your module
--   data MyModuleState s = MyModuleState {
--     -- | Next state in state chain of modules
--   , myModuleNextState :: !s
--   } deriving (Generic)
--   
--   -- | Needed to step game state
--   instance NFData s => NFData (MyModuleState s)
--  
--   -- | Creation of initial state
--   emptyMyModuleState :: s -> MyModuleState s 
--   emptyMyModuleState s = MyModuleState {
--       myModuleNextState = s
--     }
--   
--   -- Your monad transformer that implements module API
--   newtype MyModuleT s m a = MyModuleT { runMyModuleT :: StateT (MyModuleState s) m a }
--     deriving (Functor, Applicative, Monad, MonadState (MyModuleState s), MonadFix, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask)
--   
--   instance GameModule m s => GameModule (MyModuleT s m) (MyModuleState s) where 
--     type ModuleState (MyModuleT s m) = MyModuleState s
--     runModule (MyModuleT m) s = do
--       -- First phase: execute all dependent modules actions and transform own state 
--       ((a, s'), nextState) <- runModule (runStateT m s) (myModuleNextState s)
--       -- Second phase: here you could execute your IO actions
--       return (a, s' { 
--          myModuleNextState = nextState 
--         })
--   
--     newModuleState = emptyMyModuleState <$> newModuleState
--   
--     withModule _ = id
--     cleanupModule _ = return ()
--   
--   -- | Define your module API
--   class OtherModuleMonad m => MyModuleMonad m where
--     -- | The function would be seen in any arrow
--     myAwesomeFunction :: AnotherModule m => a -> b -> m (a, b) 
--   
--   -- | Implementation of API
--   instance {-# OVERLAPPING #-} OtherModuleMonad m => MyModuleMonad (MyModuleT s m) where
--      myAwesomeFunction = ...
--  
--   -- | Passing calls through other modules
--   instance {-# OVERLAPPABLE #-} (MyModuleMonad m, MonadTrans mt) => MyModuleMonad (mt m) where 
--     myAwesomeFunction a b = lift $ myAwesomeFunction a b
-- @
--
-- After the backbone definition you could include your monad to application stack with 'ModuleStack'
-- and use it within any arrow in your application.
class Monad m => GameModule m s | m -> s, s -> m where
  -- | Defines what state has given module.
  --
  -- The correct implentation of the association:
  -- >>> type ModuleState (MyModuleT s m) = MyModuleState s
  type ModuleState m :: *

  -- | Executes module action with given state. Produces new state that should be passed to next step
  --
  -- Each core module has responsibility of executing underlying modules with nested call to 'runModule'.
  --
  -- Typically there are two phases of execution:
  --
  --   * Calculation of own state and running underlying modules
  --
  --   * Execution of IO actions that are queued in module state
  --
  -- Some of modules requires 'IO' monad at the end of monad stack to call 'IO' actions in place within
  -- first phase of module execution (example: network module). You should avoid the pattern and prefer 
  -- to execute 'IO' actions at the second phase as bad designed use of first phase could lead to strange 
  -- behavior at arrow level.
  runModule :: MonadIO m' => m a -> s -> m' (a, s)
  -- | Creates new state of module.
  -- 
  -- Typically there are nested calls to 'newModuleState' for nested modules.
  -- @
  -- newModuleState = emptyMyModuleState <$> newModuleState
  -- @
  newModuleState :: MonadIO m' => m' s
  -- | Wrap action with module initialization and cleanup.
  --
  -- Could be `withSocketsDo` or another external library initalization.
  withModule :: Proxy m -> IO a -> IO a
  -- | Cleanup resources of the module, should be called on exit (actually 'cleanupGameState' do this for your)
  cleanupModule :: s -> IO ()

-- | Type level function that constucts complex module stack from given list of modules.
--
-- The type family helps to simplify chaining of core modules at user application:
--
-- @
-- | Application monad is monad stack build from given list of modules over base monad (IO)
-- type AppStack = ModuleStack [LoggingT, ActorT, NetworkT] IO
-- newtype AppState = AppState (ModuleState AppStack)
--   deriving (Generic)
-- 
-- instance NFData AppState 
-- 
-- -- | Wrapper around type family to enable automatic deriving
-- -- 
-- -- Note: There could be need of manual declaration of module API stub instances, as GHC can fail to derive instance automatically.
-- newtype AppMonad a = AppMonad (AppStack a)
--   deriving (Functor, Applicative, Monad, MonadFix, MonadIO, LoggingMonad, NetworkMonad, ActorMonad, MonadThrow, MonadCatch)
-- 
-- -- | Top level wrapper for module stack
-- instance GameModule AppMonad AppState where 
--   type ModuleState AppMonad = AppState
--   runModule (AppMonad m) (AppState s) = do 
--     (a, s') <- runModule m s 
--     return (a, AppState s')
--   newModuleState = AppState <$> newModuleState
--   withModule _ = withModule (Proxy :: Proxy AppStack)
--   cleanupModule (AppState s) = cleanupModule s 
-- 
-- -- | Arrow that is build over the monad stack
-- type AppWire a b = GameWire AppMonad a b
-- -- | Action that makes indexed app wire
-- type AppActor i a b = GameActor AppMonad i a b
-- @
--
-- There are two endpoint monads that are currently built in the core:
--
--   * 'Identity' - for modules stack that does only pure actions at it first phase;
--
--   * 'IO' - most common case, modules can execute 'IO' actions in place at firts phase.
type family ModuleStack (ms :: [* -> (* -> *) -> * -> *]) (endm :: * -> *) :: * -> * where
  ModuleStack '[] curm = curm
  ModuleStack (m ': ms) curm = ModuleStack ms (m (ModuleState curm) curm)

-- | Endpoint of state chain for Identity monad
-- 
-- Could be used in 'ModuleStack' as end monad:
--
-- @
-- type AppStack = ModuleStack [LoggingT, ActorT] Identity
-- @
data IdentityState = IdentityState deriving Generic

instance NFData IdentityState

-- | Module stack that does only pure actions in its first phase.
--
-- Could be used in 'ModuleStack' as end monad:
--
-- @
-- type AppStack = ModuleStack [LoggingT, ActorT] Identity
-- @
instance GameModule Identity IdentityState where
  type ModuleState Identity = IdentityState
  runModule i _ = return $ (runIdentity i, IdentityState)
  newModuleState = return IdentityState
  withModule _ = id
  cleanupModule _ = return ()

-- | Endpoint of state chain for IO monad.
--
-- Could be used in 'ModuleStack' as end monad:
--
-- @
-- type AppStack = ModuleStack [LoggingT, ActorT, NetworkT] IO
-- @
data IOState = IOState deriving Generic

instance NFData IOState

-- | Module stack that does IO action.
--
-- Could be used in 'ModuleStack' as end monad:
--
-- @
-- type AppStack = ModuleStack [LoggingT, ActorT, NetworkT] IO
-- @
instance GameModule IO IOState where
  type ModuleState IO = IOState
  runModule io _ = do 
    a <- liftIO io
    return (a, IOState)
  newModuleState = return IOState
  withModule _ = id
  cleanupModule _ = return ()