module Game.GoreAndAsh.Core.Monad(
    GameMonadT
  , GameContext(..)
  , newGameContext
  , evalGameMonad
  , GameModule(..)
  , IOState
  , IdentityState
  , ModuleStack(..)
  ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Functor.Identity
import Data.Proxy (Proxy)
import GHC.Generics (Generic)

-- | Basic game monad transformer
-- Here goes all core API that accessable from each 
-- game object. All specific (mods etc) API should
-- be included in inner `m` monad.
newtype GameMonadT m a = GameMonadT { 
  runGameMonadT :: StateT GameContext m a
}

-- | Data that is accessable to objects during
-- game simulation step.
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

-- | Describes how to run game modules
-- @GameMonadT@ has @m@ parameter that should implement the class.
-- The class describes how the module is executed each game frame
-- and how to pass its own state to the next state.
class Monad m => GameModule m s | m -> s, s -> m where
  -- | Defines what state has given module
  type ModuleState m :: *
  -- | Executes module action with given state
  -- Produces new state that should be passed to next step
  runModule :: MonadIO m' => m a -> s -> m' (a, s)
  -- | Creates new state of module
  newModuleState :: MonadIO m' => m' s
  -- | Wrap action with module initialization and cleanup
  -- Could be `withSocketsDo` or another external library initalization
  withModule :: Proxy m -> IO a -> IO a
  -- | Cleanup resources of the module, should be called on exit
  cleanupModule :: s -> IO ()

-- | Type level function that constucts complex module stack from given list of modules
type family ModuleStack (ms :: [* -> (* -> *) -> * -> *]) (endm :: * -> *) :: * -> * where
  ModuleStack '[] curm = curm
  ModuleStack (m ': ms) curm = ModuleStack ms (m (ModuleState curm) curm)

-- | Endpoint of state chain for Identity monad
data IdentityState = IdentityState deriving Generic

instance NFData IdentityState

-- | Module that does nothing
instance GameModule Identity IdentityState where
  type ModuleState Identity = IdentityState
  runModule i _ = return $ (runIdentity i, IdentityState)
  newModuleState = return IdentityState
  withModule _ = id
  cleanupModule _ = return ()

-- | Endpoint of state chain for IO monad
data IOState = IOState deriving Generic

instance NFData IOState

-- | Module that does IO action
instance GameModule IO IOState where
  type ModuleState IO = IOState
  runModule io _ = do 
    a <- liftIO io
    return (a, IOState)
  newModuleState = return IOState
  withModule _ = id
  cleanupModule _ = return ()