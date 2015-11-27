module Game.GoreAndAsh.Core.Monad(
    GameMonadT
  , GameContext(..)
  , newGameContext
  , evalGameMonad
  , GameModule(..)
  ) where

import Control.DeepSeq
import Control.Monad.State.Strict
import Data.Functor.Identity
import GHC.Generics (Generic)

-- | Basic game monad transformer
-- Here goes all core API that accessable from each 
-- game object. All specific (mods etc) API should
-- be included in inner `m` monad.
newtype GameMonadT m a = GameMonadT { 
  runGameMonadT :: StateT GameContext m a
}

-- | Data that accessable to objects during
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

-- | Runs game monad with given context
evalGameMonad :: GameMonadT m a -> GameContext -> m (a, GameContext)
evalGameMonad (GameMonadT m) ctx = runStateT m ctx

-- | Describes how to run game modules
-- @GameMonadT@ has @m@ parameter that should implement the class.
-- The class describes how the module is executed each game frame
-- and how to pass its own state to the next state.
class Monad m => GameModule m s | m -> s, s -> m where
  -- | Executes module action with given state
  -- Produces new state that should be passed to next step
  runModule :: MonadIO m' => m a -> s -> m' (a, s)
  -- | Creates new state of module
  newModuleState :: MonadIO m' => m' s

-- | Module that does nothing
instance GameModule Identity () where
  runModule i _ = return $ (runIdentity i, ())
  newModuleState = return ()