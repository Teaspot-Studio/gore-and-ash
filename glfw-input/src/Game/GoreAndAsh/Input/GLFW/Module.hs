module Game.GoreAndAsh.Input.GLFW.Module(
    GLFWState(..)
  , GLFWInputT
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Monad.State.Strict 
import Control.Monad.Fix 

import Game.GoreAndAsh

data GLFWState = GLFWState {
  
} deriving (Generic)

instance NFData GLFWState

newtype GLFWInputT m a = GLFWInputT { runGLFWInputT :: StateT GLFWState m a }
  deriving (Functor, Applicative, Monad, MonadState GLFWState, MonadFix)