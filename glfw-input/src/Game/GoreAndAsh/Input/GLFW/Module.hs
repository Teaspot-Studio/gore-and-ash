module Game.GoreAndAsh.Input.GLFW.Module(
    GLFWState(..)
  , GLFWInputT
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Monad.State.Strict 
import Control.Monad.Fix 

import Game.GoreAndAsh

data GLFWState s = GLFWState {
  glfwNextState :: s 
} deriving (Generic)

instance NFData s => NFData (GLFWState s)

newtype GLFWInputT m s a = GLFWInputT { runGLFWInputT :: StateT (GLFWState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (GLFWState s), MonadFix)

instance GameModule m s => GameModule (GLFWInputT m s) (GLFWState s) where 
  runModule (GLFWInputT m) s = do
    ((a, s'), nextState) <- runModule (runStateT m s) (glfwNextState s)
    return (a, s' { glfwNextState = nextState })

  newModuleState = do
    s <- newModuleState 
    return $ GLFWState {
        glfwNextState = s
      }