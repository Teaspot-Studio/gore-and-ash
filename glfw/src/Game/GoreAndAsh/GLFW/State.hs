{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.GLFW.State(
    KeyChannel
  , GLFWState(..)
  ) where

import Control.Concurrent.STM
import Control.DeepSeq
import Data.Hashable
import GHC.Generics (Generic)
import Graphics.UI.GLFW
import qualified Data.HashMap.Strict as M 

-- | Channel to connect core and callback
type KeyChannel = TChan (Key, KeyState, ModifierKeys)

-- | Module inner state
data GLFWState s = GLFWState {
  glfwNextState :: !s 
, glfwKeys :: !(M.HashMap Key (KeyState, ModifierKeys))
, glfwKeyChannel :: !KeyChannel
, glfwWindow :: !(Maybe Window)
, glfwPrevWindow :: !(Maybe Window)
} deriving (Generic)

instance NFData s => NFData (GLFWState s) where 
  rnf GLFWState {..} = 
    glfwNextState `deepseq` 
    glfwKeys `deepseq` 
    glfwKeyChannel `seq` 
    glfwWindow `seq`
    glfwPrevWindow `seq` ()

instance Hashable Key 
instance NFData ModifierKeys
instance NFData KeyState 
instance NFData Key 