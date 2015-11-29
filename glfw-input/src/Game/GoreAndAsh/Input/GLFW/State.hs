{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Input.GLFW.State(
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
} deriving (Generic)

instance NFData s => NFData (GLFWState s) where 
  rnf GLFWState {..} = 
    glfwNextState `deepseq` 
    glfwKeys `deepseq` 
    glfwKeyChannel `seq` ()

instance Hashable Key 
instance NFData ModifierKeys
instance NFData KeyState 
instance NFData Key 