{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.GLFW.State(
    KeyChannel
  , ButtonChannel
  , MouseChannel
  , WindowSizeChannel
  , GLFWState(..)
  ) where

import Control.Concurrent.STM
import Control.DeepSeq
import Data.Hashable
import GHC.Generics (Generic)
import Graphics.UI.GLFW
import qualified Data.HashMap.Strict as M 

-- | Channel to connect core and callback with key states
type KeyChannel = TChan (Key, KeyState, ModifierKeys)
-- | Channel to connect core and callback with mouse button states
type ButtonChannel = TChan (MouseButton, MouseButtonState, ModifierKeys)
-- | Channel to connect core and callback with mouse position
type MouseChannel = TVar (Double, Double)
-- | Channel to connect core and callback with window resizing 
type WindowSizeChannel = TVar (Maybe (Double, Double))

-- | Module inner state
data GLFWState s = GLFWState {
  glfwNextState :: !s 
, glfwKeys :: !(M.HashMap Key (KeyState, ModifierKeys))
, glfwKeyChannel :: !KeyChannel
, glfwMouseButtons :: !(M.HashMap MouseButton (MouseButtonState, ModifierKeys))
, glfwMouseButtonChannel :: !ButtonChannel
, glfwMousePos :: !(Double, Double)
, glfwMousePosChannel :: !MouseChannel
, glfwWindow :: !(Maybe Window)
, glfwPrevWindow :: !(Maybe Window)
, glfwWindowSize :: !(Maybe (Double, Double))
, glfwWindowSizeChannel :: !WindowSizeChannel
} deriving (Generic)

instance NFData s => NFData (GLFWState s) where 
  rnf GLFWState {..} = 
    glfwNextState `deepseq` 
    glfwKeys `deepseq` 
    glfwKeyChannel `seq` 
    glfwMouseButtons `deepseq` 
    glfwMouseButtonChannel `seq` 
    glfwMousePos `deepseq`
    glfwMousePosChannel `seq`
    glfwWindow `seq`
    glfwPrevWindow `seq` 
    glfwWindowSize `deepseq` ()

instance Hashable Key 
instance Hashable MouseButton
instance NFData Key 
instance NFData KeyState 
instance NFData ModifierKeys
instance NFData MouseButton
instance NFData MouseButtonState