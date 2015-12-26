{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.GLFW.API(
    MonadGLFWInput(..)
  -- | Arrow API
  , keyStatus
  , keyStatusDyn
  -- | Helpers
  , keyPressed
  , keyPressedDyn
  , keyReleased
  , keyReleasedDyn
  , keyRepeating
  , keyRepeatingDyn
  -- | Reexports
  , Key(..)
  , KeyState(..)
  , ModifierKeys(..)
  ) where

import Prelude hiding (id, (.))
import Control.Wire 

import Control.Monad.State.Strict 
import Control.Wire.Unsafe.Event
import Graphics.UI.GLFW
import qualified Data.HashMap.Strict as M 

import Game.GoreAndAsh
import Game.GoreAndAsh.GLFW.State
import Game.GoreAndAsh.GLFW.Module 

-- | Module low-level API
class Monad m => MonadGLFWInput m where 
  keyStatusM :: Key -> m (Maybe (KeyState, ModifierKeys))

instance Monad m => MonadGLFWInput (GLFWInputT s m) where 
  keyStatusM k = do 
    GLFWState{..} <- GLFWInputT get
    return $ M.lookup k glfwKeys

instance MonadGLFWInput m => MonadGLFWInput (GameMonadT m) where 
  keyStatusM = lift . keyStatusM

-- | Produces event when key state changes
keyStatus :: MonadGLFWInput m => Key -> GameWire m a (Event (KeyState, ModifierKeys))
keyStatus k = liftGameMonad (maybe2Event <$> keyStatusM k)

-- | Produces event when key state changes, get key as arrow argument
keyStatusDyn :: MonadGLFWInput m => GameWire m Key (Event (KeyState, ModifierKeys))
keyStatusDyn = liftGameMonad1 $ \k -> do 
  ms <- keyStatusM k 
  return $ maybe2Event ms 

-- | Simple transform from maybe to event
maybe2Event :: Maybe a -> Event a 
maybe2Event Nothing = NoEvent 
maybe2Event (Just a) = Event a

keyStated :: MonadGLFWInput m => KeyState -> Key -> GameWire m a (Event ModifierKeys)
keyStated ks k = mapE snd . filterE (\(ks', _) -> ks' == ks) . keyStatus k

keyStatedDyn :: MonadGLFWInput m => KeyState -> GameWire m Key (Event ModifierKeys)
keyStatedDyn ks = mapE snd . filterE (\(ks', _) -> ks' == ks) . keyStatusDyn

keyPressed :: MonadGLFWInput m => Key -> GameWire m a (Event ModifierKeys)
keyPressed = keyStated KeyState'Pressed

keyPressedDyn :: MonadGLFWInput m => GameWire m Key (Event ModifierKeys)
keyPressedDyn = keyStatedDyn KeyState'Pressed

keyReleased :: MonadGLFWInput m => Key -> GameWire m a (Event ModifierKeys)
keyReleased = keyStated KeyState'Released

keyReleasedDyn :: MonadGLFWInput m => GameWire m Key (Event ModifierKeys)
keyReleasedDyn = keyStatedDyn KeyState'Released

keyRepeating :: MonadGLFWInput m => Key -> GameWire m a (Event ModifierKeys)
keyRepeating = keyStated KeyState'Repeating

keyRepeatingDyn :: MonadGLFWInput m => GameWire m Key (Event ModifierKeys)
keyRepeatingDyn = keyStatedDyn KeyState'Repeating