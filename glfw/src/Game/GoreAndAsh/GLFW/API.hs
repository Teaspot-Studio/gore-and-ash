{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.GLFW.API(
    MonadGLFWInput(..)
  -- | Arrow API
  , keyStatus
  , keyStatusDyn
  , mouseButton
  , mouseButtonDyn
  -- | Helpers
  , keyPressed
  , keyPressedDyn
  , keyReleased
  , keyReleasedDyn
  , keyRepeating
  , keyRepeatingDyn
  , mouseButtonPressed
  , mouseButtonPressedDyn
  , mouseButtonReleased
  , mouseButtonReleasedDyn
  -- | Reexports
  , Key(..)
  , KeyState(..)
  , MouseButton(..)
  , MouseButtonState(..)
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
  -- | Returns state of given keyboard's key
  keyStatusM :: Key -> m (Maybe (KeyState, ModifierKeys))
  -- | Returns state of given mouse button
  mouseButtonM :: MouseButton -> m (Maybe (MouseButtonState, ModifierKeys))
  -- | Setups current window for input catch
  setCurrentWindowM :: Maybe Window -> m ()

instance Monad m => MonadGLFWInput (GLFWInputT s m) where 
  keyStatusM k = do 
    GLFWState{..} <- GLFWInputT get
    return $ M.lookup k glfwKeys

  mouseButtonM b = do 
    GLFWState{..} <- GLFWInputT get 
    return $ M.lookup b glfwMouseButtons

  setCurrentWindowM w = GLFWInputT $ do 
    s <- get 
    put $ s { 
        glfwWindow = w 
      , glfwPrevWindow = glfwWindow s 
      }

instance MonadGLFWInput m => MonadGLFWInput (GameMonadT m) where 
  keyStatusM = lift . keyStatusM
  mouseButtonM = lift . mouseButtonM
  setCurrentWindowM = lift . setCurrentWindowM

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

-- | Fires when keyboard key is pressed
keyPressed :: MonadGLFWInput m => Key -> GameWire m a (Event ModifierKeys)
keyPressed = keyStated KeyState'Pressed

-- | Version of keyPressed that takes key at runtime
keyPressedDyn :: MonadGLFWInput m => GameWire m Key (Event ModifierKeys)
keyPressedDyn = keyStatedDyn KeyState'Pressed

-- | Fires when keyboard key is released
keyReleased :: MonadGLFWInput m => Key -> GameWire m a (Event ModifierKeys)
keyReleased = keyStated KeyState'Released

-- | Version of keyReleased that takes key at runtime
keyReleasedDyn :: MonadGLFWInput m => GameWire m Key (Event ModifierKeys)
keyReleasedDyn = keyStatedDyn KeyState'Released

-- | Fires when keyboard key is entered into repeating mode
keyRepeating :: MonadGLFWInput m => Key -> GameWire m a (Event ModifierKeys)
keyRepeating = keyStated KeyState'Repeating

-- | Version of keyRepeating that takes key at runtime
keyRepeatingDyn :: MonadGLFWInput m => GameWire m Key (Event ModifierKeys)
keyRepeatingDyn = keyStatedDyn KeyState'Repeating

-- | Produces event when mouse button state changes
mouseButton :: MonadGLFWInput m => MouseButton -> GameWire m a (Event (MouseButtonState, ModifierKeys))
mouseButton k = liftGameMonad (maybe2Event <$> mouseButtonM k)

-- | Produces event when key state changes, get key as arrow argument
mouseButtonDyn :: MonadGLFWInput m => GameWire m MouseButton (Event (MouseButtonState, ModifierKeys))
mouseButtonDyn = liftGameMonad1 $ \k -> do 
  ms <- mouseButtonM k 
  return $ maybe2Event ms 

mouseButtonStated :: MonadGLFWInput m => MouseButtonState -> MouseButton -> GameWire m a (Event ModifierKeys)
mouseButtonStated bs b = mapE snd . filterE (\(bs', _) -> bs == bs') . mouseButton b

mouseButtonStatedDyn :: MonadGLFWInput m => MouseButtonState -> GameWire m MouseButton (Event ModifierKeys)
mouseButtonStatedDyn bs = mapE snd . filterE (\(bs', _) -> bs == bs') . mouseButtonDyn

-- | Fires when mouse button is pressed
mouseButtonPressed :: MonadGLFWInput m => MouseButton -> GameWire m a (Event ModifierKeys)
mouseButtonPressed = mouseButtonStated MouseButtonState'Pressed 

-- | Version of mouseButtonPressed that takes button at runtime
mouseButtonPressedDyn :: MonadGLFWInput m => GameWire m MouseButton (Event ModifierKeys)
mouseButtonPressedDyn = mouseButtonStatedDyn MouseButtonState'Pressed

-- | Fires when mouse button is released
mouseButtonReleased :: MonadGLFWInput m => MouseButton -> GameWire m a (Event ModifierKeys)
mouseButtonReleased = mouseButtonStated MouseButtonState'Released 

-- | Version of mouseButtonReleased that takes button at runtime
mouseButtonReleasedDyn :: MonadGLFWInput m => GameWire m MouseButton (Event ModifierKeys)
mouseButtonReleasedDyn = mouseButtonStatedDyn MouseButtonState'Released