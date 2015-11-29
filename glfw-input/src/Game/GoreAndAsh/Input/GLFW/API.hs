{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Input.GLFW.API(
    MonadGLFWInput(..)
  -- | Arrow API
  , keyStatus
  , keyStatusDyn
  ) where

import Control.Monad.IO.Class
import Control.Monad.State.Strict 
import Control.Wire.Unsafe.Event
import Graphics.UI.GLFW
import qualified Data.HashMap.Strict as M 

import Game.GoreAndAsh
import Game.GoreAndAsh.Input.GLFW.State
import Game.GoreAndAsh.Input.GLFW.Module 

-- | Module low-level API
class Monad m => MonadGLFWInput m where 
  keyStatusM :: Key -> m (Maybe (KeyState, ModifierKeys))

instance Monad m => MonadGLFWInput (GLFWInputT s m) where 
  keyStatusM k = do 
    GLFWState{..} <- GLFWInputT get
    return $ M.lookup k glfwKeys

instance MonadGLFWInput m => MonadGLFWInput (GameMonadT m) where 
  keyStatusM = lift . keyStatusM

instance MonadTrans (GLFWInputT s) where
  lift = GLFWInputT . lift 

instance MonadIO m => MonadIO (GLFWInputT s m) where 
  liftIO = GLFWInputT . liftIO 

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