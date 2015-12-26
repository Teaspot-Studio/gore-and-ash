{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.GLFW.Module(
    GLFWInputT(..)
  ) where

import Control.Concurrent.STM
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Fix 
import Control.Monad.State.Strict 
import Graphics.UI.GLFW
import qualified Data.HashMap.Strict as M 

import Game.GoreAndAsh
import Game.GoreAndAsh.GLFW.State

-- | Monad transformer that handles GLFW specific API
newtype GLFWInputT s m a = GLFWInputT { runGLFWInputT :: StateT (GLFWState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (GLFWState s), MonadFix)

instance GameModule m s => GameModule (GLFWInputT s m) (GLFWState s) where 
  runModule (GLFWInputT m) s = do
    ((a, s'@GLFWState{..}), nextState) <- runModule (runStateT m s) (glfwNextState s)
    bindWindow glfwPrevWindow glfwWindow glfwKeyChannel glfwMouseButtonChannel
    keys <- readAllKeys s'
    buttons <- readAllButtons s'
    return (a, s' { 
        glfwKeys = keys
      , glfwMouseButtons = buttons
      , glfwNextState = nextState 
      })
    where 
      readAllKeys GLFWState{..} = liftIO $ do
        keys <- atomically $ readAllChan glfwKeyChannel
        return $ M.fromList $ (\(k, ks, mds) -> (k, (ks, mds))) <$> keys

      readAllButtons GLFWState{..} = liftIO $ do 
        btns <- atomically $ readAllChan glfwMouseButtonChannel
        return $ M.fromList $ (\(b, bs, mds) -> (b, (bs, mds))) <$> btns 

  newModuleState = do
    s <- newModuleState 
    kc <- liftIO newTChanIO
    mbc <- liftIO newTChanIO
    return $ GLFWState {
        glfwNextState = s
      , glfwKeyChannel = kc
      , glfwKeys = M.empty
      , glfwMouseButtonChannel = mbc 
      , glfwMouseButtons = M.empty
      , glfwWindow = Nothing
      , glfwPrevWindow = Nothing
      }

instance MonadTrans (GLFWInputT s) where
  lift = GLFWInputT . lift 

instance MonadIO m => MonadIO (GLFWInputT s m) where 
  liftIO = GLFWInputT . liftIO 

-- | Updates handlers when current window changes
bindWindow :: MonadIO m => Maybe Window -> Maybe Window -> KeyChannel -> ButtonChannel -> m ()
bindWindow prev cur kch mbch = unless (prev == cur) $ liftIO $ do 
  whenJust prev  $ \w -> do
    setKeyCallback w Nothing
    setMouseButtonCallback w Nothing
  whenJust cur $ \w -> do
    bindKeyListener kch w
    bindMouseButtonListener mbch w

-- | Bind callback that passes keyboard info to channel
bindKeyListener :: KeyChannel -> Window -> IO ()
bindKeyListener kch w = setKeyCallback w (Just f)
  where
    f :: Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
    f _ k _ ks mds = atomically $ writeTChan kch (k, ks, mds)

-- | Bind callback that passes mouse button info to channel
bindMouseButtonListener :: ButtonChannel -> Window -> IO ()
bindMouseButtonListener mbch w = setMouseButtonCallback w (Just f)
  where 
    f :: Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
    f _ b bs mds = atomically $ writeTChan mbch (b, bs, mds)

-- | Helper function to read all elements from channel
readAllChan :: TChan a -> STM [a]
readAllChan chan = fmap reverse $ go []
  where
    go acc = do
      mc <- tryReadTChan chan
      case mc of 
        Nothing -> return acc
        Just a -> go (a:acc)