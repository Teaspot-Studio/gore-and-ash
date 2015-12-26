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
    bindWindow glfwPrevWindow glfwWindow glfwKeyChannel
    keys <- readAllKeys s'
    return (a, s' { 
        glfwKeys = keys
      , glfwNextState = nextState 
      })
    where 
      readAllKeys GLFWState{..} = liftIO $ do
        keys <- atomically $ readAllChan glfwKeyChannel
        return $ M.fromList $ (\(k, ks, mds) -> (k, (ks, mds))) <$> keys

  newModuleState = do
    s <- newModuleState 
    kc <- liftIO newTChanIO
    return $ GLFWState {
        glfwNextState = s
      , glfwKeyChannel = kc
      , glfwKeys = M.empty
      , glfwWindow = Nothing
      , glfwPrevWindow = Nothing
      }

instance MonadTrans (GLFWInputT s) where
  lift = GLFWInputT . lift 

instance MonadIO m => MonadIO (GLFWInputT s m) where 
  liftIO = GLFWInputT . liftIO 

-- | Updates handlers when current window changes
bindWindow :: MonadIO m => Maybe Window -> Maybe Window -> KeyChannel -> m ()
bindWindow prev cur kch = unless (prev == cur) $ liftIO $ do 
  whenJust prev  $ \w -> setKeyCallback w Nothing
  whenJust cur $ \w -> bindKeyListener kch w

-- | Bind callback that passes values to channel
bindKeyListener :: KeyChannel -> Window -> IO ()
bindKeyListener kch w = setKeyCallback w (Just f)
  where
    f :: Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
    f _ k _ ks mds = atomically $ writeTChan kch (k, ks, mds)

-- | Helper function to read all elements from channel
readAllChan :: TChan a -> STM [a]
readAllChan chan = fmap reverse $ go []
  where
    go acc = do
      mc <- tryReadTChan chan
      case mc of 
        Nothing -> return acc
        Just a -> go (a:acc)