{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.Input.GLFW.Module(
    GLFWState
  , GLFWInputT
  , MonadGLFWInput(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Extra
import Control.Monad.Fix 
import Control.Monad.IO.Class
import Control.Monad.State.Strict 
import Graphics.UI.GLFW
import qualified Data.HashMap.Strict as M 

import Game.GoreAndAsh
import Game.GoreAndAsh.Input.GLFW.State

-- | Monad transformer that handles input processing
newtype GLFWInputT s m a = GLFWInputT { _runGLFWInputT :: StateT (GLFWState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (GLFWState s), MonadFix)

instance GameModule m s => GameModule (GLFWInputT s m) (GLFWState s) where 
  runModule (GLFWInputT m) s = do
    ((a, s'), nextState) <- runModule (runStateT m s) (glfwNextState s)
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
    _ <- liftIO $ forkIO $ binder kc
    return $ GLFWState {
        glfwNextState = s
      , glfwKeyChannel = kc
      , glfwKeys = M.empty
      }

-- | Thread that changes callbacks to current window
binder :: KeyChannel -> IO ()
binder kch = go Nothing 
  where 
    go mw = do 
      mw' <- getCurrentContext
      unless (mw == mw') $ do 
        whenJust mw  $ \w -> setKeyCallback w Nothing
        whenJust mw' $ \w -> bindKeyListener kch w
        yield >> go mw'
      yield >> go mw

-- | Bind callback that passes values to channel
bindKeyListener :: KeyChannel -> Window -> IO ()
bindKeyListener kch w = setKeyCallback w (Just f)
  where
    f :: Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
    f _ k _ ks mds = atomically $ writeTChan kch (k, ks, mds)

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

-- | Helper function to read all elements from channel
readAllChan :: TChan a -> STM [a]
readAllChan chan = fmap reverse $ go []
  where
    go acc = do
      mc <- tryReadTChan chan
      case mc of 
        Nothing -> return acc
        Just a -> go (a:acc)
