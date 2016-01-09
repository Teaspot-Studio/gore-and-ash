{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.GLFW.Module(
    GLFWInputT(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Extra
import Control.Monad.Fix 
import Control.Monad.IO.Class
import Control.Monad.State.Strict 
import Data.IORef
import Graphics.UI.GLFW
import qualified Data.HashMap.Strict as M 

import Game.GoreAndAsh
import Game.GoreAndAsh.GLFW.State

-- | Monad transformer that handles GLFW specific API
newtype GLFWInputT s m a = GLFWInputT { runGLFWInputT :: StateT (GLFWState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (GLFWState s), MonadFix, MonadThrow, MonadCatch, MonadMask)

instance GameModule m s => GameModule (GLFWInputT s m) (GLFWState s) where 
  type ModuleState (GLFWInputT s m) = GLFWState s
  
  runModule (GLFWInputT m) s = do
    ((a, s'@GLFWState{..}), nextState) <- runModule (runStateT m s) (glfwNextState s)
    bindWindow glfwPrevWindow glfwWindow glfwKeyChannel glfwMouseButtonChannel 
      glfwMousePosChannel glfwWindowSizeChannel glfwScrollChannel
    keys <- readAllKeys s'
    buttons <- readAllButtons s'
    mpos <- readMousePos s'
    wsize <- readWindowSize s'
    scroll <- readMouseScroll s'
    return (a, s' { 
        glfwKeys = keys
      , glfwMouseButtons = buttons
      , glfwMousePos = mpos
      , glfwNextState = nextState 
      , glfwWindowSize = wsize
      , glfwScroll = scroll
      })
    where 
      readAllKeys GLFWState{..} = liftIO $ do
        keys <- readAllChan glfwBufferSize glfwKeyChannel
        return $ M.fromList $ (\(k, ks, mds) -> (k, (ks, mds))) <$> keys

      readAllButtons GLFWState{..} = liftIO $ do 
        btns <- readAllChan glfwBufferSize glfwMouseButtonChannel
        return $ M.fromList $ (\(b, bs, mds) -> (b, (bs, mds))) <$> btns 

      readMousePos GLFWState{..} = liftIO $
        readIORef glfwMousePosChannel

      readWindowSize GLFWState{..} = liftIO $ 
        readIORef glfwWindowSizeChannel 

      readMouseScroll GLFWState{..} = liftIO $ 
        readAllChan glfwBufferSize glfwScrollChannel

  newModuleState = do
    s <- newModuleState 
    kc <- liftIO $ newIORef []
    mbc <- liftIO $ newIORef []
    mpc <- liftIO $ newIORef (0, 0)
    wsc <- liftIO $ newIORef Nothing
    sch <- liftIO $ newIORef []
    return $ GLFWState {
        glfwNextState = s
      , glfwKeyChannel = kc
      , glfwKeys = M.empty
      , glfwMouseButtonChannel = mbc 
      , glfwMouseButtons = M.empty
      , glfwMousePos = (0, 0)
      , glfwMousePosChannel = mpc
      , glfwWindow = Nothing
      , glfwPrevWindow = Nothing
      , glfwWindowSize = Nothing
      , glfwWindowSizeChannel = wsc
      , glfwScroll = []
      , glfwScrollChannel = sch
      , glfwBufferSize = 100
      }

  withModule _ = id
  cleanupModule _ = return ()
  
instance MonadTrans (GLFWInputT s) where
  lift = GLFWInputT . lift 

instance MonadIO m => MonadIO (GLFWInputT s m) where 
  liftIO = GLFWInputT . liftIO 

-- | Updates handlers when current window changes
bindWindow :: MonadIO m => Maybe Window -> Maybe Window 
  -> KeyChannel -> ButtonChannel -> MouseChannel -> WindowSizeChannel 
  -> ScrollChannel -> m ()
bindWindow prev cur kch mbch mpch wsch sch = unless (prev == cur) $ liftIO $ do 
  whenJust prev $ \w -> do
    setKeyCallback w Nothing
    setMouseButtonCallback w Nothing
    setCursorPosCallback w Nothing
    setWindowSizeCallback w Nothing >> atomicWriteIORef wsch Nothing
    setScrollCallback w Nothing
  whenJust cur $ \w -> do
    bindKeyListener kch w
    bindMouseButtonListener mbch w
    bindMousePosListener mpch w

    bindWindowSizeListener wsch w
    -- update window size
    (!sx, !sy) <- getWindowSize w 
    atomicWriteIORef wsch $! Just (fromIntegral sx, fromIntegral sy)

    bindScrollListener sch w 

atomicAppendIORef :: IORef [a] -> a -> IO ()
atomicAppendIORef ref a = atomicModifyIORef ref $ \as -> (a : as, ()) 

-- | Bind callback that passes keyboard info to channel
bindKeyListener :: KeyChannel -> Window -> IO ()
bindKeyListener kch w = setKeyCallback w (Just f)
  where
    f :: Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
    f _ k _ ks mds = atomicAppendIORef kch (k, ks, mds)

-- | Bind callback that passes mouse button info to channel
bindMouseButtonListener :: ButtonChannel -> Window -> IO ()
bindMouseButtonListener mbch w = setMouseButtonCallback w (Just f)
  where 
    f :: Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
    f _ b bs mds = atomicAppendIORef mbch (b, bs, mds)

-- | Bind callback that passes mouse position info to channel
bindMousePosListener :: MouseChannel -> Window -> IO ()
bindMousePosListener mpch w = setCursorPosCallback w (Just f)
  where 
    f :: Window -> Double -> Double -> IO ()
    f w' x y = do
      (sx, sy) <- getWindowSize w'
      let x' = 2 * (x / fromIntegral sx - 0.5)
          y' = 2 * (0.5 - y / fromIntegral sy)
      atomicWriteIORef mpch $! x' `seq` y' `seq` (x', y')

-- | Bind callback that passes window size info to channel
bindWindowSizeListener :: WindowSizeChannel -> Window -> IO ()
bindWindowSizeListener wsch w = setWindowSizeCallback w (Just f)
  where
    f :: Window -> Int -> Int -> IO ()
    f _ sx sy = do 
      let sx' = fromIntegral sx 
          sy' = fromIntegral sy
      atomicWriteIORef wsch . Just $! sx' `seq` sy' `seq` (sx', sy')

-- | Bind callback that passes scoll info to channel
bindScrollListener :: ScrollChannel -> Window -> IO ()
bindScrollListener sch w = setScrollCallback w (Just f)
  where 
    f :: Window -> Double -> Double -> IO ()
    f _ !sx !sy = atomicAppendIORef sch $! (sx, sy)

-- | Helper function to read all elements from channel
readAllChan :: Int -> IORef [a] -> IO [a]
readAllChan mi chan = do 
  xs <- readIORef chan 
  atomicWriteIORef chan []
  return $ take mi xs