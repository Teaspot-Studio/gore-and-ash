module Game.GoreAndAsh.Input.GLFW.Module(
    GLFWState
  , GLFWInputT
  , MonadGLFWInput(..)
  -- | Arrow API
  , keyStatus
  , keyStatusDyn
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.DeepSeq
import Control.Monad.Extra
import Control.Monad.Fix 
import Control.Monad.IO.Class
import Control.Monad.State.Strict 
import Data.Hashable
import GHC.Generics (Generic)
import Graphics.UI.GLFW
import qualified Data.HashMap.Strict as M 

import Game.GoreAndAsh
-- import Control.Wire 
import Control.Wire.Unsafe.Event

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

-- | Monad transformer that handles input processing
newtype GLFWInputT s m a = GLFWInputT { runGLFWInputT :: StateT (GLFWState s) m a }
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