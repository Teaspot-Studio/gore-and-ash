module Game.Core(
    AppMonad(..)
  , AppWire
  ) where

import Control.DeepSeq
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Wire
import Data.Proxy 
import Game.GoreAndAsh
import GHC.Generics (Generic)
import Prelude hiding (id, (.))

import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.GLFW 
import Game.GoreAndAsh.Network

-- | Application monad is monad stack build from given list of modules over base monad (IO)
type AppStack = ModuleStack [LoggingT, GLFWInputT, NetworkT] IO
newtype AppState = AppState (ModuleState AppStack)
  deriving (Generic)

instance NFData AppState 

-- | Wrapper around type family
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

instance GameModule AppMonad AppState where 
  type ModuleState AppMonad = AppState
  runModule (AppMonad m) (AppState s) = do 
    (a, s') <- runModule m s 
    return (a, AppState s')
  newModuleState = AppState <$> newModuleState
  withModule _ = withModule (Proxy :: Proxy AppStack)
  cleanupModule (AppState s) = cleanupModule s 

-- | Arrow that is build over the monad stack
type AppWire a b = GameWire AppMonad a b

instance MonadGLFWInput AppMonad where
  keyStatusM = AppMonad . lift . keyStatusM
  mouseButtonM = AppMonad . lift . mouseButtonM
  mousePosM = AppMonad $ lift mousePosM
  mouseScrollM = AppMonad $ lift mouseScrollM
  windowSizeM = AppMonad $ lift windowSizeM
  setCurrentWindowM = AppMonad . lift . setCurrentWindowM
  
instance LoggingMonad AppMonad where 
  putMsgM = AppMonad . lift . lift . putMsgM
  putMsgLnM = AppMonad . lift . lift . putMsgLnM

instance NetworkMonad AppMonad where 
  networkBind a1 a2 a3 a4 a5 = AppMonad $ networkBind a1 a2 a3 a4 a5
  peersConnectedM = AppMonad peersConnectedM
  networkConnect a1 a2 a3 = AppMonad $ networkConnect a1 a2 a3
  networkPeersM = AppMonad networkPeersM
