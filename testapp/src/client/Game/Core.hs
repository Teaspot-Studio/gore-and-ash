module Game.Core(
    AppMonad(..)
  , AppWire
  , AppActor
  ) where

import Control.DeepSeq
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Wire
import Data.Proxy 
import Game.GoreAndAsh
import GHC.Generics (Generic)
import Prelude hiding (id, (.))

import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.SDL 
import Game.GoreAndAsh.Sync 

-- | Application monad is monad stack build from given list of modules over base monad (IO)
type AppStack = ModuleStack [LoggingT, ActorT, NetworkT, SyncT, SDLT] IO
newtype AppState = AppState (ModuleState AppStack)
  deriving (Generic)

instance NFData AppState 

-- | Wrapper around type family
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, LoggingMonad, NetworkMonad, ActorMonad, MonadCatch, MonadThrow, MonadSDL)

-- | Current GHC (7.10.3) isn't able to derive this
instance SyncMonad AppMonad where 
  getSyncIdM = AppMonad . getSyncIdM
  getSyncTypeRepM = AppMonad . getSyncTypeRepM
  registerSyncIdM = AppMonad . registerSyncIdM
  addSyncTypeRepM a b = AppMonad $ addSyncTypeRepM a b
  syncScheduleMessageM peer ch i mt msg  = AppMonad $ syncScheduleMessageM peer ch i mt msg
  syncSetLoggingM = AppMonad . syncSetLoggingM
  syncSetRoleM = AppMonad . syncSetRoleM
  syncGetRoleM = AppMonad syncGetRoleM
  syncRequestIdM a b = AppMonad $ syncRequestIdM a b 
  
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
-- | App actor that is monadic action to create indexed wire
type AppActor i a b = GameActor AppMonad i a b 