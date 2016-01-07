module Main where 

import Control.Exception
import Control.Monad.IO.Class
import Data.Proxy 
import Game 
import Game.Core
import Game.GoreAndAsh
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Network
import Network.BSD (getHostByName, hostAddress)
import Network.Socket (SockAddr(..))
import System.Exit
import Data.IORef 
import System.Environment
import Text.Read 

import FPS 

simulationFPS :: Int 
simulationFPS = 60 

parseArgs :: IO (String, Int)
parseArgs = do 
  args <- getArgs 
  case args of 
    [h, p] -> case readMaybe p of 
      Nothing -> fail "Failed to parse port"
      Just pint -> return (h, pint)
    _ -> fail "Misuse of arguments: gore-and-ash-server HOST PORT"

main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ do
  gs <- newGameState $ runActor' mainWire
  gsRef <- newIORef gs
  (host, port) <- liftIO parseArgs
  fps <- makeFPSBounder simulationFPS
  firstStep fps host port gs gsRef `onCtrlC` exitHandler gsRef
  where
    -- | What to do on emergency exit
    exitHandler gsRef = do 
      gs <- readIORef gsRef 
      cleanupGameState gs
      exitSuccess

    -- | Resolve given hostname and port
    getAddr s p = do
      he <- getHostByName s
      return $ SockAddrInet p $ hostAddress he

    -- | Initialization step
    firstStep fps host port gs gsRef = do 
      (_, gs') <- stepGame gs $ do 
        networkSetDetailedLoggingM False
        addr <- liftIO $ getAddr host (fromIntegral port)
        networkBind (Just addr) 100 2 0 0
      writeIORef gsRef gs'
      gameLoop fps gs' gsRef

    -- | Normal game loop
    gameLoop fps gs gsRef = do 
      waitFPSBound fps
      (_, gs') <- stepGame gs (return ())
      writeIORef gsRef gs'
      gameLoop fps gs' gsRef

-- | Executes given handler on Ctrl-C pressing
onCtrlC :: IO a -> IO () -> IO a
p `onCtrlC` q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
  where
    isUserInterrupt :: AsyncException -> Maybe ()
    isUserInterrupt UserInterrupt = Just ()
    isUserInterrupt _             = Nothing