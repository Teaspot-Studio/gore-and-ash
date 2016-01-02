module Main where 

import Control.Exception
import Control.Monad.IO.Class
import Data.Proxy 
import Game 
import Game.GoreAndAsh
import Game.GoreAndAsh.Network
import Network.BSD (getHostByName, hostAddress)
import Network.Socket (SockAddr(..))
import System.Exit
import Data.IORef 

main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ do
  gs <- newGameState mainWire
  gsRef <- newIORef gs
  firstStep gs gsRef `onCtrlC` exitHandler gsRef
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
    firstStep gs gsRef = do 
      (_, gs') <- stepGame gs $ do 
        networkSetDetailedLoggingM False
        addr <- liftIO $ getAddr "localhost" 5556
        networkBind (Just addr) 100 2 0 0
      writeIORef gsRef gs'
      gameLoop gs' gsRef

    -- | Normal game loop
    gameLoop gs gsRef = do 
      (_, gs') <- stepGame gs (return ())
      writeIORef gsRef gs'
      gameLoop gs' gsRef

-- | Executes given handler on Ctrl-C pressing
onCtrlC :: IO a -> IO () -> IO a
p `onCtrlC` q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
  where
    isUserInterrupt :: AsyncException -> Maybe ()
    isUserInterrupt UserInterrupt = Just ()
    isUserInterrupt _             = Nothing