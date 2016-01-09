module Main where  

import Control.Monad (join)
import Control.Monad.IO.Class
import Data.Proxy 
import Game 
import Game.GoreAndAsh
import Game.GoreAndAsh.GLFW
import Game.GoreAndAsh.Network
import Graphics
import Network.BSD (getHostByName, hostAddress)
import Network.Socket (SockAddr(..))
import Render 
import System.Environment
import Text.Read 

import FPS 

gameFPS :: Int 
gameFPS = 60 

parseArgs :: IO (String, Int)
parseArgs = do 
  args <- getArgs 
  case args of 
    [h, p] -> case readMaybe p of 
      Nothing -> fail "Failed to parse port"
      Just pint -> return (h, pint)
    _ -> fail "Misuse of arguments: gore-and-ash-client HOST PORT"

main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ runWindow $ do
  rs <- addSquare =<< initResources
  gs <- newGameState mainWire
  (host, port) <- liftIO parseArgs
  fps <- liftIO $ makeFPSBounder gameFPS
  firstLoop fps host port rs gs 
  where 
    -- | Resolve given hostname and port
    getAddr s p = do
      he <- getHostByName s
      return $ SockAddrInet p $ hostAddress he

    firstLoop fps host port rs gs = do 
      (_, gs') <- stepGame gs $ do 
        networkSetDetailedLoggingM False
        networkBind Nothing 1 2 0 0
        setBufferSizeM 2
        addr <- liftIO $ getAddr host (fromIntegral port)
        networkConnect addr 2 0
      gameLoop fps rs gs'

    gameLoop fps rs gs = do 
      liftIO $ putStrLn "gl start"
      --liftIO $ waitFPSBound fps
      liftIO $ putStrLn "gl step"
      (mg, gs') <- stepGame gs (preFrame rs)
      liftIO $ putStrLn "gl step done, render"
      rs2 <- case join mg of 
        Nothing -> renderEmptyScreen rs
        Just g -> renderGame g =<< stepRenderState rs
      liftIO $ putStrLn "gl render done"
      if isClosedRequest rs2 
        then cleanupGameState gs'
        else gameLoop fps rs2 gs'

    preFrame rs = do 
      setCurrentWindowM $ renderWindow rs