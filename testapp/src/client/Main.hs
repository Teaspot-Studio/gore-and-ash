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
  firstLoop host port rs gs 
  where 
    -- | Resolve given hostname and port
    getAddr s p = do
      he <- getHostByName s
      return $ SockAddrInet p $ hostAddress he

    firstLoop host port rs gs = do 
      (_, gs') <- stepGame gs $ do 
        networkSetDetailedLoggingM False
        networkBind Nothing 1 2 0 0
        addr <- liftIO $ getAddr host (fromIntegral port)
        networkConnect addr 2 0
      gameLoop rs gs'

    gameLoop rs gs = do 
      (mg, gs') <- stepGame gs (preFrame rs)
      rs2 <- case join mg of 
        Nothing -> renderEmptyScreen rs
        Just g -> renderGame g =<< stepRenderState rs
      if isClosedRequest rs2 
        then cleanupGameState gs'
        else gameLoop rs2 gs'

    preFrame rs = do 
      setCurrentWindowM $ renderWindow rs