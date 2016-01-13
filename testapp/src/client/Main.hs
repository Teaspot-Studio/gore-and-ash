module Main where  

--import Control.Monad (join)
--import Control.Monad.IO.Class
--import Data.Proxy 
--import Game 
--import Game.GoreAndAsh
--import Game.GoreAndAsh.Network
--import Game.GoreAndAsh.Sync
--import Graphics
--import Network.BSD (getHostByName, hostAddress)
--import Network.Socket (SockAddr(..))
--import Render 
import System.Environment
import Text.Read 
import Graphics.Gloss.Interface.IO.Game

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

{-
main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ do
  gs <- newGameState mainWire
  (host, port) <- liftIO parseArgs
  firstLoop fps host port rs gs 
  where 
    -- | Resolve given hostname and port
    getAddr s p = do
      he <- getHostByName s
      return $ SockAddrInet p $ hostAddress he

    firstLoop fps host port rs gs = do 
      (_, gs') <- stepGame gs $ do 
        networkSetDetailedLoggingM False
        syncSetLoggingM True
        syncSetRoleM SyncSlave
        networkBind Nothing 1 2 0 0
        setBufferSizeM 2
        addr <- liftIO $ getAddr host (fromIntegral port)
        networkConnect addr 2 0
      gameLoop fps rs gs'

    gameLoop fps rs gs = do 
      (mg, gs') <- stepGame gs rs
      rs2 <- case join mg of 
        Nothing -> renderEmptyScreen rs
        Just g -> renderGame g =<< stepRenderState rs
      if isClosedRequest rs2 
        then cleanupGameState gs'
        else gameLoop fps rs2 gs' -}

main :: IO ()
main = do
  -- gs <- newGameState mainWire
  playIO display 0 60 () renderGame eventUpdate gameUpdate
  where
    display = InWindow "Client" (0, 0) (800, 600)
    renderGame _ = pure $ Circle 80
    eventUpdate _ = pure 
    gameUpdate _ gs = return gs
