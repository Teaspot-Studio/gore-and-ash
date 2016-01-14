module Main where  

import Consts
import Control.DeepSeq 
import Control.Monad (join)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Proxy 
import FPS
import Game 
import Game.GoreAndAsh
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.SDL
import Game.GoreAndAsh.Sync
import Network.BSD (getHostByName, hostAddress)
import Network.Socket (SockAddr(..))
import System.Environment
import Text.Read 

import Linear (V4(..))

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
main = withModule (Proxy :: Proxy AppMonad) $ do
  gs <- newGameState mainWire
  (host, port) <- liftIO parseArgs
  fps <- makeFPSBounder 60
  firstLoop fps host port gs 
  where 
    -- | Resolve given hostname and port
    getAddr s p = do
      he <- getHostByName s
      return $ SockAddrInet p $ hostAddress he

    firstLoop fps host port gs = do 
      (_, gs') <- stepGame gs $ do 
        networkSetDetailedLoggingM False
        syncSetLoggingM True
        syncSetRoleM SyncSlave
        networkBind Nothing 1 2 0 0
        addr <- liftIO $ getAddr host (fromIntegral port)
        _ <- networkConnect addr 2 0
        _ <- sdlCreateWindowM mainWindowName "Gore&Ash Client" defaultWindow defaultRenderer
        sdlSetBackColor mainWindowName $ V4 200 200 200 255
      gameLoop fps gs'

    gameLoop fps gs = do
      waitFPSBound fps 
      (mg, gs') <- stepGame gs (return ())
      mg `deepseq` if fromMaybe False $ gameExit <$> join mg
        then cleanupGameState gs'
        else gameLoop fps gs'

{-
main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = not (null (filter eventIsQPress events))
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  unless qPressed (appLoop renderer) -}