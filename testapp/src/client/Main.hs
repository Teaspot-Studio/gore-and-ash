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

import Data.Vec ((:.)(..), Vec3, Vec4)

import qualified Data.Vector.Storable as V

import qualified Graphics.Rendering.OpenGL.GL as GL

import Andromeda.Simple.Expr
import Andromeda.Simple.Type
import Andromeda.Simple.StdLib
import Andromeda.Simple.GLSL

import Andromeda.Simple.Render.Mesh
import Andromeda.Simple.Render.VertexBuffer
import Andromeda.Simple.Render.Compile

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
    win <- openWindow
    initGL win

    putStrLn $ toGLSL vertShader
    putStrLn $ toGLSL fragShader
    prog <- addMesh myMesh =<< compile vertShader fragShader

    mainLoop win prog

glPosition :: Expr (Vec4 Float)
glPosition = fetch "vertex" (Vec3T SFloat) +: 1

vertShader :: Statement ()
vertShader = do
    "gl_Position" =: glPosition
    out "fragPos" glPosition

outColor :: Expr (Vec3 Float)
outColor =
    let fragPos = fetch   "fragPos" (Vec4T SFloat)
        inColor = uniform "inColor" (Vec3T SFloat)
    in vzipWith avg inColor fragPos
  where
    avg x y = (x + y) / 2

fragShader :: Statement ()
fragShader = out "color" outColor

myMesh :: Mesh
myMesh = Mesh
    [("vertex",  MeshAttribute $ V.fromList triangle),
     ("inColor", MeshUniform   $ Uniform color)] Triangles

triangle :: [Vec3 Float]
triangle = [(-1):.(-1):.0:.(),
              1 :.(-1):.0:.(),
              0 :.  1 :.0:.()]

color :: GL.Vertex3 GL.GLfloat
color = GL.Vertex3 0.5 0.5 0.5