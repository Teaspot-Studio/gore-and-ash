module Graphics(
    RenderState(..)
  , runWindow
  , initResources
  , stepRenderState
  , isClosedRequest
  ) where

import Control.Monad.Exception
import Control.Monad.IO.Class 
import Graphics.GPipe    
import Graphics.GPipe.Context.GLFW.Unsafe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Graphics.UI.GLFW as GLFW (Window)

import Graphics.Camera
import Graphics.Square 

-- ^ Starts GLFW window and performs given action with it
runWindow :: 
  (forall os . ContextT GLFW.GLFWWindow os (ContextFormat RGBFloat ()) IO ())
  -> IO ()
runWindow f = runContextT GLFW.newContext (ContextFormatColor RGB8) f

-- ^ Holds all data that is needed to perform render loop
data RenderState os = RenderState {
  renderAspectBuffer :: AspectUniform os
, renderSquare :: Square os
, renderCamera :: Camera os 
, renderShader :: ViewContext -> Render os (ContextFormat RGBFloat ()) ()
, renderStop :: Bool
, renderWindow :: Maybe GLFW.Window
}

-- ^ Test if user wants to stop rendering
isClosedRequest :: RenderState os -> Bool 
isClosedRequest = renderStop

-- ^ Creates buffers, loads textures and others
initResources :: (MonadException m, MonadIO m) 
  => ContextT w os f m (RenderState os)
initResources = do 
    aspectBuffer <- newBuffer 1  
    square <- newSquare 
    camera <- newCamera
    shader <- compileShader $ drawSquare aspectBuffer camera square
    return $ RenderState {
        renderAspectBuffer = aspectBuffer
      , renderSquare = square 
      , renderCamera = camera 
      , renderShader = shader 
      , renderStop = False
      , renderWindow = Nothing
      }

-- | Draws one frame of render state
stepRenderState :: (MonadException m, MonadIO m) 
  => RenderState os -- ^ Render state to draw
  -> ContextT GLFWWindow os (ContextFormat RGBFloat ()) m (RenderState os) -- ^ New render state
stepRenderState s@RenderState{..} = renderStep s $ \viewport -> do    
  clearContextColor 0.5   
  vertexArray <- newVertexArray $ squareBuffer renderSquare    
  renderShader $ ViewContext {
      viewArray = toPrimitiveArray TriangleStrip vertexArray
    , viewPort = viewport
    }

-- ^ Perform one step of rendering, update all buffers and draw a frame
renderStep :: (MonadException m, MonadIO m) 
  => RenderState os -- ^ State with buffers and shader
  -> (ViewPort -> Render os f ()) -- ^ How to render
  -> ContextT GLFWWindow os f m (RenderState os) -- ^ New state
renderStep rs@RenderState{..} rendering = do
  -- | Update buffers
  square' <- updateSquare renderSquare
  camera' <- updateCamera renderCamera
  
  -- | Calculate viewport
  size@(V2 w h) <- getContextBuffersSize
  let viewport = ViewPort (V2 0 0) size
  writeBuffer renderAspectBuffer 0 [fromIntegral w / fromIntegral h]
  
  -- | Render
  render $ rendering viewport
  swapContextBuffers    

  -- | Update state
  closeRequested <- GLFW.windowShouldClose   
  win <- withContextWindow $ return . getGLFWWindow
  return $ rs {
      renderSquare = square'
    , renderCamera = camera'
    , renderStop = closeRequested
    , renderWindow = Just win
    }