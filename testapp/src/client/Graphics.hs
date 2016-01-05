module Graphics(
    RenderState(..)
  , runWindow
  , initResources
  , addSquare
  , stepRenderState
  , renderEmptyScreen
  , isClosedRequest
  ) where

import Control.Monad (forM_)
import Control.Monad.Exception
import Control.Monad.IO.Class 
import Graphics.GPipe    
import Graphics.GPipe.Context.GLFW.Unsafe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Graphics.UI.GLFW as GLFW (Window)

import Graphics.Camera
import Graphics.Square 

-- | Starts GLFW window and performs given action with it
runWindow :: 
  (forall os . ContextT GLFW.GLFWWindow os (ContextFormat RGBFloat ()) IO ())
  -> IO ()
runWindow f = runContextT GLFW.newContext (ContextFormatColor RGB8) f

-- | Holds all data that is needed to perform render loop
data RenderState os = RenderState {
  renderAspectBuffer :: AspectUniform os
, renderSquares :: [Square os]
, renderCamera :: Camera os
, renderStop :: Bool
, renderWindow :: Maybe GLFW.Window
}

-- | Test if user wants to stop rendering
isClosedRequest :: RenderState os -> Bool 
isClosedRequest = renderStop

-- | Creates buffers, loads textures and others
initResources :: (MonadException m, MonadIO m) 
  => ContextT w os f m (RenderState os)
initResources = do 
    aspectBuffer <- newBuffer 1  
    camera <- newCamera
    return $ RenderState {
        renderAspectBuffer = aspectBuffer
      , renderSquares = [] 
      , renderCamera = camera 
      , renderStop = False
      , renderWindow = Nothing
      }

-- | Makes new square (buffers and shader)
addSquare :: (MonadException m, MonadIO m) 
  => RenderState os -> ContextT w os f m (RenderState os)
addSquare rs@RenderState{..} = do 
  square <- newSquare renderAspectBuffer renderCamera  
  return $ rs {
      renderSquares = renderSquares ++ [square]
    }

-- | Draws one frame of render state
stepRenderState :: (MonadException m, MonadIO m) 
  => RenderState os -- ^ Render state to draw
  -> ContextT GLFWWindow os (ContextFormat RGBFloat ()) m (RenderState os) -- ^ New render state
stepRenderState s@RenderState{..} = renderStep s $ \viewport -> do    
  clearContextColor 0.5
  forM_ renderSquares $ \Square{..} -> do 
    vertexArray <- newVertexArray $ squareBuffer squareBuffers    
    squareShader $ ViewContext {
        viewArray = toPrimitiveArray TriangleStrip vertexArray
      , viewPort = viewport
      }

-- | Draws empty frame
renderEmptyScreen :: (MonadException m, MonadIO m) 
  => RenderState os -- ^ Render state to draw
  -> ContextT GLFWWindow os (ContextFormat RGBFloat ()) m (RenderState os) -- ^ New render state
renderEmptyScreen s@RenderState{..} = renderStep s $ \_ -> do    
  clearContextColor 0.0

-- | Perform one step of rendering, update all buffers and draw a frame
renderStep :: (MonadException m, MonadIO m) 
  => RenderState os -- ^ State with buffers and shader
  -> (ViewPort -> Render os f ()) -- ^ How to render
  -> ContextT GLFWWindow os f m (RenderState os) -- ^ New state
renderStep rs@RenderState{..} rendering = do
  -- | Update buffers
  squares <- mapM updateSquare renderSquares
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
      renderSquares = squares
    , renderCamera = camera'
    , renderStop = closeRequested
    , renderWindow = Just win
    }