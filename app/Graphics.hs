module Graphics(
    RenderState
  , runWindow
  , initResources
  , stepRenderState
  , isClosedRequest
  ) where

import Control.Lens  
import Control.Monad (unless)
import Control.Monad.Exception
import Control.Monad.IO.Class 
import Graphics.GPipe    
import Graphics.GPipe.Context.GLFW.Unsafe
import qualified Graphics.GPipe.Context.GLFW as GLFW

data ViewContext = ViewContext {
  viewArray :: PrimitiveArray Triangles (B2 Float)
, viewPort :: ViewPort
}

type CameraUniform os = Buffer os (Uniform (B4 Float))

data Camera os = Camera {
  cameraViewMtxUniform :: CameraUniform os
, cameraPos :: V2 Float 
, cameraRot :: Float 
, cameraZoom :: Float
}

newCamera :: MonadIO m => ContextT w os f m (Camera os)
newCamera = do 
  vu <- newBuffer 4
  writeBuffer vu 0 [
      V4 1 0 0 0
    , V4 0 1 0 0
    , V4 0 0 1 0
    , V4 0 0 0 1 ]

  return $ Camera {
      cameraViewMtxUniform = vu 
    , cameraPos = 0 
    , cameraRot = 0 
    , cameraZoom = 1
    }

updateCamera :: MonadIO m => 
     V2 Float -- ^ Position of camera 
  -> Float -- ^ Rotation of camera
  -> Float -- ^ Zoom of camera
  -> Camera os -- ^ Current camera
  -> ContextT w os f m (Camera os) -- ^ New camera
updateCamera pos rot zm cam@Camera{..} = do 
  unless (cameraPos == pos && cameraRot == rot && cameraZoom == zm) $ do 
    let (V4 r1 r2 r3 r4) = scale (V3 zm zm zm) !*! rotationZ (-rot) !*! translate (V3 (-pos^._x) (-pos^._y) 0) 
    writeBuffer cameraViewMtxUniform 0 [r1, r2, r3, r4]
  return $ cam {
      cameraPos = pos 
    , cameraRot = rot 
    , cameraZoom = zm
    }

data Square os = Square {
  squareBuffer :: Buffer os (B2 Float)
, squareModelMtxUniform :: Buffer os (Uniform (B4 Float))
, squareColorUniform :: Buffer os (Uniform (B3 Float))
, squareWidth :: Float 
, squarePos :: V2 Float
, squareRot :: Float
, squareColor :: V3 Float 
}

newSquare :: MonadIO m => ContextT w os f m (Square os) 
newSquare = do 
  b <- newBuffer 4
  writeBuffer b 0 [V2 (-1) (-1), V2 1 (-1), V2 (-1) 1, V2 1 1]
  
  mu <- newBuffer 4
  writeBuffer mu 0 [
      V4 1 0 0 0
    , V4 0 1 0 0
    , V4 0 0 1 0
    , V4 0 0 0 1 ]

  cu <- newBuffer 1
  writeBuffer cu 0 [0]

  return Square {
      squareBuffer = b
    , squareModelMtxUniform = mu
    , squareColorUniform = cu
    , squareWidth = 1
    , squarePos = 0
    , squareRot = 0
    , squareColor = 0
    }

updateSquare :: MonadIO m => 
     Float -- ^ Square width
  -> V2 Float -- ^ Square pos
  -> Float -- ^ Square rotation
  -> V3 Float -- ^ Square color RGB
  -> Square os -- ^ current square
  -> ContextT w os f m (Square os) -- ^ New square 
updateSquare w p r c sq@Square{..} = do 
  unless (squareWidth == w && squarePos == p && squareRot == r) $ do
    let (V4 r1 r2 r3 r4) = scale (V3 w w w) !*! rotationZ r !*! translate (V3 (p^._x) (p^._y) 0) 
    writeBuffer squareModelMtxUniform 0 [r1, r2, r3, r4]
  unless (squareColor == c) $ writeBuffer squareColorUniform 0 [c]
  return $ sq {
      squareWidth = w 
    , squarePos = p 
    , squareRot = r 
    , squareColor = c
    }

scale :: Num a => V3 a -> M44 a 
scale (V3 x y z) = V4
  (V4 x 0 0 0)
  (V4 0 y 0 0)
  (V4 0 0 z 0)
  (V4 0 0 0 1)

rotationZ :: Floating a => a -> M44 a 
rotationZ a = V4 
  (V4 (cos a) (- sin a) 0 0)
  (V4 (sin a) (  cos a) 0 0)
  (V4 0 0 1 0)
  (V4 0 0 0 1)

translate :: Num a => V3 a -> M44 a 
translate (V3 x y z) = V4 
  (V4 1 0 0 x)
  (V4 0 1 0 y)
  (V4 0 0 1 z)
  (V4 0 0 0 1)

type AspectUniform os = Buffer os (Uniform (B Float))
type AppShader os a = Shader os (ContextFormat RGBFloat ()) ViewContext a

-- | Load matrix from uniform buffer (4 4-dim vectors)
loadMatrix44 ::
     Buffer os (Uniform (B4 Float))
  -> AppShader os (M44 VFloat)
loadMatrix44 b = V4 
  <$> getUniform (const (b, 0))
  <*> getUniform (const (b, 1))
  <*> getUniform (const (b, 2))
  <*> getUniform (const (b, 3))

drawSquare :: 
     AspectUniform os -- ^ Buffer with aspect ration
  -> Camera os -- ^ Camera matrix (View matrix)
  -> Square os -- ^ Square with uniform buffers (Holds Model matrix)
  -> AppShader os ()
  -- ^ Resulting shader
drawSquare aspectBuffer Camera{..} Square{..} = do 
  -- | Load uniforms
  aspect <- getUniform (const (aspectBuffer,0))
  modelMtx <- loadMatrix44 squareModelMtxUniform
  viewMtx <- loadMatrix44 cameraViewMtxUniform
  color <- getUniform (const (squareColorUniform, 0))

  -- | Convert vertecies
  primitiveStream <- toPrimitiveStream viewArray  
  let primitiveStream2 = fmap (\pos2d -> (make3d aspect viewMtx modelMtx pos2d, pos2d)) primitiveStream  
  fragmentStream <- rasterize (\ViewContext{..} -> (FrontAndBack, viewPort, DepthRange 0 1)) primitiveStream2  

  -- | Colorize vertecies
  let fragmentStream2 = fmap (const color) fragmentStream  
  drawContextColor (const (ContextColorOption NoBlending (pure True))) fragmentStream2       

make3d :: Floating a => a -> M44 a -> M44 a -> V2 a -> V4 a
make3d aspect viewMat modelMat (V2 x y) = 
  projMat !* (viewMat !* (modelMat !* V4 x y 0 1))
  where       
    projMat = ortho (-aspect) aspect (-1) 1 1 (-1)

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
  square' <- updateSquare 1 0 (squareRot renderSquare + 0.01) (V3 1 0 0) renderSquare
  camera' <- updateCamera (-1) 0 1 renderCamera
  
  -- | Calculate viewport
  size@(V2 w h) <- getContextBuffersSize
  let viewport = ViewPort (V2 0 0) size
  writeBuffer renderAspectBuffer 0 [fromIntegral w / fromIntegral h]
  
  -- | Render
  render $ rendering viewport
  swapContextBuffers    

  -- | Update state
  closeRequested <- GLFW.windowShouldClose   
  return $ rs {
      renderSquare = square'
    , renderCamera = camera'
    , renderStop = closeRequested
    }