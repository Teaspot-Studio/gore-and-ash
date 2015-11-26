{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TypeFamilies #-}    
module Main where  
   
import Graphics.GPipe    
import Graphics.GPipe.Context.GLFW.Unsafe
import qualified Graphics.GPipe.Context.GLFW as GLFW    
import qualified Graphics.UI.GLFW as GLFW (getWindowSize)
import Control.Lens  
import Control.Monad (unless)
import Control.Monad.IO.Class 

data ViewContext = ViewContext {
  viewArray :: PrimitiveArray Triangles (B2 Float)
, viewPort :: ViewPort
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


drawSquare :: 
     AspectUniform os -- ^ Buffer with aspect ration
  -> Square os -- ^ Square with uniform buffers
  -> Shader os (ContextFormat RGBFloat ()) ViewContext ()
  -- ^ Resulting shader
drawSquare aspectBuffer Square{..} = do 
  -- | Load uniforms
  aspect <- getUniform (const (aspectBuffer,0))
  modelMtx <- getModelMtx squareModelMtxUniform
  color <- getUniform (const (squareColorUniform, 0))

  -- | Convert vertecies
  primitiveStream <- toPrimitiveStream viewArray  
  let primitiveStream2 = fmap (\pos2d -> (make3d aspect modelMtx pos2d, pos2d)) primitiveStream  
  fragmentStream <- rasterize (\ViewContext{..} -> (FrontAndBack, viewPort, DepthRange 0 1)) primitiveStream2  

  -- | Colorize vertecies
  let fragmentStream2 = fmap (const color) fragmentStream  
  drawContextColor (const (ContextColorOption NoBlending (pure True))) fragmentStream2       
  where 
    getModelMtx ::
         Buffer os (Uniform (B4 Float))
      -> Shader os (ContextFormat RGBFloat ()) ViewContext (M44 VFloat)
    getModelMtx b = V4 
      <$> getUniform (const (b, 0))
      <*> getUniform (const (b, 1))
      <*> getUniform (const (b, 2))
      <*> getUniform (const (b, 3))

main :: IO ()
main =    
  runContextT GLFW.newContext (ContextFormatColor RGB8) $ do    
    aspectBuffer :: Buffer os (Uniform (B Float)) <- newBuffer 1  
    square <- newSquare 
    shader <- compileShader $ drawSquare aspectBuffer square

    renderLoop aspectBuffer square $ \viewport -> do    
      clearContextColor 0.5   
      vertexArray <- newVertexArray $ squareBuffer square    
      shader $ ViewContext {
          viewArray = toPrimitiveArray TriangleStrip vertexArray
        , viewPort = viewport
        }

make3d :: Floating a => a -> M44 a -> V2 a -> V4 a
make3d aspect modelMat (V2 x y) = 
  projMat !* (viewMat !* (modelMat !* V4 x y 0 1))
  where       
    viewMat = cameraMat (V2 0 0) 0 0
    projMat = ortho (-aspect) aspect (-1) 1 1 (-1)

renderLoop :: Buffer os (Uniform (B Float)) -> Square os -> (ViewPort -> Render os f ()) -> ContextT GLFW.GLFWWindow os f IO ()
renderLoop aspectBuffer square rendering = do
  square' <- updateSquare 1 0 (squareRot square + 0.01) (V3 1 0 0) square
  (w, h) <- withContextWindow $ GLFW.getWindowSize . getGLFWWindow
  let viewport = ViewPort (V2 0 0) (V2 w h)
  writeBuffer aspectBuffer 0 [fromIntegral w / fromIntegral h]
  render $ rendering viewport
  swapContextBuffers    
  closeRequested <- GLFW.windowShouldClose    
  unless closeRequested $    
    renderLoop aspectBuffer square' rendering  

-- | 2D camera matrix
cameraMat :: Floating a => V2 a -> a -> a -> V4 (V4 a)
cameraMat eye dist ang = 
  V4 (V4 (cos ang) (- sin ang) 0 (-eye^._x))
     (V4 (sin ang) (  cos ang) 0 (-eye^._y))
     (V4 0         0           1 (-dist))
     (V4 0 0 0 1)