module Graphics.Square(
    Square(..)
  , SquareBuffers(..)
  , ViewContext(..)
  , AspectUniform
  , newSquare
  , updateSquare
  , drawSquare
  ) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.Exception
import Control.Monad.IO.Class 
import Graphics.Camera
import Graphics.GPipe 
import Math

data ViewContext = ViewContext {
  viewArray :: !(PrimitiveArray Triangles (B2 Float))
, viewPort :: !ViewPort
}

data SquareBuffers os = SquareBuffers {  
  squareBuffer :: !(Buffer os (B2 Float))
, squareModelMtxUniform :: !(Buffer os (Uniform (B4 Float)))
, squareColorUniform :: !(Buffer os (Uniform (B3 Float)))
}

data Square os = Square {
  squareBuffers :: !(SquareBuffers os)
, squareShader :: ViewContext -> Render os (ContextFormat RGBFloat ()) ()
, squareWidth :: !Float 
, squarePos :: !(V2 Float)
, squareRot :: !Float
, squareColor :: !(V3 Float)
, squareDirty :: !Bool
}

newSquare :: (MonadException m, MonadIO m) => AspectUniform os -> Camera os -> ContextT w os f m (Square os) 
newSquare aspectBuffer camera = do 
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

  let sbuffs = SquareBuffers {
        squareBuffer = b
      , squareModelMtxUniform = mu
      , squareColorUniform = cu
      }
  shader <- compileShader $ drawSquare aspectBuffer camera sbuffs

  return Square {
      squareBuffers = sbuffs
    , squareShader = shader
    , squareWidth = 1
    , squarePos = 0
    , squareRot = 0
    , squareColor = 0
    , squareDirty = False
    }

updateSquare :: MonadIO m => 
     Square os -- ^ current square
  -> ContextT w os f m (Square os) -- ^ New square 
updateSquare sq@Square{..} = do 
  when squareDirty $ do
    let (V4 r1 r2 r3 r4) = scale (V3 squareWidth squareWidth squareWidth) !*! rotationZ squareRot !*! translate (V3 (squarePos^._x) (squarePos^._y) 0) 
    writeBuffer (squareModelMtxUniform squareBuffers) 0 [r1, r2, r3, r4]
    writeBuffer (squareColorUniform squareBuffers) 0 [squareColor]
  return $ sq {
      squareDirty = False
    }

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
  -> SquareBuffers os -- ^ Square with uniform buffers (Holds Model matrix)
  -> AppShader os ()
  -- ^ Resulting shader
drawSquare aspectBuffer Camera{..} SquareBuffers{..} = do 
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

-- | Convert 2d point into 3d using aspect, view and model matrices
-- Project matrix is built in
make3d :: Floating a => a -> M44 a -> M44 a -> V2 a -> V4 a
make3d aspect viewMat modelMat (V2 x y) = 
  projMat !* (viewMat !* (modelMat !* V4 x y 0 1))
  where       
    projMat = ortho (-aspect) aspect (-1) 1 1 (-1)
