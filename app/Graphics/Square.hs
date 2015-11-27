module Graphics.Square(
    Square(..)
  , ViewContext(..)
  , AspectUniform
  , newSquare
  , updateSquare
  , drawSquare
  ) where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class 
import Graphics.Camera
import Graphics.GPipe 
import Math

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

-- | Convert 2d point into 3d using aspect, view and model matrices
-- Project matrix is built in
make3d :: Floating a => a -> M44 a -> M44 a -> V2 a -> V4 a
make3d aspect viewMat modelMat (V2 x y) = 
  projMat !* (viewMat !* (modelMat !* V4 x y 0 1))
  where       
    projMat = ortho (-aspect) aspect (-1) 1 1 (-1)
