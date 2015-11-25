{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TypeFamilies #-}    
module Main where  
   
import Graphics.GPipe    
import Graphics.GPipe.Context.GLFW.Unsafe
import qualified Graphics.GPipe.Context.GLFW as GLFW    
import qualified Graphics.UI.GLFW as GLFW (getWindowSize)
import Control.Lens  
import Control.Monad (unless)

data ViewContext = ViewContext {
  viewArray :: PrimitiveArray Triangles (B2 Float)
, viewPort :: ViewPort
}

main :: IO ()
main =    
  runContextT GLFW.newContext (ContextFormatColor RGB8) $ do    
    vertexBuffer :: Buffer os (B2 Float) <- newBuffer 4    
    writeBuffer vertexBuffer 0 [V2 (-1) (-1), V2 1 (-1), V2 (-1) 1, V2 1 1] 
    
    aspectBuffer :: Buffer os (Uniform (B Float)) <- newBuffer 1  

    shader <- compileShader $ do    
      aspect <- getUniform (const (aspectBuffer,0))
      primitiveStream <- toPrimitiveStream viewArray  
      let primitiveStream2 = fmap (\pos2d -> (make3d aspect pos2d, pos2d)) primitiveStream  
      fragmentStream <- rasterize (\ViewContext{..} -> (FrontAndBack, viewPort, DepthRange 0 1)) primitiveStream2  

      let red = V3 1.0 0.0 0.0
          fragmentStream2 = fmap (const red) fragmentStream  
      drawContextColor (const (ContextColorOption NoBlending (pure True))) fragmentStream2       

    renderLoop aspectBuffer $ \viewport -> do    
      clearContextColor 0.5   
      vertexArray <- newVertexArray vertexBuffer    
      shader $ ViewContext {
          viewArray = toPrimitiveArray TriangleStrip vertexArray
        , viewPort = viewport
        }

make3d :: Floating a => a -> V2 a -> V4 a
make3d aspect (V2 x y) = 
  projMat !* (viewMat !* V4 x y 0 1)  
  where       
    viewMat = cameraMat (V2 0 0) 0 0
    projMat = ortho (-aspect) aspect (-1) 1 1 (-1)

renderLoop :: Buffer os (Uniform (B Float)) -> (ViewPort -> Render os f ()) -> ContextT GLFW.GLFWWindow os f IO ()
renderLoop aspectBuffer rendering = do  
  (w, h) <- withContextWindow $ GLFW.getWindowSize . getGLFWWindow
  let viewport = ViewPort (V2 0 0) (V2 w h)
  writeBuffer aspectBuffer 0 [fromIntegral w / fromIntegral h]
  render $ rendering viewport
  swapContextBuffers    
  closeRequested <- GLFW.windowShouldClose    
  unless closeRequested $    
    renderLoop aspectBuffer rendering  

-- | 2D camera matrix
cameraMat :: Floating a => V2 a -> a -> a -> V4 (V4 a)
cameraMat eye dist ang = 
  V4 (V4 (cos ang) (- sin ang) 0 (-eye^._x))
     (V4 (sin ang) (  cos ang) 0 (-eye^._y))
     (V4 0         0           1 (-dist))
     (V4 0 0 0 1)

-- Copy of lookAt from linear with normalize replaced with signorm   
--lookAt' :: Floating a => V3 a -> V3 a -> V3 a -> V4 (V4 a)
--lookAt' eye center up =  
--  V4 (V4 (xa^._x) (xa^._y) (xa^._z) xd)  
--     (V4 (ya^._x) (ya^._y) (ya^._z) yd)  
--     (V4 (-za^._x) (-za^._y) (-za^._z) zd)  
--     (V4 0     0     0     1)  
--  where za = signorm $ center - eye  
--        xa = signorm $ cross za up  
--        ya = cross xa za  
--        xd = -dot xa eye  
--        yd = -dot ya eye  
--        zd = dot za eye     