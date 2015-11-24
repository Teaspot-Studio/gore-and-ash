{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}    
module Main where  
   
import Graphics.GPipe    
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW    
import "lens" Control.Lens  
import Control.Monad (unless)    
import Data.Word (Word32)   

main :: IO ()
main =    
  runContextT GLFW.newContext (ContextFormatColor RGB8) $ do    
    vertexBuffer :: Buffer os (B2 Float) <- newBuffer 4    
    writeBuffer vertexBuffer 0 [V2 0 0, V2 1 0, V2 0 1, V2 1 1]  
    tex <- newTexture2D R8 (V2 8 8) 1  
    let whiteBlack = cycle [minBound,maxBound] :: [Word32]  
        blackWhite = tail whiteBlack  
    writeTexture2D tex 0 0 (V2 8 8) (cycle (take 8 whiteBlack ++ take 8 blackWhite))   
    shader <- compileShader $ do    
      primitiveStream <- toPrimitiveStream id  
      let primitiveStream2 = fmap (\pos2d -> (make3d pos2d, pos2d)) primitiveStream  
      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 500 500), DepthRange 0 1)) primitiveStream2  
      let sfilter = SamplerFilter Nearest Nearest Nearest Nothing
          edge = (pure Repeat, undefined)
      samp <- newSampler2D (const (tex, sfilter, edge))
      let sampleTexture = pure . sample2D samp SampleAuto Nothing Nothing  
          fragmentStream2 = fmap sampleTexture fragmentStream  
      drawContextColor (const (ContextColorOption NoBlending (pure True))) fragmentStream2       

    renderLoop $ do    
      clearContextColor 0.5   
      vertexArray <- newVertexArray vertexBuffer    
      shader (toPrimitiveArray TriangleStrip vertexArray)  

make3d :: Floating a => V2 a -> V4 a
make3d (V2 x y) = projMat !*! viewMat !* V4 x y 0 1  
  where       
    viewMat = lookAt' (V3 1 2 2) (V3 0.5 0.5 0) (V3 0 1 0)   
    projMat = perspective (pi/3) 1 1 100   

renderLoop :: Render os f () -> ContextT GLFW.GLFWWindow os f IO ()
renderLoop rendering = do   
  render rendering  
  swapContextBuffers    
  closeRequested <- GLFW.windowShouldClose    
  unless closeRequested $    
    renderLoop rendering  
     
-- Copy of lookAt from linear with normalize replaced with signorm   
lookAt' :: Floating a => V3 a -> V3 a -> V3 a -> V4 (V4 a)
lookAt' eye center up =  
  V4 (V4 (xa^._x) (xa^._y) (xa^._z) xd)  
     (V4 (ya^._x) (ya^._y) (ya^._z) yd)  
     (V4 (-za^._x) (-za^._y) (-za^._z) zd)  
     (V4 0     0     0     1)  
  where za = signorm $ center - eye  
        xa = signorm $ cross za up  
        ya = cross xa za  
        xd = -dot xa eye  
        yd = -dot ya eye  
        zd = dot za eye     