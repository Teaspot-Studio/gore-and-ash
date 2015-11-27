module Graphics.Camera(
    CameraUniform
  , Camera(..)
  , newCamera
  , updateCamera
  ) where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class 
import Graphics.GPipe
import Math 

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