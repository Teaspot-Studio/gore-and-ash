module Game.Camera(
    Camera(..)
  , CameraId(..)
  , CameraMessage(..)
  , cameraWire
  , cameraMatrix
  ) where

import Control.DeepSeq
import Control.Lens 
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Typeable 
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.Core
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.SDL
import Math 

data Camera = Camera {
  cameraId :: !CameraId
, cameraPos :: !(V2 Double)
, cameraZoom :: !Double
} deriving (Generic)

instance NFData Camera 

newtype CameraId = CameraId { unCameraId :: Int } deriving (Eq, Show, Generic)
instance NFData CameraId 

data CameraMessage = CameraMessageStub deriving (Typeable, Generic)
instance NFData CameraMessage 

instance ActorMessage CameraId where
  type ActorMessageType CameraId = CameraMessage
  toCounter = unCameraId
  fromCounter = CameraId

cameraWire :: (CameraId -> Camera) -> AppActor CameraId a Camera 
cameraWire initialCamera = stateActor initialCamera process $ \_ -> proc (_, c) -> do 
  c2 <- moveCamera (V2 0 (-cameraSpeed)) ScancodeS 
    . moveCamera (V2 0 cameraSpeed) ScancodeW 
    . moveCamera (V2 cameraSpeed 0) ScancodeA
    . moveCamera (V2 (-cameraSpeed) 0) ScancodeD
    . zoomCamera 0.1 -< c
  forceNF -< c2
  where 
    cameraSpeed :: Double 
    cameraSpeed = 50

    process :: CameraId -> Camera -> CameraMessage -> Camera 
    process _ c _ = c 

    moveCamera :: V2 Double -> Scancode -> AppWire Camera Camera
    moveCamera dv k = proc c -> do 
      e <- keyPressing k -< ()
      let newCam = c {
            cameraPos = cameraPos c + dv 
          }
      returnA -< event c (const newCam) e

    zoomCamera :: Double -> AppWire Camera Camera 
    zoomCamera z = proc c -> do 
      e <- mouseScrollY -< ()
      let newCam k = c {
            cameraZoom = max (-3) $ min (-0.01) $ cameraZoom c + z * k
          }
      returnA -< event c (newCam . fromIntegral) e 

-- | Calculate transformation matrix for camera
cameraMatrix :: Camera -> M33 Double
cameraMatrix Camera{..} = scale2D (V2 cameraZoom cameraZoom) 
  !*! translate2D (V2 (-cameraPos^._x) (-cameraPos^._y))