module Game.Camera(
    Camera(..)
  , CameraId(..)
  , CameraMessage(..)
  , cameraWire
  ) where

import Control.DeepSeq
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Typeable 
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.Core
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.GLFW

data Camera = Camera {
  cameraPos :: !(V2 Float)
, cameraRot :: !Float 
, cameraZoom :: !Float
} deriving (Generic)

instance NFData Camera 

newtype CameraId = CameraId { unCameraId :: Int } deriving Generic 
instance NFData CameraId 

data CameraMessage = CameraMessageStub deriving (Typeable, Generic)
instance NFData CameraMessage 

instance ActorMessage CameraId where
  type ActorMessageType CameraId = CameraMessage
  toCounter = unCameraId
  fromCounter = CameraId 

cameraWire :: Camera -> AppActor CameraId a Camera 
cameraWire initialCamera = stateActor initialCamera process $ \_ -> proc (_, c) -> do 
  c2 <- moveCamera (V2 0 (-0.1)) Key'W 
    . moveCamera (V2 0 0.1) Key'S 
    . moveCamera (V2 0.1 0) Key'A
    . moveCamera (V2 (-0.1) 0) Key'D 
    . zoomCamera 0.1 -< c
  forceNF -< c2
  where 
    process :: CameraId -> Camera -> CameraMessage -> Camera 
    process _ c _ = c 

    moveCamera :: V2 Float -> Key -> AppWire Camera Camera
    moveCamera dv k = proc c -> do 
      e <- keyPressing k -< ()
      let newCam = c {
            cameraPos = cameraPos c + dv 
          }
      returnA -< event c (const newCam) e

    zoomCamera :: Float -> AppWire Camera Camera 
    zoomCamera z = proc c -> do 
      e <- mouseScrollY -< ()
      let newCam k = c {
            cameraZoom = max (-3) $ min (-0.01) $ cameraZoom c + z * k
          }
      returnA -< event c (newCam . realToFrac) e 