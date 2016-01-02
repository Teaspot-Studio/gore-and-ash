module Game.Camera(
    Camera(..)
  , cameraWire
  ) where

import Control.DeepSeq
import Control.Wire
import Control.Wire.Unsafe.Event
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.Core
import Game.GoreAndAsh.GLFW

data Camera = Camera {
  cameraPos :: !(V2 Float)
, cameraRot :: !Float 
, cameraZoom :: !Float
} deriving (Generic)

instance NFData Camera 

cameraWire :: Camera -> AppWire a Camera 
cameraWire initialCamera = loop $ proc (_, c_) -> do 
  c <- delay initialCamera -< c_
  c2 <- moveCamera (V2 0 (-0.1)) Key'W 
    . moveCamera (V2 0 0.1) Key'S 
    . moveCamera (V2 0.1 0) Key'A
    . moveCamera (V2 (-0.1) 0) Key'D 
    . zoomCamera 0.1 -< c
  forceNF -< (c2, c2)
  where 
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