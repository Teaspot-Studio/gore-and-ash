module Render(
    renderGame
  ) where

import Game as GM 
import Graphics as GR
import Graphics.Camera as GR
import Graphics.Square as GR

renderGame :: Game -> RenderState os -> RenderState os
renderGame Game{..} rs = rs {
    renderSquare = renderGameSquare gamePlayer $ renderSquare rs
  , renderCamera = renderGameCamera gameCamera $ renderCamera rs
  }  

renderGameSquare :: GM.Player -> GR.Square os -> GR.Square os
renderGameSquare p s = s {
    GR.squarePos = realToFrac <$> GM.playerPos p
  , GR.squareRot = realToFrac $ GM.playerRot p
  , GR.squareColor = realToFrac <$> GM.playerColor p
  , GR.squareDirty = True
  } 

renderGameCamera :: GM.Camera -> GR.Camera os -> GR.Camera os
renderGameCamera gc c = c {
    GR.cameraPos = realToFrac <$> GM.cameraPos gc
  , GR.cameraRot = realToFrac $ GM.cameraRot gc
  , GR.cameraZoom = realToFrac $ GM.cameraZoom gc
  , GR.cameraDirty = True
  } 

