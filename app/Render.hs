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
    GR.squarePos = GM.playerPos p
  , GR.squareRot = GM.playerRot p
  , GR.squareColor = GM.playerColor p
  } 

renderGameCamera :: GM.Camera -> GR.Camera os -> GR.Camera os
renderGameCamera gc c = c {
    GR.cameraPos = GM.cameraPos gc
  , GR.cameraRot = GM.cameraRot gc
  , GR.cameraZoom = GM.cameraZoom gc
  } 

