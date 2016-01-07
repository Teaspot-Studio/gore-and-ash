module Render(
    renderGame
  ) where

import Control.Monad.Exception
import Control.Monad.IO.Class
import Game as GM 
import Game.RemotePlayer as GM
import Graphics as GR
import Graphics.Camera as GR
import Graphics.GPipe
import Graphics.Square as GR
import qualified Data.Foldable as F 

renderGame :: (MonadIO m, MonadException m) => Game -> RenderState os -> ContextT w os f m (RenderState os)
renderGame Game{..} rs = do 
  -- liftIO $ print gamePlayer
  rs' <- F.foldlM (\s _ -> addSquare s) rs [1 .. length gameRemotePlayers + 1 - length (renderSquares rs)]
  return $ rs' {
    renderSquares = case renderSquares rs' of
      [] -> []
      (x:xs) -> renderGameSquare gamePlayer x : fmap (uncurry renderGameSquareR) (gameRemotePlayers `zip` xs)
  , renderCamera = renderGameCamera gameCamera $ renderCamera rs'
  }  

renderGameSquare :: GM.Player -> GR.Square os -> GR.Square os
renderGameSquare p s = s {
    GR.squarePos = realToFrac <$> GM.playerPos p
  , GR.squareRot = realToFrac $ GM.playerRot p
  , GR.squareColor = realToFrac <$> GM.playerColor p
  , GR.squareDirty = True
  } 

renderGameSquareR :: GM.RemotePlayer -> GR.Square os -> GR.Square os
renderGameSquareR p s = s {
    GR.squarePos = realToFrac <$> GM.remotePlayerPos p
  , GR.squareRot = realToFrac $ GM.remotePlayerRot p
  , GR.squareColor = realToFrac <$> GM.remotePlayerCol p
  , GR.squareDirty = True
  } 

renderGameCamera :: GM.Camera -> GR.Camera os -> GR.Camera os
renderGameCamera gc c = c {
    GR.cameraPos = realToFrac <$> GM.cameraPos gc
  , GR.cameraRot = realToFrac $ GM.cameraRot gc
  , GR.cameraZoom = realToFrac $ GM.cameraZoom gc
  , GR.cameraDirty = True
  } 

