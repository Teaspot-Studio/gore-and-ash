module Graphics.Bullet(
    renderBullet
  ) where

import Consts
import Game.Camera 
import Game.GoreAndAsh
import Game.GoreAndAsh.Math 
import Game.GoreAndAsh.SDL 
import Linear
import Linear.Affine
import SDL 

-- | Function of rendering player
renderBullet :: MonadSDL m => V2 Double -> V2 Double -> Camera -> GameMonadT m ()
renderBullet pos vel c = do  
  mwr <- sdlGetWindowM mainWindowName
  case mwr of 
    Nothing -> return ()
    Just (w, r) -> do 
      wsize <- fmap (fmap fromIntegral) . get $ windowSize w
      rendererDrawColor r $= V4 0 0 0 255
      drawLine r (apply wsize startPoint) (apply wsize endPoint)
  where
    velScaleFactor = 0.04
    startPoint = negate (velScaleFactor * 0.5 * vel)
    endPoint = velScaleFactor * 0.5 * vel

    apply wsize = P . fmap round . applyTransform2D (modelMtx wsize)

    modelMtx :: V2 Double -> M33 Double 
    modelMtx wsize = viewportTransform2D 0 wsize !*! cameraMatrix c !*! translate2D pos