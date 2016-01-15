module Graphics.Square(
    renderSquare
  ) where

import Consts
import Data.Word
import Foreign.C.Types
import Game.Camera 
import Game.GoreAndAsh
import Game.GoreAndAsh.SDL 
import Linear
import Linear.Affine
import Math 
import qualified Data.Vector.Storable as V 

-- | Function of rendering player
renderSquare :: MonadSDL m => Double -> V2 Double -> V3 Double -> Camera -> GameMonadT m ()
renderSquare size pos col c = do  
  mwr <- sdlGetWindowM mainWindowName
  case mwr of 
    Nothing -> return ()
    Just (_, r) -> do 
      rendererDrawColor r $= transColor col 
      drawLines r transformedSquare
  where
    transColor :: V3 Double -> V4 Word8
    transColor (V3 r g b) = V4 (round $ r * 255) (round $ g * 255) (round $ b * 255) 255

    square :: Double -> V.Vector (V2 Double)
    square s = V.fromList [
        V2 s s 
      , V2 (-s) s
      , V2 (-s) (-s)
      , V2 s (-s)
      , V2 s s
      ]

    modelMtx :: M33 Double 
    modelMtx = translate2D pos !*! cameraMatrix c

    transformedSquare :: V.Vector (Point V2 CInt)
    transformedSquare = V.map (P . fmap round . applyTransform2D modelMtx) $ square size