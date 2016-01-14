module Math(
  -- | 3D matrix transformations
    scale
  , rotationZ
  , translate
  -- | 2D matrix transformations
  , scale2D
  , rotation2D
  , translate2D
  , toHom2D
  , fromHom2D
  , applyTransform2D
  ) where

import Linear

scale :: Num a => V3 a -> M44 a 
scale (V3 x y z) = V4
  (V4 x 0 0 0)
  (V4 0 y 0 0)
  (V4 0 0 z 0)
  (V4 0 0 0 1)

rotationZ :: Floating a => a -> M44 a 
rotationZ a = V4 
  (V4 (cos a) (- sin a) 0 0)
  (V4 (sin a) (  cos a) 0 0)
  (V4 0 0 1 0)
  (V4 0 0 0 1)

translate :: Num a => V3 a -> M44 a 
translate (V3 x y z) = V4 
  (V4 1 0 0 x)
  (V4 0 1 0 y)
  (V4 0 0 1 z)
  (V4 0 0 0 1)

scale2D :: Num a => V2 a -> M33 a 
scale2D (V2 x y) = V3
  (V3 x 0 0)
  (V3 0 y 0)
  (V3 0 0 1)

rotation2D :: Floating a => a -> M33 a 
rotation2D a = V3 
  (V3 (cos a) (- sin a) 0)
  (V3 (sin a) (  cos a) 0)
  (V3 0 0 1)

translate2D :: Num a => V2 a -> M33 a 
translate2D (V2 x y) = V3 
  (V3 1 0 x)
  (V3 0 1 y)
  (V3 0 0 1)

-- | Transform to homogenius coordinates
toHom2D :: Num a => V2 a -> V3 a 
toHom2D (V2 x y) = V3 x y 1

-- | Transform from homogenius coordinates
fromHom2D :: Floating a => V3 a -> V2 a 
fromHom2D (V3 x y w) = V2 (x/w) (y/w)

-- | Applies transformation matrix to vector
applyTransform2D :: Floating a => M33 a -> V2 a -> V2 a 
applyTransform2D mt v = fromHom2D $ mt !* toHom2D v