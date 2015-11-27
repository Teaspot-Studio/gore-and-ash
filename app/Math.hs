module Math(
    scale
  , rotationZ
  , translate
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