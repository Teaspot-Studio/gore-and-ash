{-|
Module      : Game.GoreAndAsh.Core.Wire
Description : Definition of Arrow based on an monad.
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

I don't know yet if the arrow is helpful now. If it would not find an application,
the module would be deleted from the package.
-}
module Game.GoreAndAsh.Core.Wire(
    Wire(..)
  ) where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

newtype Wire m a b = Wire { unWire :: a -> m (b, Wire m a b) }

instance Monad m => Category (Wire m) where
  id = Wire $ \a -> return (a, id)
  (Wire mkC) . (Wire mkB) = Wire $ \a -> do
    (b, mkB') <- mkB a
    (c, mkC') <- mkC b
    return (c, mkC' . mkB')

instance Monad m => Arrow (Wire m) where
  arr f = Wire $ \b -> return (f b, arr f)
  first (Wire mkC) = Wire $ \(b, d) -> do
    (c, mkC') <- mkC b
    return ((c, d), first mkC')
  second (Wire mkC) = Wire $ \(d, b) -> do
    (c, mkC') <- mkC b
    return ((d, c), second mkC')
  (Wire mkC1) *** (Wire mkC2) = Wire $ \(b1, b2) -> do
    (c1, mkC1') <- mkC1 b1
    (c2, mkC2') <- mkC2 b2
    return ((c1, c2), mkC1' *** mkC2')
  (Wire mkC1) &&& (Wire mkC2) = Wire $ \b -> do
    (c1, mkC1') <- mkC1 b
    (c2, mkC2') <- mkC2 b
    return ((c1, c2), mkC1' &&& mkC2')