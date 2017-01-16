{-|
Module      : Game.GoreAndAsh.Core.Chain
Description : Helper for recursive chaining of reactive components.
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE RecursiveDo #-}
module Game.GoreAndAsh.Core.Chain(
    Chain(..)
  , chain
  ) where

import Game.GoreAndAsh.Core.Monad

-- | Like 'Fix' for FRP widget, allow to endlessly jump into
-- returned routes of widgets
newtype Chain t m a = Chain { unChain :: Event t (m (a, Chain t m a)) }

instance Reflex t => Monoid (Chain t m a) where
  mempty = Chain never
  (Chain e1) `mappend` (Chain e2) = Chain $ leftmost [e1, e2]

-- | Run component that can replace itself with new component constructed
-- internally in the original widget.
chain :: forall t m a . MonadAppHost t m => m (a, Chain t m a) -> m (Dynamic t a)
chain w = do
  rec (rd :: Dynamic t (a, Chain t m a)) <- holdAppHost w re
      let (rd':: Dynamic t (Event t (m (a, Chain t m a)))) = fmap (unChain . snd) rd
          (re :: Event t (m (a, Chain t m a))) = switchPromptlyDyn rd'
  return $ fst <$> rd
