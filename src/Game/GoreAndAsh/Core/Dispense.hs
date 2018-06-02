{-|
Module      : Game.GoreAndAsh.Core.Dispense
Description : Helpers for item dispensing from collections.
Copyright   : (c) Anton Gushcha, 2016-2017
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.Core.Dispense(
    ItemRoller
  , itemRoller
  ) where

import Control.Monad.IO.Class
import Game.GoreAndAsh.Core.ExternalRef
import Game.GoreAndAsh.Core.Monad

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

-- | Item roller contains dynamic of current element and action to switch to next
-- item
type ItemRoller t a = (Dynamic t a, IO ())

-- | Create a item chooser from given list, returns dynamic with current item
-- and action to change it.
itemRoller :: (Reflex t, MonadHold t m, TriggerEvent t m, MonadIO m) => NonEmpty a -> m (ItemRoller t a)
itemRoller as = do
  ref <- newExternalRef (NE.toList as, [])
  let getCurItem xs = case xs of
        [] -> error "itemRoller: impossible"
        (x : _) -> x
  curDyn <- fmap (getCurItem . fst) <$> externalRefDynamic ref
  let updRoller = modifyExternalRef ref $ \(xs, ys) -> case xs of
        []        -> ((reverse ys, []),  ())
        [x]       -> ((reverse ys, [x]), ())
        (x : xs') -> ((xs', x : ys), ())
  return (curDyn, updRoller)
