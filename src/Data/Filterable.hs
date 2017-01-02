{-|
Module      : Data.Filterable
Description : Generalization of filter function.
Copyright   : (c) Anton Gushcha, 2015-2016
                  Oganyan Levon, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

Defines generic filter utilities for collections.
-}
module Data.Filterable(
    Filterable(..)
  , KeyHashMap(..)
  , cutMaybes
  ) where

import Control.Monad (filterM)
import Data.Hashable
import Data.Maybe
import GHC.Exts
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M

-- | Generic filter for collections
class Filterable f where
  -- | Specific constraint for instance
  type FilterConstraint f o :: Constraint
  type FilterConstraint f o = ()

  -- | Test collection for emptiness
  fNull :: FilterConstraint f a => f a -> Bool
  -- | Filter function for collection
  fFilter :: FilterConstraint f a => (a -> Bool) -> f a -> f a
  -- | Monad version of filter
  fFilterM :: (FilterConstraint f a, Monad m) => (a -> m Bool) -> f a -> m (f a)

instance Filterable [] where
  fNull = null
  fFilter = filter
  fFilterM = filterM
  {-# INLINE fNull #-}
  {-# INLINE fFilter #-}
  {-# INLINE fFilterM #-}

instance Filterable S.Seq where
  fNull = S.null
  fFilter = S.filter
  fFilterM p = F.foldlM (\xs x -> do
    f <- p x
    return $! if f then xs S.|> x else xs) S.empty
  {-# INLINE fNull #-}
  {-# INLINE fFilter #-}
  {-# INLINE fFilterM #-}

-- | Wrapper around HashMap to Filterable instance over keys
newtype KeyHashMap v k = KeyHashMap { unKeyHashMap :: H.HashMap k v }

instance Filterable (KeyHashMap v) where
  type FilterConstraint (KeyHashMap v) o = (Eq o, Hashable o)
  fNull = H.null . unKeyHashMap
  fFilter p (KeyHashMap m) = KeyHashMap $ H.filterWithKey (\k _ -> p k) m
  fFilterM p (KeyHashMap m) = fmap KeyHashMap $ H.foldlWithKey' (\mxs k x -> do
    xs <- mxs
    f <- p k
    return $! if f then H.insert k x xs else xs) (return H.empty) m
  {-# INLINE fNull #-}
  {-# INLINE fFilter #-}
  {-# INLINE fFilterM #-}

instance (Eq k, Hashable k) => Filterable (H.HashMap k) where
  fNull = H.null
  fFilter = H.filter
  fFilterM p = H.foldlWithKey' (\mxs k x -> do
    xs <- mxs
    f <- p x
    return $! if f then H.insert k x xs else xs) (return H.empty)
  {-# INLINE fNull #-}
  {-# INLINE fFilter #-}
  {-# INLINE fFilterM #-}

instance Ord k => Filterable (M.Map k) where
  fNull = M.null
  fFilter = M.filter
  fFilterM p = M.foldlWithKey' (\mxs k x -> do
    xs <- mxs
    f <- p x
    return $! if f then M.insert k x xs else xs) (return M.empty)
  {-# INLINE fNull #-}
  {-# INLINE fFilter #-}
  {-# INLINE fFilterM #-}

-- | Throw away Nothings from collection
cutMaybes :: (Functor f, Filterable f, FilterConstraint f (Maybe a)) => f (Maybe a) -> f a
cutMaybes = fmap fromJust . fFilter isJust
