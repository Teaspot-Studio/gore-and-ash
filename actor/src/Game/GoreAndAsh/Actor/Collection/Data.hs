module Game.GoreAndAsh.Actor.Collection.Data(
    DynCollection(..)
  , rightsDynColl
  ) where

import Control.Monad 
import Control.Wire
import Data.Either (isRight)
import Data.Filterable 
import Data.Hashable 
import Data.List (nub)
import GHC.Exts
import Prelude hiding ((.), id)
import qualified Data.Foldable as F 
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

class (Filterable c, F.Foldable c, Functor c, Traversable c) => DynCollection c where
  type DynConsConstr c o :: Constraint 
  type DynConsConstr c o = ()

  concatDynColl :: c a -> c a -> c a
  unzipDynColl :: c (a , b) -> (c a, c b)
  zipDynColl :: c a -> c b -> c (a, b)
  emptyDynColl :: c a 
  consDynColl :: DynConsConstr c a => a -> c a -> c a 

instance DynCollection [] where
  concatDynColl = (++)
  unzipDynColl = unzip 
  zipDynColl = zip 
  emptyDynColl = []
  consDynColl = (:)

instance DynCollection S.Seq where 
  concatDynColl = (S.><)
  unzipDynColl = F.foldl' (\(as, bs) (a, b) -> (as S.|> a, bs S.|> b)) (S.empty, S.empty)
  zipDynColl = S.zip 
  emptyDynColl = S.empty
  consDynColl = (S.<|)

class (Hashable i, Eq i) => ElementWithId a i where
  elementId :: a -> i 

instance (Eq k, Hashable k) => DynCollection (H.HashMap k) where
  type DynConsConstr (H.HashMap k) o = ElementWithId o k

  concatDynColl = H.union
  unzipDynColl = H.foldlWithKey' (\(as, bs) k (a, b) -> (H.insert k a as, H.insert k b bs)) (H.empty, H.empty) 
  zipDynColl as bs = F.foldl' mrg H.empty $ nub $ H.keys as ++ H.keys bs
    where 
    mrg acc k = case (H.lookup k as, H.lookup k bs) of 
      (Just a, Just b) -> H.insert k (a, b) acc
      _ -> acc

  emptyDynColl = H.empty
  consDynColl a = H.insert (elementId a) a

-- | Helper to filter out lefts
rightsDynColl :: (FilterConstraint c (Either e a), DynCollection c) 
  => c (Either e a) -> c a 
rightsDynColl = fmap fromRight . fFilter isRight
  where
  fromRight e = case e of 
    Left _ -> error "rightsDynColl: left (impossible)"
    Right a -> a
