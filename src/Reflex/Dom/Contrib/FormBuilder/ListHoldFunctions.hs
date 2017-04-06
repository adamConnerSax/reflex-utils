{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Reflex.Dom.Contrib.FormBuilder.ListHoldFunctions
  (
  ) where

import qualified Reflex as R
import Reflex.Patch (PatchDMap(..),ComposeMaybe(..))
import qualified Reflex.Dom as RD

import Data.Dependent.Map (DMap,DSum((:=>)))
import qualified Data.Dependent.Map as DM

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM


import Control.Monad.Identity (Identity(..))
import Data.Functor.Misc (mapWithFunctorToDMap, dmapToMap, Const2(..))

import Data.Proxy (Proxy(..))


listHoldWithKey :: forall t m k v a. (Ord k, RD.DomBuilder t m, R.MonadHold t m)
  => Map k v -> R.Event t (Map k (Maybe v)) -> (k -> v -> m a) -> m (R.Dynamic t (Map k a))
listHoldWithKey m0 m' f = do
  let dm0 = mapWithFunctorToDMap $ Map.mapWithKey f m0
      dm' = fmap (PatchDMap . mapWithFunctorToDMap . Map.mapWithKey (\k v -> ComposeMaybe $ fmap (f k) v)) m'
  (a0, a') <- R.sequenceDMapWithAdjust dm0 dm'
  fmap dmapToMap . R.incrementalToDynamic <$> R.holdIncremental a0 a' --TODO: Move the dmapToMap to the righthand side so it doesn't get fully redone every time

-- Can we get rid of the need for proxies here?
-- Can we put the GCompare constraint here?
class DM.GCompare (DMapKey f k a)=>ListHoldable f k v a where
  type DMapKey f k a :: * -> *
  type PatchType f k v :: *  
  toDMapWithFunctor::Functor g=>(k->v->g a)->f v->DM.DMap (DMapKey f k a) g
  makePatch::Proxy f -> (k->v->g a)->PatchType f k v -> PatchDMap (DMapKey f k a) g
  fromDMap::Proxy k->Proxy v->DM.DMap (DMapKey f k a) Identity -> f a

{-
class ShallowDiffable f k v a where
  type PatchType f k v :: *
  fanPatch::PatchType f k v -> R.EventSelector t (Const2 k v)
  emptyPatch::PatchType f k v
  applyPatch::PatchType f k v -> f v -> f v
  diffPatch::PatchType f k v -> PatchType f k () -> PatchType f k v 
-}


listHoldWithKeyGeneral::forall t m f k v a. (RD.DomBuilder t m, R.MonadHold t m, ListHoldable f k v a)
  =>f v -> R.Event t (PatchType f k v) -> (k->v-> m a) -> m (R.Dynamic t (f a))
listHoldWithKeyGeneral c0 c' h = do
  let dc0 = toDMapWithFunctor h c0
      dc' = fmap (makePatch (Proxy :: Proxy f) h) c' --fmap (PatchDMap . toDMapWithComposeMaybe f) c'
      fromDM = fromDMap (Proxy :: Proxy k) (Proxy :: Proxy v)
  (a0,a') <- R.sequenceDMapWithAdjust dc0 dc'
  fmap fromDM . R.incrementalToDynamic <$> R.holdIncremental a0 a' --TODO: Move the dmapToMap to the righthand side so it doesn't get fully redone every time


instance Ord k=>ListHoldable (Map k) k v a where
  type DMapKey (Map k) k a = Const2 k a
  type PatchType (Map k) k v = Map k (Maybe v)
  toDMapWithFunctor h  = mapWithFunctorToDMap . Map.mapWithKey h
  makePatch _ h = PatchDMap . mapWithFunctorToDMap . Map.mapWithKey (\k mv -> ComposeMaybe $ fmap (h k) mv) 
  fromDMap _ _ = dmapToMap


listHoldWithKeyMap::forall t m k v a. (RD.DomBuilder t m, R.MonadHold t m,Ord k)=>Map k v->R.Event t (Map k (Maybe v))->(k->v->m a)->m (R.Dynamic t (Map k a))
listHoldWithKeyMap = listHoldWithKeyGeneral

intMapWithFunctorToDMap :: IntMap (f v) -> DMap (Const2 Int v) f
intMapWithFunctorToDMap = DM.fromDistinctAscList . fmap (\(k, v) -> Const2 k :=> v) . IM.toAscList

dmapToIntMap :: DMap (Const2 Int v) Identity -> IntMap v
dmapToIntMap = IM.fromDistinctAscList . fmap (\(Const2 k :=> Identity v) -> (k, v)) . DM.toAscList

instance ListHoldable IntMap Int v a where
  type DMapKey IntMap Int a = Const2 Int a
  type PatchType IntMap Int v = IntMap (Maybe v)
  toDMapWithFunctor h  = intMapWithFunctorToDMap . IM.mapWithKey h
  makePatch _ h = PatchDMap . intMapWithFunctorToDMap . IM.mapWithKey (\k mv -> ComposeMaybe $ fmap (h k) mv) 
  fromDMap _ _ = dmapToIntMap

listHoldWithKeyIntMap::forall t m v a. (RD.DomBuilder t m, R.MonadHold t m)=>IntMap v->R.Event t (IntMap (Maybe v))->(Int->v->m a)->m (R.Dynamic t (IntMap a))
listHoldWithKeyIntMap = listHoldWithKeyGeneral

hashMapWithFunctorToDMap ::(Ord k, Hashable k)=>HashMap k (f v) -> DMap (Const2 k v) f
hashMapWithFunctorToDMap = DM.fromList . fmap (\(k, v) -> Const2 k :=> v) . HM.toList

dmapToHashMap ::(Hashable k, Eq k)=>DMap (Const2 k v) Identity -> HashMap k v
dmapToHashMap = HM.fromList . fmap (\(Const2 k :=> Identity v) -> (k, v)) . DM.toList

instance (Ord k, Hashable k)=>ListHoldable (HashMap k) k v a where
  type DMapKey (HashMap k) k a = Const2 k a
  type PatchType (HashMap k) k v= HashMap k (Maybe v)
  toDMapWithFunctor h  = hashMapWithFunctorToDMap . HM.mapWithKey h
  makePatch _ h = PatchDMap . hashMapWithFunctorToDMap .HM.mapWithKey (\k mv -> ComposeMaybe $ fmap (h k) mv) 
  fromDMap _ _ = dmapToHashMap

listHoldWithKeyHashMap::forall t m k v a. (RD.DomBuilder t m, R.MonadHold t m,Ord k, Hashable k)
  =>HashMap k v->R.Event t (HashMap k (Maybe v))->(k->v->m a)->m (R.Dynamic t (HashMap k a))
listHoldWithKeyHashMap = listHoldWithKeyGeneral

