{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE UndecidableInstances  #-} -- for the Reflex t needed for instances of ShallowDiffable.  Maybe split fan out?
module Reflex.Dom.Contrib.ListHoldFunctions
  (
    listHoldWithKeyMap
  , listHoldWithKeyIntMap
  , listHoldWithKeyHashMap
  , listWithKeyShallowDiffMap
  , ListHoldable(..)
  , listHoldWithKeyGeneral
  , ShallowDiffable(..)  
  , listWithKeyShallowDiffGeneral
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


import Control.Monad.Identity (Identity(..),void)
import Control.Monad.Fix (MonadFix)
import Data.Functor.Misc (mapWithFunctorToDMap, dmapToMap, Const2(..))
import Data.Functor.Compose (Compose(Compose,getCompose))

import Data.Proxy (Proxy(..))


-- Can we get rid of the need for proxies here? AllowAmbiguousTypes and then use TypeApplication?
-- Can we put the GCompare constraint here?
class DM.GCompare (DMapKey f k a)=>ListHoldable (f :: * -> *) k v a where
  type DMapKey f k a :: * -> *
  type LHPatch f k v :: * 
  toDMapWithFunctor::Functor g=>(k->v->g a)->f v->DM.DMap (DMapKey f k a) g
  makePatch::Proxy f -> (k->v->g a)->LHPatch f k v -> PatchDMap (DMapKey f k a) g
  fromDMap::Proxy k->Proxy v->DM.DMap (DMapKey f k a) Identity -> f a


listHoldWithKeyGeneral::forall t m f k v a. (RD.DomBuilder t m, R.MonadHold t m, ListHoldable f k v a)
  =>f v -> R.Event t (LHPatch f k v) -> (k->v-> m a) -> m (R.Dynamic t (f a))
listHoldWithKeyGeneral c0 c' h = do
  let pf = Proxy :: Proxy f
      pk = Proxy :: Proxy k
      pv = Proxy :: Proxy v
      makeP = makePatch pf
      fromDM = fromDMap pk pv
      dc0 = toDMapWithFunctor h c0
      dc' = fmap (makeP h) c' --fmap (PatchDMap . toDMapWithComposeMaybe f) c'
  (a0,a') <- R.sequenceDMapWithAdjust dc0 dc'
  fmap fromDM . R.incrementalToDynamic <$> R.holdIncremental a0 a' --TODO: Move the dmapToMap to the righthand side so it doesn't get fully redone every time


class ShallowDiffable (f :: * -> *) v where
  type SDPatch f v :: * 
  type SDPatchKey f v :: *
  type SDPatchEventDKey f v :: * -> *  
  fanPatch::R.Reflex t=>Proxy f->Proxy v->R.Event t (SDPatch f v) -> R.EventSelector t (SDPatchEventDKey f v)
  emptyVoidPatch::Proxy f->Proxy v->SDPatch f ()
  voidPatch::Proxy f->Proxy v->SDPatch f v->SDPatch f ()
  makePatchEventDKey::Proxy f->Proxy v->SDPatchKey f v->SDPatchEventDKey f v v
  diffPatch::Proxy f->Proxy v->SDPatch f v -> SDPatch f () -> SDPatch f v
  applyPatch::Proxy f->Proxy v->SDPatch f () -> SDPatch f () -> SDPatch f () -- NB: this is a diff type from applyMap


listWithKeyShallowDiffGeneral :: forall t m f k v a.(RD.DomBuilder t m
                                                    , MonadFix m
                                                    , R.MonadHold t m
                                                    , ListHoldable f k v a
                                                    , ShallowDiffable f v
                                                    , SDPatch f v ~ LHPatch f k v
                                                    , SDPatchKey f v ~ k) 
  => f v -> R.Event t (SDPatch f v) -> (SDPatchKey f v -> v -> R.Event t v -> m a) -> m (R.Dynamic t (f a))
listWithKeyShallowDiffGeneral initialVals valsChanged mkChild = do
  let pf = Proxy :: Proxy f
      pv = Proxy :: Proxy v
      fanP = fanPatch pf pv
      emptyVoidP = emptyVoidPatch pf pv
      voidP = voidPatch pf pv
      makePEDK = makePatchEventDKey pf pv
      diffP = diffPatch pf pv
      applyP = applyPatch pf pv
      childValChangedSelector = fanP valsChanged
  sentVals <- R.foldDyn applyP emptyVoidP $ fmap voidP valsChanged
  listHoldWithKeyGeneral initialVals (R.attachWith (flip diffP) (R.current sentVals) valsChanged) $ \k v ->
    mkChild k v $ R.select childValChangedSelector $ makePEDK k

{-
class MapDiffable (f :: * -> *) v where
  type MDPatch f :: * -> *
--  compactMaybe::MDPatch v -> f v
  union::f v -> f v -> f v
  difference::
  

  diffNoEq:: f v -> f v -> f (Maybe v)
  diff::Eq v=> f v -> f v -> f (Maybe v)
  apply::f (Maybe v) -> f v -> f v
-} 

instance Ord k=>ListHoldable (Map k) k v a where
  type DMapKey (Map k) k a = Const2 k a
  type LHPatch (Map k) k v = Map k (Maybe v)
  toDMapWithFunctor h  = mapWithFunctorToDMap . Map.mapWithKey h
  makePatch _ h = PatchDMap . mapWithFunctorToDMap . Map.mapWithKey (\k mv -> ComposeMaybe $ fmap (h k) mv) 
  fromDMap _ _ = dmapToMap

instance Ord k=>ShallowDiffable (Map k) v where
  type SDPatch (Map k) v = Map k (Maybe v)
  type SDPatchKey (Map k) v = k
  type SDPatchEventDKey (Map k) v = Const2 k v
  fanPatch _ _ = R.fanMap . fmap (Map.mapMaybe id)
  emptyVoidPatch _ _ = Map.empty
  voidPatch _ _ = fmap void
  makePatchEventDKey _ _ = Const2
  diffPatch _ _ =
    let relevantPatch patch _ = case patch of
          Nothing -> Just Nothing
          Just _ -> Nothing          
    in Map.differenceWith relevantPatch
  applyPatch _ _ patch = RD.applyMap (fmap Just patch)

listHoldWithKeyMap::forall t m k v a. (RD.DomBuilder t m, R.MonadHold t m,Ord k)=>Map k v->R.Event t (Map k (Maybe v))->(k->v->m a)->m (R.Dynamic t (Map k a))
listHoldWithKeyMap = listHoldWithKeyGeneral

listWithKeyShallowDiffMap::forall t m k v a. (RD.DomBuilder t m, MonadFix m, R.MonadHold t m, Ord k)
  => Map k v -> R.Event t (Map k (Maybe v)) -> (k -> v -> R.Event t v -> m a) -> m (R.Dynamic t (Map k a))
listWithKeyShallowDiffMap = listWithKeyShallowDiffGeneral

intMapWithFunctorToDMap :: IntMap (f v) -> DMap (Const2 Int v) f
intMapWithFunctorToDMap = DM.fromDistinctAscList . fmap (\(k, v) -> Const2 k :=> v) . IM.toAscList

dmapToIntMap :: DMap (Const2 Int v) Identity -> IntMap v
dmapToIntMap = IM.fromDistinctAscList . fmap (\(Const2 k :=> Identity v) -> (k, v)) . DM.toAscList

instance ListHoldable IntMap Int v a where
  type DMapKey IntMap Int a = Const2 Int a
  type LHPatch IntMap Int v = IntMap (Maybe v)
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
  type LHPatch (HashMap k) k v= HashMap k (Maybe v)
  toDMapWithFunctor h  = hashMapWithFunctorToDMap . HM.mapWithKey h
  makePatch _ h = PatchDMap . hashMapWithFunctorToDMap .HM.mapWithKey (\k mv -> ComposeMaybe $ fmap (h k) mv) 
  fromDMap _ _ = dmapToHashMap

listHoldWithKeyHashMap::forall t m k v a. (RD.DomBuilder t m, R.MonadHold t m,Ord k, Hashable k)
  =>HashMap k v->R.Event t (HashMap k (Maybe v))->(k->v->m a)->m (R.Dynamic t (HashMap k a))
listHoldWithKeyHashMap = listHoldWithKeyGeneral

