{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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

import qualified Reflex                 as R
import qualified Reflex.Dom             as RD
import           Reflex.Patch           (ComposeMaybe (..), PatchDMap (..))

import           Data.Dependent.Map     (DMap, DSum ((:=>)))
import qualified Data.Dependent.Map     as DM

import           Data.Map               (Map)
import qualified Data.Map               as Map

import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IM

import           Data.Hashable
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM


import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Identity (Identity (..), void)
import           Data.Functor.Compose   (Compose(Compose,getCompose))
import           Data.Functor.Misc      (Const2 (..), dmapToMap,
                                         mapWithFunctorToDMap)

import           Data.Proxy             (Proxy (..))




-- This just says we can sequence in the way of monadAdjust 
class DM.GCompare k=>Sequenceable (d :: (* -> *) -> (* -> *) -> *) (pd :: (* -> *) -> (* -> *) -> *)  (k :: * -> *) where
  sequenceWithPatch::(R.Reflex t, R.MonadAdjust t m)=>d k m -> R.Event t (pd k m) -> m (d k Identity, R.Event t (pd k Identity))

-- Can we get rid of the need for proxies here? AllowAmbiguousTypes and then use TypeApplication?
class ListHoldable (f :: * -> *) k v a where
  type DMapKey f k a :: * -> *
  type LHPatch f k v :: *
  type DMapPatchType f k a :: (* -> *) -> (* -> *) -> *
  toDMapWithFunctor::Functor g=>(k->v->g a)->f v->DM.DMap (DMapKey f k a) g
  makePatch::Proxy f->(k->v->g a)->LHPatch f k v -> DMapPatchType f k a (DMapKey f k a) g
  fromDMap::Proxy k->Proxy v->DM.DMap (DMapKey f k a) Identity -> f a

-- This class has the capabilities to translate f v and its difftype into types that are sequenceable, and then bring original type back
class (RD.Patch (SeqPatchType f k (SeqTypeKey f k a) Identity)
      ,R.PatchTarget (SeqPatchType f k (SeqTypeKey f k a) Identity) ~ SeqType f k (SeqTypeKey f k a) Identity)=>ToPatchType (f :: * -> *) k v a where
  type DiffType f k :: * -> *
  type SeqType  f k :: (* -> *) -> (* -> *) -> *
  type SeqPatchType f k :: (* -> *) -> (* -> *) -> *
  type SeqTypeKey f k a :: * -> *
  toSeqTypeWithFunctor::Functor g=>(k->v->g a) -> f v -> SeqType f k (SeqTypeKey f k a) g
  makePatchSeq::Proxy f->(k->v->g a) -> DiffType f k v -> SeqPatchType f k (SeqTypeKey f k a) g
  fromSeqType::Proxy k->Proxy v->SeqType f k (SeqTypeKey f k a) Identity -> f a

listHoldWithKeyGeneral'::forall t m f k v a. (RD.DomBuilder t m, R.MonadHold t m
                                            , ToPatchType f k v a
                                            , Sequenceable (SeqType f k) (SeqPatchType f k) (SeqTypeKey f k a))
  =>f v -> R.Event t (DiffType f k v) -> (k->v-> m a) -> m (R.Dynamic t (f a))
listHoldWithKeyGeneral' c0 c' h = do
  let pf = Proxy :: Proxy f
      pk = Proxy :: Proxy k
      pv = Proxy :: Proxy v
      makePatchSeq' = makePatchSeq pf
      fromSeqType' = fromSeqType pk pv
      dc0 = toSeqTypeWithFunctor h c0
      dc' = fmap (makePatchSeq' h) c' 
  (a0,a') <- sequenceWithPatch dc0 dc'
  fmap fromSeqType' . R.incrementalToDynamic <$> R.holdIncremental a0 a' --TODO: Move the dmapToMap to the righthand side so it doesn't get fully redone every time



listHoldWithKeyGeneral::forall t m f k v a. (RD.DomBuilder t m, R.MonadHold t m
                                            , ListHoldable f k v a
                                            , Sequenceable DM.DMap (DMapPatchType f k a) (DMapKey f k a)
                                            , RD.Patch (DMapPatchType f k a (DMapKey f k a) Identity)
                                            , R.PatchTarget (DMapPatchType f k a (DMapKey f k a) Identity) ~ DM.DMap (DMapKey f k a) Identity)
  =>f v -> R.Event t (LHPatch f k v) -> (k->v-> m a) -> m (R.Dynamic t (f a))
listHoldWithKeyGeneral c0 c' h = do
  let pf = Proxy :: Proxy f
      pk = Proxy :: Proxy k
      pv = Proxy :: Proxy v
      makeP = makePatch pf
      fromDM = fromDMap pk pv
      dc0 = toDMapWithFunctor h c0
      dc' = fmap (makeP h) c' --fmap (PatchDMap . toDMapWithComposeMaybe f) c'
  (a0,a') <- sequenceWithPatch dc0 dc'
  fmap fromDM . R.incrementalToDynamic <$> R.holdIncremental a0 a' --TODO: Move the dmapToMap to the righthand side so it doesn't get fully redone every time


class HasFan (a :: * -> *) v where
  type FanInKey a :: * 
  type FanSelKey a v :: * -> *
  makeSelKey::Proxy a->Proxy v->FanInKey a->FanSelKey a v v
  doFan::R.Reflex t=>Proxy v->R.Event t (a v) -> R.EventSelector t (FanSelKey a v)
  
class ShallowDiffable (f :: * -> *) v where
  type Diff f :: * -> *
  emptyVoidDiff::Proxy f->Proxy v->Diff f ()
  voidDiff::Proxy f->Proxy v->Diff f v->Diff f ()
  diff::Proxy f->Proxy v->Diff f v -> Diff f () -> Diff f v
  applyDiff::Proxy f->Proxy v->Diff f () -> Diff f () -> Diff f () -- NB: this is a diff type from applyMap


listWithKeyShallowDiffGeneral :: forall t m f k v a.(RD.DomBuilder t m
                                                    , MonadFix m
                                                    , R.MonadHold t m
                                                    , ToPatchType f k v a
                                                    , Sequenceable (SeqType f k) (SeqPatchType f k) (SeqTypeKey f k a)
                                                    , ShallowDiffable f v
                                                    , HasFan (Diff f) v
                                                    , FanInKey (Diff f) ~ k                                                      
                                                    , Diff f ~ DiffType f k)
--                                                    , RD.Patch (DMapPatchType f k a (DMapKey f k a) Identity)
--                                                    , RD.PatchTarget (DMapPatchType f k a (DMapKey f k a) Identity) ~ DM.DMap (DMapKey f k a) Identity)
  => f v -> R.Event t (Diff f v) -> (FanInKey (Diff f) -> v -> R.Event t v -> m a) -> m (R.Dynamic t (f a))
listWithKeyShallowDiffGeneral initialVals valsChanged mkChild = do
  let pf = Proxy :: Proxy f
      pv = Proxy :: Proxy v
      emptyVoidDiff' = emptyVoidDiff pf pv
      voidDiff' = voidDiff pf pv
      pDiff = Proxy :: Proxy (Diff f)
      makeSelKey' = makeSelKey pDiff pv
      diff' = diff pf pv
      applyDiff' = applyDiff pf pv
      doFan' = doFan pv
      childValChangedSelector = doFan' valsChanged
  sentVals <- R.foldDyn applyDiff' emptyVoidDiff' $ fmap voidDiff' valsChanged
  listHoldWithKeyGeneral' initialVals (R.attachWith (flip diff') (R.current sentVals) valsChanged) $ \k v ->
    mkChild k v $ R.select childValChangedSelector $ makeSelKey' k


{-
listWithKeyShallowDiffGeneral :: forall t m f k v a.(RD.DomBuilder t m
                                                    , MonadFix m
                                                    , R.MonadHold t m
                                                    , ListHoldable f k v a
                                                    , Sequenceable DM.DMap (DMapPatchType f k a) (DMapKey f k a)
                                                    , ShallowDiffable f v
                                                    , HasFan (Diff f v)
                                                    , FanVal (Diff f v) ~ v
                                                    , FanInKey (Diff f v) ~ k                                                      
                                                    , Diff f v ~ LHPatch f k v
                                                    , RD.Patch (DMapPatchType f k a (DMapKey f k a) Identity)
                                                    , RD.PatchTarget (DMapPatchType f k a (DMapKey f k a) Identity) ~ DM.DMap (DMapKey f k a) Identity)
  => f v -> R.Event t (Diff f v) -> (FanInKey (Diff f v) -> v -> R.Event t v -> m a) -> m (R.Dynamic t (f a))
listWithKeyShallowDiffGeneral initialVals valsChanged mkChild = do
  let pf = Proxy :: Proxy f
      pv = Proxy :: Proxy v
      emptyVoidDiff' = emptyVoidDiff pf pv
      voidDiff' = voidDiff pf pv
      pDiff = Proxy :: Proxy (Diff f v)
      makeSelKey' = makeSelKey pDiff
      diff' = diff pf pv
      applyDiff' = applyDiff pf pv
      childValChangedSelector = doFan valsChanged
  sentVals <- R.foldDyn applyDiff' emptyVoidDiff' $ fmap voidDiff' valsChanged
  listHoldWithKeyGeneral initialVals (R.attachWith (flip diff') (R.current sentVals) valsChanged) $ \k v ->
    mkChild k v $ R.select childValChangedSelector $ makeSelKey' k
-}
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

instance Ord k=>Sequenceable DM.DMap PatchDMap (Const2 k a) where
  sequenceWithPatch = R.sequenceDMapWithAdjust

instance Ord k=>ListHoldable (Map k) k v a where
  type DMapKey (Map k) k a = Const2 k a
  type LHPatch (Map k) k v = Map k (Maybe v)
  type DMapPatchType (Map k) k a = PatchDMap 
  toDMapWithFunctor h  = mapWithFunctorToDMap . Map.mapWithKey h
  makePatch _ h = PatchDMap . mapWithFunctorToDMap . Map.mapWithKey (\k mv -> ComposeMaybe $ fmap (h k) mv)
  fromDMap _ _ = dmapToMap

instance Ord k=>ToPatchType (Map k) k v a where
  type DiffType (Map k) k = Compose (Map k) Maybe
  type SeqType (Map k) k = DM.DMap
  type SeqPatchType (Map k) k = PatchDMap
  type SeqTypeKey (Map k) k a = Const2 k a
  toSeqTypeWithFunctor h = mapWithFunctorToDMap . Map.mapWithKey h
  makePatchSeq _ h = PatchDMap . mapWithFunctorToDMap . Map.mapWithKey (\k mv -> ComposeMaybe $ fmap (h k) mv) . getCompose
  fromSeqType _ _ = dmapToMap

instance Ord k=>HasFan (Compose (Map k) Maybe) v where
  type FanInKey (Compose (Map k) Maybe) = k
  type FanSelKey (Compose (Map k) Maybe) v = Const2 k v
  doFan _ = R.fanMap . fmap (Map.mapMaybe id) . fmap getCompose
  makeSelKey _ _ = Const2

instance Ord k=>ShallowDiffable (Map k) v where
  type Diff (Map k) = Compose  (Map k) Maybe
  emptyVoidDiff _ _ = Compose Map.empty
  voidDiff _ _ = void
  diff _ _ old new =
    let relevantPatch patch _ = case patch of
          Nothing -> Just Nothing
          Just _ -> Nothing
    in Compose $ Map.differenceWith relevantPatch (getCompose old) (getCompose new)
  applyDiff _ _ patch old = Compose $ RD.applyMap (getCompose $ fmap Just patch) (getCompose old)

listHoldWithKeyMap::forall t m k v a. (RD.DomBuilder t m, R.MonadHold t m,Ord k)=>Map k v->R.Event t (Map k (Maybe v))->(k->v->m a)->m (R.Dynamic t (Map k a))
listHoldWithKeyMap = listHoldWithKeyGeneral

listHoldWithKeyMap'::forall t m k v a. (RD.DomBuilder t m, R.MonadHold t m,Ord k)=>Map k v->R.Event t (Map k (Maybe v))->(k->v->m a)->m (R.Dynamic t (Map k a))
listHoldWithKeyMap' c diffCEv f = listHoldWithKeyGeneral' c (Compose <$> diffCEv) f 

listWithKeyShallowDiffMap::forall t m k v a. (RD.DomBuilder t m, MonadFix m, R.MonadHold t m, Ord k)
  => Map k v -> R.Event t (Map k (Maybe v)) -> (k -> v -> R.Event t v -> m a) -> m (R.Dynamic t (Map k a))
listWithKeyShallowDiffMap c diffCEv f = listWithKeyShallowDiffGeneral c (Compose <$> diffCEv) f

intMapWithFunctorToDMap :: IntMap (f v) -> DMap (Const2 Int v) f
intMapWithFunctorToDMap = DM.fromDistinctAscList . fmap (\(k, v) -> Const2 k :=> v) . IM.toAscList

dmapToIntMap :: DMap (Const2 Int v) Identity -> IntMap v
dmapToIntMap = IM.fromDistinctAscList . fmap (\(Const2 k :=> Identity v) -> (k, v)) . DM.toAscList

instance ListHoldable IntMap Int v a where
  type DMapKey IntMap Int a = Const2 Int a
  type LHPatch IntMap Int v = IntMap (Maybe v)
  type DMapPatchType IntMap Int a = PatchDMap 
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
  type DMapPatchType (HashMap k) k a = PatchDMap 
  toDMapWithFunctor h  = hashMapWithFunctorToDMap . HM.mapWithKey h
  makePatch _ h = PatchDMap . hashMapWithFunctorToDMap .HM.mapWithKey (\k mv -> ComposeMaybe $ fmap (h k) mv)
  fromDMap _ _ = dmapToHashMap

listHoldWithKeyHashMap::forall t m k v a. (RD.DomBuilder t m, R.MonadHold t m,Ord k, Hashable k)
  =>HashMap k v->R.Event t (HashMap k (Maybe v))->(k->v->m a)->m (R.Dynamic t (HashMap k a))
listHoldWithKeyHashMap = listHoldWithKeyGeneral

