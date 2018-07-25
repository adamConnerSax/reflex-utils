{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Reflex.Dom.Contrib.FormBuilder.ListHoldFunctions
  (
    listHoldWithKeyMap
  , listWithKeyShallowDiffMap
  ) where

import qualified Reflex                 as R
import qualified Reflex.Dom             as RD
import           Reflex.Patch           (ComposeMaybe (..), PatchDMap (..))

import           Data.Dependent.Map     (DMap, DSum ((:=>)))
import qualified Data.Dependent.Map     as DM

import qualified Data.Foldable          as F

import           Data.Map               (Map)
import qualified Data.Map               as Map

import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IM

import           Data.Hashable
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM

import           Data.Array

import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Identity (Identity (..), void)
import           Data.Functor.Misc      (Const2 (..), dmapToMap,
                                         mapWithFunctorToDMap)

import           Data.Proxy             (Proxy (..))


listHoldWithKey :: forall t m k v a. (Ord k, RD.DomBuilder t m, R.MonadHold t m)
  => Map k v -> R.Event t (Map k (Maybe v)) -> (k -> v -> m a) -> m (R.Dynamic t (Map k a))
listHoldWithKey m0 m' f = do
  let dm0 = mapWithFunctorToDMap $ Map.mapWithKey f m0
      dm' = fmap (PatchDMap . mapWithFunctorToDMap . Map.mapWithKey (\k v -> ComposeMaybe $ fmap (f k) v)) m'
  (a0, a') <- R.sequenceDMapWithAdjust dm0 dm'
  fmap dmapToMap . R.incrementalToDynamic <$> R.holdIncremental a0 a' --TODO: Move the dmapToMap to the righthand side so it doesn't get fully redone every time

-- Can we get rid of the need for proxies here? AllowAmbiguousTypes and then use TypeApplication?
-- Can we put the GCompare constraint here?
class DM.GCompare (DMapKey f k a)=>ListHoldable (f :: * -> *) k v a where
  type DMapKey f k a :: * -> *
  type LHPatch f k v :: *
  toDMapWithFunctor::Functor g=>(k->v->g a)->f v->DM.DMap (DMapKey f k a) g
  makePatch::Proxy f -> (k->v->g a)->LHPatch f k v -> PatchDMap (DMapKey f k a) g
  fromDMap::Proxy k->Proxy v->DM.DMap (DMapKey f k a) Identity -> f a


class ShallowDiffable (f :: * -> *) k v where
  type SDPatch f k v :: *
  type SDPatchEventDKey f k v :: * -> *
  fanPatch::R.Reflex t=>Proxy f->Proxy k->Proxy v->R.Event t (SDPatch f k v) -> R.EventSelector t (SDPatchEventDKey f k v)
  emptyVoidPatch::Proxy f->Proxy k->Proxy v->SDPatch f k ()
  voidPatch::Proxy f->Proxy k->Proxy v->SDPatch f k v->SDPatch f k ()
  makePatchEventDKey::Proxy f -> Proxy v->k -> SDPatchEventDKey f k v v
  diffPatch::Proxy f->Proxy k->Proxy v->SDPatch f k v -> SDPatch f k () -> SDPatch f k v
  applyPatch::Proxy f->Proxy k->Proxy v->SDPatch f k () -> SDPatch f k () -> SDPatch f k () -- NB: this is a diff type from applyMap



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


listWithKeyShallowDiffGeneral :: forall t m f k v a.(RD.DomBuilder t m
                                                    , MonadFix m
                                                    , R.MonadHold t m
                                                    , ListHoldable f k v a
                                                    , ShallowDiffable f k v
                                                    , SDPatch f k v ~ LHPatch f k v) -- We could make ShallowDiff a superclass of ListHoldable ?
  => f v -> R.Event t (SDPatch f k v) -> (k -> v -> R.Event t v -> m a) -> m (R.Dynamic t (f a))
listWithKeyShallowDiffGeneral initialVals valsChanged mkChild = do
  let pf = Proxy :: Proxy f
      pk = Proxy :: Proxy k
      pv = Proxy :: Proxy v
      fanP = fanPatch pf pk pv
      emptyVoidP = emptyVoidPatch pf pk pv
      voidP = voidPatch pf pk pv
      applyP = applyPatch pf pk pv
      diffP = diffPatch pf pk pv
      makePK = makePatchEventDKey pf pv
      childValChangedSelector = fanP valsChanged
  sentVals <- R.foldDyn applyP emptyVoidP $ fmap voidP valsChanged
--  let relevantPatch patch _ = case patch of
--        Nothing -> Just Nothing -- Even if we let a Nothing through when the element doesn't already exist, this doesn't cause a problem because it is ignored
--        Just _ -> Nothing -- We don't want to let spurious re-creations of items through
  listHoldWithKeyGeneral initialVals (R.attachWith (flip diffP) (R.current sentVals) valsChanged) $ \k v ->
    mkChild k v $ R.select childValChangedSelector $ makePK k

instance Ord k=>ListHoldable (Map k) k v a where
  type DMapKey (Map k) k a = Const2 k a
  type LHPatch (Map k) k v = Map k (Maybe v)
  toDMapWithFunctor h  = mapWithFunctorToDMap . Map.mapWithKey h
  makePatch _ h = PatchDMap . mapWithFunctorToDMap . Map.mapWithKey (\k mv -> ComposeMaybe $ fmap (h k) mv)
  fromDMap _ _ = dmapToMap


instance Ord k=>ShallowDiffable (Map k) k v where
  type SDPatch (Map k) k v = Map k (Maybe v)
  type SDPatchEventDKey (Map k) k v = Const2 k v
  fanPatch _ _ _ = R.fanMap . fmap (Map.mapMaybe id)
  emptyVoidPatch _ _ _ = Map.empty
  voidPatch _ _ _ = fmap void
  makePatchEventDKey _ _ = Const2
  diffPatch _ _ _ =
    let relevantPatch patch _ = case patch of
          Nothing -> Just Nothing
          Just _  -> Nothing

    in Map.differenceWith relevantPatch
  applyPatch _ _ _ = applyPatchMap

applyPatchMap::Ord k=>Map k (Maybe v) -> Map k (Maybe v) -> Map k (Maybe v)
applyPatchMap patch old = insertions `Map.union` (old `Map.difference` deletions)
  where (deletions,insertions) = RD.mapPartitionEithers $ maybeToEither <$> patch
        maybeToEither = \case
          Nothing -> Left $ Just ()
          Just r -> Right $ Just r


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

arrayWithFunctorToDMap :: Ix k => Array k v -> DMap (Const2 k v) f
arrayWithFunctortoDMap = DM.fromList . fmap (\(k, v) -> Const2 k := v) . F.toList

dmapToArray :: Ix k => DMap (Const2 k v) Identity -> Array k v
dmapToArray dm =
  let kvPairs = fmap (\(Const2 k :=> Identity v) -> (k, v)) $ DM.toList dm
      keys = fst <$> kvPairs
      in A.array (min keys, max keys) kvPairs

arrayMapWithKey :: Ix k => (k -> v -> a) -> Array k v -> Array k a
arrayMapWithKey h a =
  let kvPairs = assocs a
      mapped = (\(k,v) -> (k, h k v)) <$> kvPairs
  in A.array (min . fst $ kvPairs, max . fst $ kvPairs) mapped

instance Ix k => ListHoldable (Array k v) k v a where
  type DmapKey (Array k) k a = Const2 k a
  type LHPatch (Array k) k v = Array k (Maybe v)
  toDMapWithFunctor h = arrayWithFunctorToDMap . arrayMapWithKey h
  makePatch _ h = PatchDMap . arrayWithFunctorToDMap . arrayMapWithKey (\k mv -> ComposeMaybe $ fmap (h k) mv)
  fromDMap _ _ = dmapToArray

listHoldWithKeyArray :: forall t m k v a. (RD.DomBuilder t m, R.MonadHold t m, Ix k)
  => Array k v -> R.Event t (Array k (Maybe v)) -> (k -> v -> m a) -> m (R.Dynamic t (Array k a))
listHoldWithKeyArray = listHoldWithKeyGeneral
