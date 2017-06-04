{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE UndecidableInstances   #-}
-- | Module with machinery for building reflex-dom forms/dynamic editors from containers.
--
-- This module also contains FormBuilder instances for the various basic container types (Map, List, Set, IntMap, Sequence, HashMap, HashSet)
-- These are based on mapping the container to something map-like (Map, IntMap, HashMap)
module Reflex.Dom.Contrib.FormBuilder.Instances.Containers
  (
    -- * Container form builders
    buildListWithSelect
  , buildList
  , buildEqList
  , buildMap
  , buildMapWithSelect
  , buildMapEditOnly
  , buildEqMap
  , buildEqMapEditOnly
  , buildSet
  , buildIntMap
  , buildEqIntMap
  , buildSequence
  , buildEqSequence
  , buildHashMap
  , buildEqHashMap
  , buildEqHashSet

  -- * Types for mapping containers to builders
  , MapLike(..)
  , MapElemWidgets(..)
  , buildAdjustableContainer
  , buildAdjustableContainerWithSelect
  ) where


import           Reflex.Dom.Contrib.ReflexConstraints (MonadWidgetExtraC)
import           Reflex.Dom.Contrib.FormBuilder.Instances.Basic (FormInstanceC)
import           Reflex.Dom.Contrib.FormBuilder.Builder (DynMaybe(..), Form(..), FR, FRW, unF, FieldName, VFormBuilderC
                                                        , FormBuilder(buildForm), makeForm, buildVForm, getFormType, toReadOnly
                                                        , FValidation, validateForm
                                                        , constDynMaybe, FormType(..), FormValidator
                                                        , fItem, fItemR, fRow, fCol, fCenter, fFill, avToMaybe, joinDynOfFormResults)
import           Reflex.Dom.Contrib.FormBuilder.DynValidation (DynValidation(..),constDynValidation,joinDynOfDynValidation
                                                              ,accValidation, mergeAccValidation, FormErrors, FormError(FNothing), avToEither)
import Reflex.Dom.Contrib.Widgets.WidgetResult (WidgetResult, buildWidgetResult, dynamicToWidgetResult, currentWidgetResult,
                                                dynamicWidgetResultToWidgetResult, buildReadOnlyWidgetResult, updatedWidgetResult, widgetResultToDynamic)                 
import           Reflex.Dom.Contrib.Layout.Types (LayoutOrientation(..), LayoutDirection (..))
import           Reflex.Dom.Contrib.DynamicUtils (dynBasedOn, dynAsEv, traceDynAsEv, mDynAsEv)
import Reflex.Dom.Contrib.EventUtils (leftWhenNotRight)
import qualified Reflex.Dom.Contrib.ListHoldFunctions.Maps as LHF
import           Reflex.Dom.Contrib.ListHoldFunctions.Maps (LHFMap(..))
import qualified Reflex.Dom.Contrib.Widgets.ModalEditor as MW 

-- reflex imports
import qualified Reflex as R 
import qualified Reflex.Dom as RD
import           Reflex.Dom ((=:))
import qualified Reflex.Dom.Contrib.Widgets.Modal as RDC

import           Control.Monad (join)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.Reader (lift,local)
import           Control.Monad.State (StateT, runStateT, get, put)
import           Control.Monad.Morph (hoist)
import           Control.Lens (view,(&),(.~))
import           Data.Functor.Compose (Compose(Compose,getCompose))
import qualified Data.Text as T
import           Text.Read                        (readMaybe)
import           Data.Validation (AccValidation(..))
import qualified Data.Foldable as F
import           Safe                              (headMay)
import           Control.Arrow ((&&&))


-- imports only to make instances
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashSet as HS
import           Data.Maybe (fromMaybe,catMaybes,isNothing,fromJust)
import           Data.Monoid ((<>))
-- my libs
import qualified DataBuilder as B


-- Container instances
-- Editing/Appendability requires that the container be isomorphic to something traversable, but traversable in the (key,value) pairs for maps.
-- and the ability to make an empty traversable and insert new items/pairs.
-- Deletability requires some way of knowing which element to delete *in the traversable container rather than the original*.
-- If this uses an index, it might require some state as the traversable container is rendered.


-- I'd prefer these as classes but I can't make all the types work.  I end up with open type families and injectivity issues.
-- So, instead, I carry the dictionaries around as arguments.  That works too.


{-
-- default behavior of button in a form is to submit the form
-- This creates a button but without that default
buttonNoSubmit :: forall t m .RD.DomBuilder t m => T.Text -> m (RD.Event t ())
buttonNoSubmit t = do
  let pd::RD.EventSpec (RD.DomBuilderSpace m) RD.EventResult -> RD.EventSpec (RD.DomBuilderSpace m) RD.EventResult
      pd x =  RD.addEventSpecFlags (Proxy::Proxy (RD.DomBuilderSpace m)) RD.Click (const RD.preventDefault) x
      pdCfg::RD.ElementConfig RD.EventResult t m -> RD.ElementConfig RD.EventResult t m
      pdCfg x = RD.elementConfig_eventSpec %~ pd $ x
  (e, _) <- RD.element "button" (pdCfg def) $ RD.text t
  return $ RD.domEvent RD.Click e

clickableLabel::RD.DomBuilder t m=>T.Text -> m (RD.Event t ())
clickableLabel t = do
  (e,_) <- RD.element "label" def $ RD.text t
  return $ RD.domEvent RD.Click e

checkBoxClicker::(RD.PostBuild t m, RD.DomBuilder t m) => T.Text -> m (RD.Event t ())
checkBoxClicker _ = fmap void (RD._checkbox_change <$> RD.checkbox False def)
-}

buttonNoSubmit'::RD.DomBuilder t m=>T.Text -> m (RD.Event t ())
buttonNoSubmit' t = (RD.domEvent RD.Click . fst) <$> RD.elAttr' "button" ("type" RD.=: "button") (RD.text t)
  
  
containerActionButton::(RD.PostBuild t m, RD.DomBuilder t m)=>T.Text -> m (RD.Event t ())
containerActionButton = buttonNoSubmit'

-- TODO: Make this f T.Text and generalize dropdown to accept any LHFMap
data LabelStrategy k v = LabelValues (v -> T.Text) | LabelKeys (k -> T.Text) 
labelLHFMap::(LHFMap f,Ord (LHFMapKey f))=>LabelStrategy (LHFMapKey f) v -> f v -> M.Map (LHFMapKey f)  T.Text
labelLHFMap (LabelValues lv) = M.fromList . uncurry zip . (lhfMapKeys &&& (fmap lv . lhfMapElems)) 
labelLHFMap (LabelKeys   lk) = M.fromList . uncurry zip . (id &&& fmap lk) . lhfMapKeys  

type VFormBuilderBoth t m a b = (VFormBuilderC t m a, VFormBuilderC t m b)
type LHFMapForForm g k = (Functor g, Traversable g, LHFMap g, LHFMapKey g ~ k, Ord k) 
type ContainerForm t m g k v = (FormInstanceC t m, LHFMapForForm g k, VFormBuilderBoth t m k v)

buildContainerEditOnly::ContainerForm t m g k v
  =>MapLike f g v
  ->MapElemWidgets g t m k v
  ->BuildForm t m (f v)
buildContainerEditOnly ml mews va mFN = validateForm va . buildLBEditOnly ml mews mFN


buildAdjustableContainer::ContainerForm t m g k v
  =>MapLike f g v
  ->MapElemWidgets g t m k v
  ->BuildForm t m (f v)
buildAdjustableContainer ml mews va mFN dmfa  = validateForm va . makeForm $ do
    fType <- getFormType
    case fType of
      Interactive ->  unF $ buildLBAddDelete ml mews mFN dmfa 
      ObserveOnly ->  unF $ buildLBEditOnly  ml mews mFN dmfa

buildAdjustableContainerWithSelect::ContainerForm t m g k v
  =>LabelStrategy k v
  ->MapLike f g v 
  ->MapElemWidgets g t m k v
  ->BuildForm t m (f v)
buildAdjustableContainerWithSelect labelStrategy ml mews va mFN dmfa  = validateForm va . makeForm $ do
    fType <- getFormType
    case fType of
      Interactive ->  unF $ buildLBWithSelect labelStrategy ml mews mFN dmfa 
      ObserveOnly ->  unF $ buildLBWithSelectEditOnly labelStrategy ml mews mFN dmfa

mapML::Ord k=>MapLike (M.Map k) (M.Map k) v
mapML = MapLike id id LHF.lhfMapDiffNoEq 

mapEQML::(Ord k, Eq v)=>MapLike (M.Map k) (M.Map k) v
mapEQML = mapML { diffMap = LHF.lhfMapDiff }

mapEditWidget::(FormInstanceC t m, VFormBuilderBoth t m k v)
  =>R.Dynamic t (M.Map k (FValidation v)) -> R.Event t (k,v) -> R.Event t (M.Map k v) -> FRW t m (k,v)
mapEditWidget _ newPair newMap = do
  let pairWidget = unF $ buildVForm Nothing (constDynMaybe Nothing)
      pairWidgetEv = pairWidget <$ R.leftmost [() <$ newPair, () <$ newMap]
  joinDynOfFormResults <$> RD.widgetHold pairWidget pairWidgetEv -- FormResult t (k,v)

mapWidgets::(FormInstanceC t m, VFormBuilderBoth t m k v)=>MapElemWidgets (M.Map k) t m k v
mapWidgets = MapElemWidgets showKeyEditVal mapEditWidget

buildMap::(FormInstanceC t m,Ord k, VFormBuilderBoth t m k v)=>BuildForm t m (M.Map k v)
buildMap = buildAdjustableContainer mapML mapWidgets

buildMapWithSelect :: (FormInstanceC t m, Ord k, Show k, VFormBuilderBoth t m k v) => BuildForm t m (M.Map k v)
buildMapWithSelect = buildAdjustableContainerWithSelect (LabelKeys (T.pack . show)) mapML mapWidgets

buildMapEditOnly :: (FormInstanceC t m,Ord k, VFormBuilderBoth t m k v)=>BuildForm t m (M.Map k v)
buildMapEditOnly = buildContainerEditOnly mapML mapWidgets

buildEqMap::(FormInstanceC t m,Ord k, Eq v, VFormBuilderBoth t m k v)=>BuildForm t m (M.Map k v)
buildEqMap = buildAdjustableContainer mapEQML mapWidgets

buildEqMapEditOnly :: (FormInstanceC t m, Ord k, Eq v, VFormBuilderBoth t m k v)=>BuildForm t m (M.Map k v)
buildEqMapEditOnly = buildContainerEditOnly mapEQML mapWidgets


instance (FormInstanceC t m, Ord k, VFormBuilderBoth t m k a)=>FormBuilder t m (M.Map k a) where
  buildForm =  buildMap

listML::MapLike [] IM.IntMap v
listML = MapLike (IM.fromAscList . zip [0..]) (fmap snd . IM.toList) LHF.lhfMapDiffNoEq

listEqML::Eq a=>MapLike [] IM.IntMap a
listEqML = listML { diffMap = LHF.lhfMapDiff }

safeMaximum::Ord a=>[a]->Maybe a
safeMaximum l = if null l then Nothing else Just $ maximum l

listEditWidget::(FormInstanceC t m, VFormBuilderC t m a)
  => R.Dynamic t (IM.IntMap (FValidation a))
  -> R.Event t (Int, a)
  -> R.Event t (IM.IntMap a)
  -> FRW t m (Int,a)
listEditWidget mapDyn newPairEv newMapEv = do
  inMapEv <- dynAsEv mapDyn
  let keyFromOldKey = (+1)
      keyFromIntMap :: forall b. IM.IntMap b -> Int
      keyFromIntMap = maybe 0 keyFromOldKey . safeMaximum . IM.keys
      newKeyEv = R.leftmost $ [ keyFromOldKey . fst <$>  newPairEv
                              , keyFromIntMap <$> inMapEv
                              , keyFromIntMap <$> newMapEv
                              ]
      widget newKey = fRow $ do
        newElem <- fItem . unF $ buildVForm Nothing (constDynMaybe Nothing)
        return $ (,) <$> pure newKey <*> newElem
  joinDynOfFormResults <$> RD.widgetHold (widget 0) (widget <$> newKeyEv)

listEditWidgetNoDup::(FormInstanceC t m
                     , VFormBuilderC t m a
                     , Eq a)
  => R.Dynamic t (IM.IntMap (FValidation a))
  -> R.Event t (Int, a)
  -> R.Event t (IM.IntMap a)
  -> FRW t m (Int,a)
listEditWidgetNoDup mapVDyn newPairEv newMapEv = do
  let dupF (intKey,val) curMap = let isDup = L.elem val (IM.elems curMap) in if isDup then AccFailure [FNothing] else AccSuccess (intKey,val)
  newPair <- listEditWidget mapVDyn newPairEv newMapEv
  return . Compose . fmap mergeAccValidation . getCompose $ dupF <$> newPair <*> (Compose $ sequenceA <$> buildReadOnlyWidgetResult mapVDyn)

  
listWidgets::(FormInstanceC t m, VFormBuilderC t m a)=>MapElemWidgets IM.IntMap t m Int a
listWidgets = MapElemWidgets hideKeyEditVal listEditWidget

buildList::(FormInstanceC t m, VFormBuilderC t m a)=>BuildForm t m [a]
buildList = buildAdjustableContainer listML listWidgets

buildListWithSelect::(FormInstanceC t m, VFormBuilderC t m a)=>BuildForm t m [a]
buildListWithSelect = buildAdjustableContainerWithSelect (LabelKeys (T.pack . show)) listML listWidgets

buildEqList::(FormInstanceC t m, VFormBuilderC t m a, Eq a)=>BuildForm t m [a]
buildEqList = buildAdjustableContainer listEqML listWidgets

instance (FormInstanceC t m, VFormBuilderC t m a)=>FormBuilder t m [a] where
  buildForm =  buildList

setEqML::Ord a=>MapLike S.Set IM.IntMap a
setEqML = MapLike (IM.fromAscList . zip [0..] . S.toList) (S.fromList . fmap snd . IM.toList) LHF.lhfMapDiffNoEq

setEqWidgets::(FormInstanceC t m, VFormBuilderC t m a,Eq a)=>MapElemWidgets IM.IntMap t m Int a
setEqWidgets = MapElemWidgets hideKeyEditVal listEditWidgetNoDup

buildSet::(FormInstanceC t m, VFormBuilderC t m a, Ord a)=>BuildForm t m (S.Set a)
buildSet = buildAdjustableContainer setEqML setEqWidgets

instance (FormInstanceC t m, VFormBuilderC t m a, Ord a)=>FormBuilder t m (S.Set a) where
  buildForm = buildSet

intMapEditWidget::(FormInstanceC t m, VFormBuilderBoth t m Int v)
  =>R.Dynamic t (IM.IntMap (FValidation v)) -> R.Event t (Int,v) -> R.Event t (IM.IntMap v) -> FRW t m (Int,v)
intMapEditWidget _ newPair newMap = do
  let pairWidget = unF $ buildVForm Nothing (constDynMaybe Nothing)
      pairWidgetEv = pairWidget <$ R.leftmost [() <$ newPair, () <$ newMap]
  joinDynOfFormResults <$> RD.widgetHold pairWidget pairWidgetEv -- FormResult (Int,v)

intMapWidgets::(FormInstanceC t m, VFormBuilderBoth t m Int v)=>MapElemWidgets IM.IntMap t m Int v
intMapWidgets = MapElemWidgets showKeyEditVal intMapEditWidget

intMapML::MapLike IM.IntMap IM.IntMap v
intMapML = MapLike id id  LHF.lhfMapDiffNoEq

intMapEqML::Eq v=>MapLike IM.IntMap IM.IntMap v
intMapEqML = intMapML { diffMap = LHF.lhfMapDiff } 

buildIntMap::(FormInstanceC t m, VFormBuilderBoth t m Int a)=>BuildForm t m (IM.IntMap a)
buildIntMap = buildAdjustableContainer intMapML intMapWidgets

buildEqIntMap::(FormInstanceC t m, VFormBuilderBoth t m Int a, Eq a)=>BuildForm t m (IM.IntMap a)
buildEqIntMap = buildAdjustableContainer intMapEqML intMapWidgets

instance (FormInstanceC t m, VFormBuilderBoth t m Int a)=>FormBuilder t m (IM.IntMap a) where
  buildForm = buildIntMap
    
sequenceML::MapLike Seq.Seq IM.IntMap v
sequenceML = MapLike (IM.fromAscList . zip [0..] . F.toList) (Seq.fromList . fmap snd . IM.toList) LHF.lhfMapDiffNoEq

sequenceEqML::Eq a=>MapLike Seq.Seq IM.IntMap a
sequenceEqML = sequenceML { diffMap = LHF.lhfMapDiff }

buildSequence::(FormInstanceC t m, VFormBuilderC t m a)=>BuildForm t m (Seq.Seq a)
buildSequence  = buildAdjustableContainer sequenceML listWidgets

buildEqSequence::(FormInstanceC t m, VFormBuilderC t m a, Eq a)=>BuildForm t m (Seq.Seq a)
buildEqSequence  = buildAdjustableContainer sequenceEqML listWidgets

instance (FormInstanceC t m, VFormBuilderC t m a)=>FormBuilder t m (Seq.Seq a) where
  buildForm = buildSequence

hashMapEditWidget::(FormInstanceC t m, VFormBuilderBoth t m k v)
  =>R.Dynamic t (HML.HashMap k (FValidation v))
  -> R.Event t (k,v)
  -> R.Event t (HML.HashMap k v)
  -> FRW t m (k,v)
hashMapEditWidget _ newPair newMap = do
  let pairWidget = unF $ buildVForm Nothing (constDynMaybe Nothing)
      pairWidgetEv = pairWidget <$ R.leftmost [() <$ newPair, () <$ newMap]
  joinDynOfFormResults <$> RD.widgetHold pairWidget pairWidgetEv -- DynValidation (k,v)

hashMapWidgets::(FormInstanceC t m, VFormBuilderC t m k, VFormBuilderC t m v)=>MapElemWidgets (HML.HashMap k) t m k v
hashMapWidgets = MapElemWidgets showKeyEditVal hashMapEditWidget

hashMapML::(Ord k, Hashable k)=>MapLike (HML.HashMap k) (HML.HashMap k) v
hashMapML = MapLike id id LHF.lhfMapDiffNoEq 

hashMapEqML::(Hashable k, Ord k, Eq v)=>MapLike (HML.HashMap k) (HML.HashMap k) v
hashMapEqML = hashMapML { diffMap = LHF.lhfMapDiff }

buildHashMap::(FormInstanceC t m,Ord k, Hashable k, VFormBuilderBoth t m k v)
  =>BuildForm t m (HML.HashMap k v)
buildHashMap = buildAdjustableContainer hashMapML hashMapWidgets

buildEqHashMap::(FormInstanceC t m,Ord k, Hashable k, VFormBuilderBoth t m k v, Eq v)
  =>BuildForm t m (HML.HashMap k v)
buildEqHashMap = buildAdjustableContainer hashMapEqML hashMapWidgets

instance (FormInstanceC t m
         , VFormBuilderBoth t m k v
         , Ord k
         , Hashable k
         , Eq k)=>FormBuilder t m (HML.HashMap k v) where
  buildForm = buildHashMap

-- Can't do plain HashSet since we need (Eq a) for fromList. So we may as well take advantage in the diffing

hashSetEqML::(Eq a, Hashable a)=>MapLike HS.HashSet IM.IntMap a
hashSetEqML = MapLike (IM.fromList . zip [0..] . HS.toList) (HS.fromList . fmap snd . IM.toList) LHF.lhfMapDiff

buildEqHashSet::(FormInstanceC t m,Hashable a, Eq a, VFormBuilderC t m a)=>BuildForm t m (HS.HashSet a)
buildEqHashSet = buildAdjustableContainer hashSetEqML setEqWidgets

instance (FormInstanceC t m, VFormBuilderC t m a, Hashable a, Eq a)=>FormBuilder t m (HS.HashSet a) where
  buildForm = buildEqHashSet

  
-- the various container builder components
type BuildF t m a    = FormValidator a->Maybe FieldName->DynMaybe t a->FRW t m a
type BuildForm t m a = FormValidator a->Maybe FieldName->DynMaybe t a->Form t m a

type LBBuildF' g t m k v = Maybe FieldName->R.Dynamic t (g v)->FRW t m (g v)

data MapLike f g v = MapLike { toMap::f v->g v
                             , fromMap::g v->f v
                             , diffMap::g v -> g v -> g (Maybe v)
                             }

data MapElemWidgets g t m k v = MapElemWidgets { elemW::ElemWidget t m k v
                                               , newOneWF::R.Dynamic t (g (FValidation v)) -> R.Event t (k,v)->R.Event t (g v)->FRW t m (k,v)
                                               }



instance (B.Validatable FValidation a, B.Validatable FValidation b)=>B.Validatable FValidation (a,b) where
  validator (a,b) = (,) <$> B.validator a <*> B.validator b


type ElemWidget t m k v = k->R.Dynamic t v->FRW t m v 
type LBWidget t m k v = k->R.Dynamic t v->FR t m (WidgetResult t (Maybe (FValidation v)))

elemWidgetToLBWidget::(R.Reflex t, Functor m)=>ElemWidget t m k v->LBWidget t m k v
elemWidgetToLBWidget ew k vDyn = fmap Just . getCompose <$> ew k vDyn

maybeMapToMap::LHFMap f=>Maybe (f v) -> f v
maybeMapToMap = fromMaybe lhfEmptyMap 

buildLBEditOnly::ContainerForm t m g k v
  =>MapLike f g v 
  ->MapElemWidgets g t m k v
  ->Maybe FieldName
  ->DynMaybe t (f v)
  ->Form t m (f v)
buildLBEditOnly (MapLike to from _) (MapElemWidgets eW _) mFN dmfa =  makeForm $ do
  let mapDyn0 = fmap maybeMapToMap . getCompose $ to <$> dmfa
  fmap from <$> buildLBEMapLWK' (elemWidgetToLBWidget eW) mFN mapDyn0

buildLBDelete::ContainerForm t m g k v
  =>MapLike f g v 
  ->MapElemWidgets g t m k v
  ->Maybe FieldName
  ->DynMaybe t (f v)
  ->Form t m (f v)
buildLBDelete (MapLike to from _) (MapElemWidgets eW _) mFN dmfa = makeForm $ do
  let eW' = editAndDeleteElemWidget eW (R.constDyn True)
      mapDyn0 = fmap maybeMapToMap . getCompose $ to <$> dmfa
  fmap from <$> buildLBEMapLWK' eW' mFN mapDyn0      



--NB: I see why tagPromtlyDyn is required for pairWidgetEv (so when the new one is drawn, it sees the updated map) but not quite why
-- that doesn't lead to a cycle.
-- Should we do something better with the WidgetResults in updateMapDyn?  Get a Map k (Event) and then mergeMap? 
buildLBAddDelete::ContainerForm t m g k v
  =>MapLike f g v
  ->MapElemWidgets g t m k v
  ->Maybe FieldName
  ->DynMaybe t (f v)
  ->Form t m (f v)
buildLBAddDelete (MapLike to from diffMapF) (MapElemWidgets eW nWF) mFN dmfa = makeForm $ fCol $ mdo
  let eW' k v0 vEv = R.holdDyn v0 vEv >>= editAndDeleteElemWidget eW (R.constDyn True) k
      mapDyn0 = fmap maybeMapToMap . getCompose $ to <$> dmfa
      mapDynAV0 = fmap AccSuccess <$> mapDyn0
      diffMap' avOld new = case avOld of
        AccSuccess old -> diffMapF old new
        AccFailure _ -> Just <$> new
  newInputMapEv <- dynAsEv mapDyn0 
  updateMapDyn <- fItem $ LHF.listWithKeyShallowDiffLHFMap lhfEmptyMap diffMapEv eW' -- Dynamic t (Map k (WidgetResult t (Maybe (FValidation v))))
  
  insertDiffEv <- fRow $ newItemWidget nWF mapDyn newInputMapEv
  let mapAfterInsertEv = R.attachWith (flip LHF.lhfMapApplyDiff) (R.current mapDyn) (fmap (fmap AccSuccess) <$> insertDiffEv)
      newInputDiffEv = R.attachWith diffMap' (R.current $ sequenceA <$> mapDyn) newInputMapEv --newInputMapEv -- Event t (Map k (Maybe v))
      diffMapEv = R.leftmost [newInputDiffEv, insertDiffEv]
      mapEditsFVEv = R.updated . join $ LHF.distributeLHFMapOverDynPure . fmap widgetResultToDynamic <$> updateMapDyn -- Event t (Map k (Maybe (FValidation v))) ??
      editedMapEv = R.attachWith (flip LHF.lhfMapApplyDiff) (R.current mapDyn) mapEditsFVEv -- Event t (Map k (FValidation v))
  mapDyn <- R.buildDynamic (R.sample $ R.current mapDynAV0) $ R.leftmost [ R.updated mapDynAV0
                                                                         , editedMapEv
                                                                         ]
  wr <- fmap (fmap from . sequenceA) <$> (buildWidgetResult mapDynAV0 $ R.leftmost [mapAfterInsertEv, editedMapEv])
  return $ Compose wr

{-
newItemWidget' :: ContainerForm t m g k v
  => (R.Dynamic t (g (FValidation v)) -> R.Event t (k,v) -> R.Event t (g v) -> FRW t m (k,v))
  -> R.Dynamic t (g (FValidation v))
  -> R.Event t (g v)
  -> FR t m (R.Event t (g (Maybe v)))
newItemWidget' editPairW mapDyn newInputMapEv = mdo
  addPairDV <- fRow $ editPairW mapDyn newPairEv newInputMapEv
  let newPairMaybeDyn = avToMaybe <$> getCompose addPairDV
  addButtonEv <- fItem $ buttonNoSubmit' "+" -- Event t ()
  let newPairEv = R.fmapMaybe id $ R.tag (R.current newPairMaybeDyn) addButtonEv
      addDiffEv = fmap Just . uncurry lhfMapSingleton <$> newPairEv  
  return addDiffEv
-}

newItemEditorConfig :: R.Reflex t => MW.ModalEditorConfig t a
newItemEditorConfig = RD.def
                      & MW.modalEditor_closeOnOk .~ True
                      & MW.modalEditor_openButton .~ const (MW.ButtonConfig "Add" M.empty (Just "fa fa-plus"))
                      & MW.modalEditor_XButton .~ Nothing 
                      & MW.modalEditor_OkButton .~ const (MW.ButtonConfig "OK" M.empty (Just "fa fa-check"))
                      & MW.modalEditor_CancelButton .~ const (MW.ButtonConfig "Cancel" M.empty (Just "fa fa-window-close"))


newItemWidget :: ContainerForm t m g k v
  => (R.Dynamic t (g (FValidation v)) -> R.Event t (k,v) -> R.Event t (g v) -> FRW t m (k,v))
  -> R.Dynamic t (g (FValidation v))
  -> R.Event t (g v)
  -> FR t m (R.Event t (g (Maybe v)))
newItemWidget editPairW mapDyn newInputMapEv = mdo
  let modalEditW = const $ widgetResultToDynamic . fmap avToEither . getCompose <$> editPairW mapDyn newPairEv newInputMapEv
      blankInput = R.constDyn $ Left [FNothing]
      pairEvToDiffEv pairEv = fmap Just . uncurry lhfMapSingleton <$> pairEv 
  newPairEv <- MW.modalEditor_change <$> MW.modalEditorEither modalEditW blankInput newItemEditorConfig
  return $ pairEvToDiffEv newPairEv
  

dynValidationToDynamicMaybe::R.Reflex t=>DynValidation t a -> R.Dynamic t (Maybe a)
dynValidationToDynamicMaybe = fmap avToMaybe . unDynValidation 

-- simplest.  Use listWithKey.  This will be for ReadOnly and fixed element (no adds or deletes allowed) uses.
-- Just make widget into right form and do the distribute over the result
-- Should we do something better with the WidgetResults in updateMapDyn?  Get a Map k (Event ) and then mergeMap? 
buildLBEMapLWK'::ContainerForm t m g k v=>LBWidget t m k v->LBBuildF' g t m k v
buildLBEMapLWK' editW _ mapDyn0 = do
  mapDynEv <- dynAsEv mapDyn0 
  mapDyn' <- R.holdDyn lhfEmptyMap mapDynEv
  mapOfDyn <- LHF.listWithKeyLHFMap mapDyn' editW -- Dynamic t (M.Map k (WidgetResult t (Maybe (FValidation v)))
  let mapFValDyn = lhfMapMaybe id <$> join (LHF.distributeLHFMapOverDynPure . fmap widgetResultToDynamic <$> mapOfDyn) -- Dynamic t (Map k (FValidation v))
  return $ Compose $ dynamicToWidgetResult $ sequenceA <$> mapFValDyn

boolToEither::Bool -> Either () ()
boolToEither True = Right ()
boolToEither False = Left ()

-- NB: right event fires if true, left if false--(FalseEv,TrueEv)--which fits Either but is not intuitive, at least to me
fanBool::R.Reflex t=>R.Event t Bool->(R.Event t (), R.Event t ())
fanBool = R.fanEither . fmap boolToEither

-- in final line do we want all the events in the WidgetResult or just the updates?
toSVLWKWidget::(RD.DomBuilder t m
               ,RD.PostBuild t m)
  =>ElemWidget t m k v -> (k -> R.Dynamic t v -> R.Dynamic t Bool -> FR t m (R.Event t (FValidation v)))
toSVLWKWidget ew k dv db = do
   let divAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> db
   fra <- RD.elDynAttr "div" divAttrs $ ew k dv
   return $ R.updated $ widgetResultToDynamic $ getCompose fra  

-- something using selectView
buildSelectViewer::(FormInstanceC t m
                   , Traversable g
                   , LHFMap g
                   , LHFMapKey g ~ k
                   , VFormBuilderBoth t m k v)
  =>LabelStrategy k v->MapElemWidgets g t m k v->LBBuildF' g t m k v
buildSelectViewer labelStrategy (MapElemWidgets eW nWF) mFN dgvIn = fCol $ mdo
  newInputContainerEv <- dynAsEv dgvIn -- generate events when the input changes
  
  -- chooser widget with dropdown
  (maybeSelDyn, editedMapEv) <- selectWidget labelStrategy eW dgvForDD

  -- we should make the button inactive if the container is empty
  deleteButtonEv <- fRow $ buttonNoSubmit' "delete"
  let deleteEv = R.fmapMaybe id $ R.tag (R.current maybeSelDyn) deleteButtonEv  -- Event t k, only fires if there is a current selection
      deleteDiffEv = flip lhfMapSingleton Nothing <$> deleteEv -- Event t (k, Nothing)  
        
  insertDiffEv <- fRow $ newItemWidget nWF (fmap AccSuccess <$> dgv) newInputContainerEv
    
  let insertDeleteDiffEv = R.leftmost [insertDiffEv, deleteDiffEv]
      mapAfterInsertDeleteEv = R.attachWith (flip LHF.lhfMapApplyDiff) (R.current dgv) insertDeleteDiffEv
      
  let updatedMapForDDEv = R.leftmost [newInputContainerEv, mapAfterInsertDeleteEv] --widget already knows about edits
      updatedMapEv = R.leftmost [updatedMapForDDEv, editedMapEv] -- order matters here.  mapEditEv on new map will not have the whole map.  Arbitrary patch.

  dgvForDD <- R.holdDyn lhfEmptyMap updatedMapForDDEv -- input to dropdown widget 
  dgv <- R.holdDyn lhfEmptyMap updatedMapEv -- authoritative value of edited container
  Compose . fmap AccSuccess <$> (buildWidgetResult dgvIn $ R.leftmost [editedMapEv, mapAfterInsertDeleteEv])


selectWidget :: ( FormInstanceC t m
                , Traversable g
                , LHFMap g
                , LHFMapKey g ~ k
                , VFormBuilderBoth t m k v
                )
  => LabelStrategy k v
  -> ElemWidget t m k v
  -> R.Dynamic t (g v)
  -> FR t m (R.Dynamic t (Maybe k), R.Event t (g v))
selectWidget labelStrategy eW dgv = do
  -- we need to deal differently with the null and non-null container case
  -- and we only want to know when we've changed from one to the other
  inputNullEv <- R.holdUniqDyn (lhfMapNull <$> dgv) >>= dynAsEv --(R.uniqDyn $ lhfMapNull <$> dgv)
  let (nonNullEv,nullEv) = fanBool inputNullEv -- . R.updated . R.uniqDyn $ lhfMapNull <$> dgv
      nullWidget = RD.el "div" (RD.text "Empty Container") >> return (R.constDyn Nothing, R.never)
      nullWidgetEv = nullWidget <$ nullEv
      -- This one needs to be prompt since the container just became non-null
      defaultKeyEv = R.fmapMaybe id $ R.tagPromptlyDyn (headMay . lhfMapKeys <$> dgv) nonNullEv -- headMay and fmapMaybe id are redundant here but...
      sWidget k0 = selectWidgetWithDefault labelStrategy eW k0 dgv
      widgetEv = R.leftmost [nullWidgetEv, sWidget <$> defaultKeyEv]
  selWidget <- fRow $ RD.widgetHold nullWidget widgetEv
  let maybeSelDyn =  join $ fst <$> selWidget -- Dynamic t (Maybe k)
      mapEditEvDyn = snd <$> selWidget
  mapEditEvBeh <- R.hold R.never (R.updated mapEditEvDyn)
  let mapEditEv = R.switch mapEditEvBeh 
      editDiffEv = fmap avToMaybe . uncurry lhfMapSingleton <$> mapEditEv
      editedMapEv = R.attachWith (flip LHF.lhfMapApplyDiff) (R.current dgv) editDiffEv  -- has edits; these don't get fed back in.  
  return (maybeSelDyn, leftWhenNotRight editedMapEv (R.updated dgv))


selectWidgetWithDefault::ContainerForm t m g k v
  =>LabelStrategy k v
  ->ElemWidget t m k v
  ->k
  ->R.Dynamic t (g v)
  ->FR t m (R.Dynamic t (Maybe k), R.Event t (k, FValidation v))
selectWidgetWithDefault labelStrategy eW k0 dgv = mdo
  let keyLabelMap = labelLHFMap labelStrategy <$> dgv -- dynamic map for the dropdown/chooser.  dgvForDD will change on input change or new element add. Not edits. Deletes?
      newK0 oldK0 m = if M.member oldK0 m then Nothing else headMay $ M.keys m  -- compute new default key           
      newk0Ev = R.attachWithMaybe newK0 (R.current k0Dyn) (R.updated keyLabelMap) -- has to be old k0, otherwise causality loop
      ddConfig = RD.DropdownConfig newk0Ev (R.constDyn ("size" =: "1")) -- TODO: figure out how to build a multi-chooser.
      dropdownWidget k =  RD._dropdown_value <$> RD.dropdown k keyLabelMap ddConfig 
  k0Dyn <- R.holdDyn k0 newk0Ev 
  selDyn <- dropdownWidget k0
  editEv <- LHF.selectViewListWithKeyLHFMap selDyn dgv (toSVLWKWidget eW)  -- NB: this map doesn't need updating from edits or deletes
  return (Just <$> selDyn, editEv) -- we need the selection to make the delete button work
  
    
buildLBWithSelect::ContainerForm t m g k v
  =>LabelStrategy k v
  ->MapLike f g v 
  ->MapElemWidgets g t m k v
  ->Maybe FieldName
  ->DynMaybe t (f v)
  ->Form t m (f v)
buildLBWithSelect labelStrategy (MapLike to from _) widgets mFN dmfa =  makeForm $ do
  let mapDyn0 = fmap maybeMapToMap . getCompose $ to <$> dmfa
  fmap from <$> buildSelectViewer labelStrategy widgets mFN mapDyn0



buildLBWithSelectEditOnly::ContainerForm t m g k v
  =>LabelStrategy k v
  ->MapLike f g v 
  ->MapElemWidgets g t m k v
  ->Maybe FieldName
  ->DynMaybe t (f v)
  ->Form t m (f v)
buildLBWithSelectEditOnly labelStrategy (MapLike to from _) (MapElemWidgets eW _)  mFN dmfv =  makeForm $ do
  let dgv = fmap maybeMapToMap . getCompose $ to <$> dmfv
  (_, editedMapEv) <- selectWidget labelStrategy eW dgv
  res <- buildWidgetResult dgv editedMapEv
--  res <- R.buildDynamic (R.sample $ R.current dgv) $ R.leftmost [R.updated dgv, editedMapEv]
  return . Compose $ AccSuccess . from <$> res

    
showKeyEditVal::(FormInstanceC t m, VFormBuilderBoth t m k v)=>ElemWidget t m k v
showKeyEditVal k vDyn = do
  let showKey k = toReadOnly $ buildVForm Nothing (constDynMaybe (Just k))
  fRow $ do
    fItem . fFill LayoutRight . unF $ showKey k
    fItem . fFill LayoutLeft . unF $ buildVForm Nothing (Compose $ Just <$> vDyn)


hideKeyEditVal::(FormInstanceC t m, VFormBuilderC t m v)=>ElemWidget t m k v
hideKeyEditVal _ vDyn = do
  fItem . unF $ buildVForm Nothing (Compose $ Just <$> vDyn)

-- TODO: review dynamics vs WidgetResults here
editAndDeleteElemWidget::(FormInstanceC t m, VFormBuilderBoth t m k v)
  =>ElemWidget t m k v->R.Dynamic t Bool->LBWidget t m k v
editAndDeleteElemWidget eW visibleDyn k vDyn = mdo
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> visibleDyn'
  (visibleDyn', outWR') <- RD.elDynAttr "div" widgetAttrs . fRow $ do
    resWR <- getCompose <$> (fItem $ eW k vDyn) -- WidgetResult t (FValidation v)
--    resEv <- dynAsEv resDyn -- Event t (FValidation v)  
    delButtonEv <- fItem $ fFill LayoutLeft $ buttonNoSubmit' "-"
--    selEv <- dynAsEv visibleDyn
    visDyn <-  dynBasedOn visibleDyn $ R.leftmost
               [
                 R.updated visibleDyn
               , False <$ delButtonEv -- delete button pressed, so hide
               , True <$ (R.updated $ widgetResultToDynamic $ resWR) --resEv -- value updated so make sure it's visible (in case of re-use of deleted key)
               ]
    outWR <- buildWidgetResult (Just <$> widgetResultToDynamic resWR) $ Nothing <$ delButtonEv
    return (visDyn,outWR)
  return outWR'


hiddenCSS::M.Map T.Text T.Text
hiddenCSS  = "style" =: "display: none !important"
visibleCSS::M.Map T.Text T.Text
visibleCSS = "style" =: "display: inline"

ddAttrsDyn::R.Reflex t=>(Int->Int)->R.Dynamic t Int->R.Dynamic t RD.AttributeMap
ddAttrsDyn sizeF = fmap (\n->if n==0 then hiddenCSS else visibleCSS <> ("size" =: (T.pack . show $ sizeF n)))

-- unused from here down but good for reference
type LBBuildF t m k v = Maybe FieldName->R.Dynamic t (M.Map k v)->FR t m (WidgetResult t (M.Map k v))

buildLBEMapWithAdd::(FormInstanceC t m, VFormBuilderBoth t m k v, Ord k)
  =>LBBuildF t m k v->LBBuildF t m k v
buildLBEMapWithAdd lbbf mFN mapDyn0 = fCol $ mdo
  editedMapWR <- fItem $ lbbf mFN (widgetResultToDynamic mapWR) -- WidgetResult t (M.Map k v)
  addEv <- fRow $ mdo -- Event t (k,v)
    let newOneWidget = fmap avToMaybe . getCompose <$> (fRow . unF $ buildVForm Nothing (constDynMaybe Nothing)) -- m (WidgetResult t (Maybe (k,v))
        addWidget = dynamicWidgetResultToWidgetResult <$> RD.widgetHold newOneWidget (newOneWidget <$ addButtonEv) 
    newOneWR <- fItem addWidget -- WidgetResult t (Maybe (k,v))
    addButtonEv <- fCenter LayoutVertical . fItemR . lift $ containerActionButton "+" -- Event t ()
    return $ R.attachWithMaybe const (currentWidgetResult newOneWR) addButtonEv -- fires only if newOneWR is (Just x)
  let mapWithAdditionEv = R.attachWith (\m (k,v)->M.insert k v m) (currentWidgetResult editedMapWR) addEv
  mapWR <- buildWidgetResult mapDyn0 mapWithAdditionEv
  return editedMapWR


-- simplest.  Use listWithKey.  This will be for ReadOnly and fixed element (no adds or deletes allowed) uses. 
buildLBEMapLWK::(FormInstanceC t m, VFormBuilderC t m v, Ord k, Show k)=>LBBuildF t m k v
buildLBEMapLWK mFN map0Dyn = do
  mapOfDynMaybe <- fmap (fmap widgetResultToDynamic) <$> LHF.listWithKeyLHFMap map0Dyn editOne -- Dynamic t (Map k (Dynamic t (Maybe v)))
  return $ dynamicToWidgetResult $ M.mapMaybe id <$> join (R.distributeMapOverDynPure <$> mapOfDynMaybe)


editOne::(FormInstanceC t m, VFormBuilderC t m v, Show k)=>k->R.Dynamic t v->FR t m (WidgetResult t (Maybe v))
editOne k valDyn = do
  fItem $ RD.el "div" $ RD.el "p" $ RD.text (T.pack $ show k)
  fItem $ fmap avToMaybe . getCompose <$> unF (buildVForm Nothing (Compose $ Just <$> valDyn))

-- now do with ListViewWithKey so we can put in delete events
-- NB: ListViewWithKey returns an Event t (M.Map k v) but it contains only the keys for which things have changed
-- NB: ListViewWithKey gets only mapDyn0 as input.  Only need to update if something *else* changes the map.
buildLBEMapLVWK::(FormInstanceC t m, VFormBuilderC t m v, Ord k , Show k)=> LBBuildF t m k v
buildLBEMapLVWK mFN mapDyn0 = mdo
  let editF k valDyn = R.updated . widgetResultToDynamic <$> editOne k valDyn -- editOneEv (R.constDyn True) k valDyn
  mapEditsEv  <- RD.listViewWithKey mapDyn0 editF -- Event t (M.Map k (Maybe v)), carries only updates
  let editedMapEv = R.attachWith (flip RD.applyMap) (R.current mapDyn) mapEditsEv
      mapEv = R.leftmost [R.updated mapDyn0, editedMapEv]
  mapDyn <- dynBasedOn mapDyn0 mapEv
  buildWidgetResult mapDyn0 editedMapEv




