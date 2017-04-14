{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances  #-}
module Reflex.Dom.Contrib.FormBuilder.Instances.Containers (buildListWithSelect) where


import Reflex.Dom.Contrib.ReflexConstraints (MonadWidgetExtraC)
-- All the basic (primitive types, tuples, etc.) are in here
import Reflex.Dom.Contrib.FormBuilder.Instances.Basic (FormInstanceC,dynAsEv,traceDynAsEv)
import Reflex.Dom.Contrib.FormBuilder.Builder
import Reflex.Dom.Contrib.FormBuilder.DynValidation (accValidation)
import Reflex.Dom.Contrib.Layout.Types (LayoutOrientation(..))
import Reflex.Dom.Contrib.DynamicUtils (dynAsEv,traceDynAsEv,mDynAsEv)
import qualified Reflex.Dom.Contrib.ListHoldFunctions.Maps as LHF
import Reflex.Dom.Contrib.ListHoldFunctions.Maps (LHFMap(..))

-- reflex imports
import qualified Reflex as R 
import qualified Reflex.Dom as RD
import Reflex.Dom ((=:))

import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (lift,local)
import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad.Morph (hoist)
import Control.Lens (view,(&),(.~))
import Data.Functor.Compose (Compose(Compose,getCompose))
import qualified Data.Text as T
import           Text.Read                        (readMaybe)
import Data.Validation (AccValidation(..))
import qualified Data.Foldable as F
import           Safe                              (headMay)
import Control.Arrow ((&&&))


-- imports only to make instances
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe,catMaybes,isNothing,fromJust)
import Data.Monoid ((<>))
-- my libs
import qualified DataBuilder as B

import           Text.Read                        (readMaybe)

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

buildAdjustableContainer::(FormInstanceC t m
                          , LHFMap g
                          , LHFMapKey g ~ k
                          , Functor g                          
                          , Traversable g
                          , Ord k
                          , VFormBuilderC t m k
                          , VFormBuilderC t m v)
  =>MapLike f g v 
  ->MapElemWidgets g t m k v
  ->BuildForm t m (f v)
buildAdjustableContainer ml mews va mFN dmfa  = validateForm va . makeForm $ do
    fType <- getFormType
    case fType of
      Interactive ->  unF $ buildLBAddDelete ml mews mFN dmfa 
      ObserveOnly ->  unF $ buildLBEditOnly  ml mews mFN dmfa

buildAdjustableContainerWithSelect::(FormInstanceC t m
                                    , LHFMap g
                                    , LHFMapKey g ~ k
                                    , Functor g                          
                                    , Traversable g
                                    , Ord k
                                    , VFormBuilderC t m k
                                    , VFormBuilderC t m v)
  =>(g v -> M.Map k T.Text)
  ->MapLike f g v 
  ->MapElemWidgets g t m k v
  ->BuildForm t m (f v)
buildAdjustableContainerWithSelect labelKeys ml mews va mFN dmfa  = validateForm va . makeForm $ do
    fType <- getFormType
    case fType of
      Interactive ->  unF $ buildLBWithSelect labelKeys ml mews mFN dmfa 
      ObserveOnly ->  unF $ buildLBWithSelect labelKeys ml mews mFN dmfa
  

mapML::Ord k=>MapLike (M.Map k) (M.Map k) v
mapML = MapLike id id LHF.lhfMapDiffNoEq 

mapEQML::(Ord k, Eq v)=>MapLike (M.Map k) (M.Map k) v
mapEQML = mapML { diffMap = LHF.lhfMapDiff }

mapEditWidget::(FormInstanceC t m, VFormBuilderC t m k, VFormBuilderC t m v)=>R.Dynamic t (M.Map k (FValidation v)) -> R.Event t (k,v) -> R.Event t (M.Map k v) -> FRW t m (k,v)
mapEditWidget _ newPair newMap = do
  let pairWidget = unF $ buildForm' Nothing (constDynMaybe Nothing)
      pairWidgetEv = pairWidget <$ R.leftmost [() <$ newPair, () <$ newMap]
  joinDynOfDynValidation <$> RD.widgetHold pairWidget pairWidgetEv -- DynValidation (k,v)

mapWidgets::(FormInstanceC t m, VFormBuilderC t m k, VFormBuilderC t m v)=>MapElemWidgets (M.Map k) t m k v
mapWidgets = MapElemWidgets showKeyEditVal mapEditWidget

buildMap::(FormInstanceC t m,Ord k, VFormBuilderC t m k, VFormBuilderC t m v)=>BuildForm t m (M.Map k v)
buildMap = buildAdjustableContainer mapML mapWidgets

buildEqMap::(FormInstanceC t m,Ord k, Eq v, VFormBuilderC t m k, VFormBuilderC t m v)=>BuildForm t m (M.Map k v)
buildEqMap = buildAdjustableContainer mapEQML mapWidgets

instance (FormInstanceC t m, Ord k, VFormBuilderC t m k, VFormBuilderC t m a)=>FormBuilder t m (M.Map k a) where
  buildForm =  buildMap

listML::MapLike [] IM.IntMap v
listML = MapLike (IM.fromAscList . zip [0..]) (fmap snd . IM.toList) LHF.lhfMapDiffNoEq

listEqML::Eq a=>MapLike [] IM.IntMap a
listEqML = listML { diffMap = LHF.lhfMapDiff }

safeMaximum::Ord a=>[a]->Maybe a
safeMaximum l = if null l then Nothing else Just $ maximum l

listEditWidget::(FormInstanceC t m, VFormBuilderC t m a)=>R.Dynamic t (IM.IntMap (FValidation a)) -> R.Event t (Int, a) -> R.Event t (IM.IntMap a) -> FRW t m (Int,a)
listEditWidget _ newPairEv newMapEv = do
  let newKeyEv = R.leftmost [(+1) . fst <$>  newPairEv, maybe 0 (+1) . safeMaximum . IM.keys <$> newMapEv]
      widget newKey = fRow $ do
        newElem <- fItem . unF $ buildForm' Nothing (constDynMaybe Nothing)
        return $ (,) <$> constDynValidation newKey <*> newElem
  joinDynOfDynValidation <$> RD.widgetHold (widget 0) (widget <$> newKeyEv) -- DynValidation (k,v)

listEditWidgetNoDup::(FormInstanceC t m, VFormBuilderC t m a, Eq a)=>R.Dynamic t (IM.IntMap (FValidation a)) -> R.Event t (Int, a) -> R.Event t (IM.IntMap a) -> FRW t m (Int,a)
listEditWidgetNoDup mapVDyn newPairEv newMapEv = do
  let newKeyEv = R.leftmost [(+1) . fst <$>  newPairEv, maybe 0 (+1) . safeMaximum . IM.keys <$> newMapEv]
      widget newKey = fRow $ do
        newElem <- fItem . unF $ buildForm' Nothing (constDynMaybe Nothing)
        return $ (,) <$> constDynValidation newKey <*> newElem
      dupF (intKey,val) curMap = let isDup = L.elem val (IM.elems curMap) in if isDup then AccFailure [FNothing] else AccSuccess (intKey,val)
  newPair <- joinDynOfDynValidation <$> RD.widgetHold (widget 0) (widget <$> newKeyEv) -- DynValidation (k,v)
  return . DynValidation . fmap mergeAccValidation . unDynValidation $ dupF <$> newPair <*> (DynValidation $ sequenceA <$> mapVDyn)

  
listWidgets::(FormInstanceC t m, VFormBuilderC t m a)=>MapElemWidgets IM.IntMap t m Int a
listWidgets = MapElemWidgets hideKeyEditVal listEditWidget

buildList::(FormInstanceC t m, VFormBuilderC t m a)=>BuildForm t m [a]
buildList = buildAdjustableContainer listML listWidgets

buildListWithSelect::(FormInstanceC t m, VFormBuilderC t m a)=>BuildForm t m [a]
buildListWithSelect = buildAdjustableContainerWithSelect (M.fromList . fmap (id &&& (T.pack . show)) . IM.keys) listML listWidgets

buildEqList::(FormInstanceC t m, Eq a, VFormBuilderC t m a, Eq a)=>BuildForm t m [a]
buildEqList = buildAdjustableContainer listEqML listWidgets

instance (FormInstanceC t m, VFormBuilderC t m a)=>FormBuilder t m [a] where
  buildForm =  buildList

setEqML::Ord a=>MapLike S.Set IM.IntMap a
setEqML = MapLike (IM.fromAscList . zip [0..] . S.toList) (S.fromList . fmap snd . IM.toList) LHF.lhfMapDiff

setEqWidgets::(FormInstanceC t m, VFormBuilderC t m a,Eq a)=>MapElemWidgets IM.IntMap t m Int a
setEqWidgets = MapElemWidgets hideKeyEditVal listEditWidgetNoDup

buildSet::(FormInstanceC t m, VFormBuilderC t m a, Ord a)=>BuildForm t m (S.Set a)
buildSet = buildAdjustableContainer setEqML setEqWidgets

instance (FormInstanceC t m, VFormBuilderC t m a, Ord a)=>FormBuilder t m (S.Set a) where
  buildForm = buildSet

intMapEditWidget::(FormInstanceC t m, VFormBuilderC t m Int, VFormBuilderC t m v)
  =>R.Dynamic t (IM.IntMap (FValidation v)) -> R.Event t (Int,v) -> R.Event t (IM.IntMap v) -> FRW t m (Int,v)
intMapEditWidget _ newPair newMap = do
  let pairWidget = unF $ buildForm' Nothing (constDynMaybe Nothing)
      pairWidgetEv = pairWidget <$ R.leftmost [() <$ newPair, () <$ newMap]
  joinDynOfDynValidation <$> RD.widgetHold pairWidget pairWidgetEv -- DynValidation (Int,v)

intMapWidgets::(FormInstanceC t m, VFormBuilderC t m Int, VFormBuilderC t m v)=>MapElemWidgets IM.IntMap t m Int v
intMapWidgets = MapElemWidgets showKeyEditVal intMapEditWidget

intMapML::MapLike IM.IntMap IM.IntMap v
intMapML = MapLike id id  LHF.lhfMapDiffNoEq

intMapEqML::Eq v=>MapLike IM.IntMap IM.IntMap v
intMapEqML = intMapML { diffMap = LHF.lhfMapDiff } 

buildIntMap::(FormInstanceC t m, VFormBuilderC t m Int, VFormBuilderC t m a)=>BuildForm t m (IM.IntMap a)
buildIntMap = buildAdjustableContainer intMapML intMapWidgets

buildEqIntMap::(FormInstanceC t m, VFormBuilderC t m Int, VFormBuilderC t m a, Eq a)=>BuildForm t m (IM.IntMap a)
buildEqIntMap = buildAdjustableContainer intMapEqML intMapWidgets

instance (FormInstanceC t m, VFormBuilderC t m Int, VFormBuilderC t m a)=>FormBuilder t m (IM.IntMap a) where
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

hashMapEditWidget::(FormInstanceC t m, VFormBuilderC t m k, VFormBuilderC t m v)
  =>R.Dynamic t (HML.HashMap k (FValidation v)) -> R.Event t (k,v) -> R.Event t (HML.HashMap k v) -> FRW t m (k,v)
hashMapEditWidget _ newPair newMap = do
  let pairWidget = unF $ buildForm' Nothing (constDynMaybe Nothing)
      pairWidgetEv = pairWidget <$ R.leftmost [() <$ newPair, () <$ newMap]
  joinDynOfDynValidation <$> RD.widgetHold pairWidget pairWidgetEv -- DynValidation (k,v)

hashMapWidgets::(FormInstanceC t m, VFormBuilderC t m k, VFormBuilderC t m v)=>MapElemWidgets (HML.HashMap k) t m k v
hashMapWidgets = MapElemWidgets showKeyEditVal hashMapEditWidget

hashMapML::(Ord k, Hashable k)=>MapLike (HML.HashMap k) (HML.HashMap k) v
hashMapML = MapLike id id LHF.lhfMapDiffNoEq 

hashMapEqML::(Hashable k, Ord k, Eq v)=>MapLike (HML.HashMap k) (HML.HashMap k) v
hashMapEqML = hashMapML { diffMap = LHF.lhfMapDiff }

buildHashMap::(FormInstanceC t m,Ord k, Hashable k, VFormBuilderC t m k, VFormBuilderC t m v)=>BuildForm t m (HML.HashMap k v)
buildHashMap = buildAdjustableContainer hashMapML hashMapWidgets

buildEqHashMap::(FormInstanceC t m,Ord k, Hashable k, VFormBuilderC t m k, VFormBuilderC t m v, Eq v)=>BuildForm t m (HML.HashMap k v)
buildEqHashMap = buildAdjustableContainer hashMapEqML hashMapWidgets

instance (FormInstanceC t m, VFormBuilderC t m k, VFormBuilderC t m v, Ord k, Hashable k, Eq k)=>FormBuilder t m (HML.HashMap k v) where
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
type LBWidget t m k v = k->R.Dynamic t v->FR t m (R.Dynamic t (Maybe (FValidation v)))

elemWidgetToLBWidget::(R.Reflex t, Functor m)=>ElemWidget t m k v->LBWidget t m k v
elemWidgetToLBWidget ew k vDyn = fmap Just . unDynValidation <$> ew k vDyn

class IsMap m k | m->k where
  emptyMap::m a
  mapMaybe::(a -> Maybe b) -> m a -> m b
  distributeOverDynPure::R.Reflex t=> m (R.Dynamic t v) -> R.Dynamic t (m v)
  singleton::k->v->m v
  applyMapDiff::m (Maybe v) -> m v -> m v

maybeMapToMap::LHFMap f=>Maybe (f v) -> f v
maybeMapToMap = fromMaybe lhfEmptyMap 

buildLBEditOnly::(FormInstanceC t m
                 , LHFMap g
                 , LHFMapKey g ~ k
                 , Traversable g
                 , VFormBuilderC t m k
                 , VFormBuilderC t m v)
  =>MapLike f g v 
  ->MapElemWidgets g t m k v
  ->Maybe FieldName
  ->DynMaybe t (f v)
  ->Form t m (f v)
buildLBEditOnly (MapLike to from _) (MapElemWidgets eW _) mFN dmfa =  makeForm $ do
  let mapDyn0 = fmap maybeMapToMap . getCompose $ to <$> dmfa
  fmap from <$> buildLBEMapLWK' (elemWidgetToLBWidget eW) mFN mapDyn0

buildLBDelete::(FormInstanceC t m
               , LHFMap g
               , LHFMapKey g ~ k
               , Traversable g
               , VFormBuilderC t m k
               , VFormBuilderC t m v)
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
buildLBAddDelete::(FormInstanceC t m
                  , Functor g
                  , Traversable g
                  , LHFMap g
                  , LHFMapKey g ~ k
                  , VFormBuilderC t m k
                  , VFormBuilderC t m v)
  =>MapLike f g v
  ->MapElemWidgets g t m k v
  ->Maybe FieldName
  ->DynMaybe t (f v)
  ->Form t m (f v)
buildLBAddDelete (MapLike to from diffMapF) (MapElemWidgets eW nWF) mFN dmfa = makeForm $ fCol $ mdo
  let eW' k v0 vEv = R.holdDyn v0 vEv >>= \vDyn -> editAndDeleteElemWidget eW (R.constDyn True) k vDyn
      mapDyn0 = fmap maybeMapToMap . getCompose $ to <$> dmfa 
      diffMap' avOld new = case avOld of
        AccSuccess old -> diffMapF old new
        AccFailure _ -> Just <$> new
  newInputMapEv <- dynAsEv mapDyn0 
  updateMapDyn <- fItem $ LHF.listWithKeyShallowDiffLHFMap lhfEmptyMap diffMapEv eW' -- Dynamic t (Map k (Dynamic t (Maybe (FValidation v))))
  addEv <- fRow $ mdo
    addPairDV <- fRow $ nWF mapDyn newPairEv newInputMapEv
    let newPairMaybeDyn = avToMaybe <$> unDynValidation addPairDV
    addButtonEv <- fItem $ buttonNoSubmit' "+" -- Event t ()
    let newPairEv = R.fmapMaybe id $ R.tag (R.current newPairMaybeDyn) addButtonEv
    return newPairEv
  let newInputDiffEv = R.attachWith diffMap' (R.current $ sequenceA <$> mapDyn) newInputMapEv -- Event t (Map k (Maybe v))
      insertDiffEv = fmap Just . uncurry lhfMapSingleton <$> addEv  
      diffMapEv = R.leftmost [newInputDiffEv, insertDiffEv]
      mapEditsFVEv = R.updated . join $ LHF.distributeLHFMapOverDynPure <$> updateMapDyn -- Event t (Map k (Maybe (FValidation v)))
      editedMapEv = R.attachWith (flip LHF.lhfMapApplyDiff) (R.current mapDyn) mapEditsFVEv -- Event t (Map k (FValidation v))
  mapDyn <- R.holdDyn lhfEmptyMap $ R.leftmost
            [
              fmap AccSuccess <$> newInputMapEv
            , editedMapEv
            ]
  return . DynValidation $ fmap from . sequenceA <$> mapDyn



dynValidationToDynamicMaybe::R.Reflex t=>DynValidation t a -> R.Dynamic t (Maybe a)
dynValidationToDynamicMaybe = fmap avToMaybe . unDynValidation 

-- simplest.  Use listWithKey.  This will be for ReadOnly and fixed element (no adds or deletes allowed) uses.
-- Just make widget into right form and do the distribute over the result
buildLBEMapLWK'::(FormInstanceC t m
                 , Traversable g
                 , LHFMap g
                 , LHFMapKey g ~ k
                 , VFormBuilderC t m k
                 , VFormBuilderC t m v)
  =>LBWidget t m k v->LBBuildF' g t m k v
buildLBEMapLWK' editW _ mapDyn0 = do
  mapDynEv <- traceDynAsEv (const "buildLBEMapLWK'") mapDyn0
  mapDyn' <- R.holdDyn lhfEmptyMap mapDynEv
  mapOfDyn <- LHF.listWithKeyLHFMap (R.traceDynWith (const "LWK' mapDyn0") mapDyn') editW -- Dynamic t (M.Map k (Dynamic t (Maybe (FValidation v)))
  let mapFValDyn = lhfMapMaybe id <$> (join $ LHF.distributeLHFMapOverDynPure <$> mapOfDyn) -- Dynamic t (Map k (FValidation v))
  return . DynValidation $ sequenceA <$> mapFValDyn

boolToEither::Bool -> Either () ()
boolToEither True = Right ()
boolToEither False = Left ()

-- NB: right event fires if true, left if false--(FalseEv,TrueEv)--which fits Either but is not intuitive, at least to me
fanBool::R.Reflex t=>R.Event t Bool->(R.Event t (), R.Event t ())
fanBool = R.fanEither . fmap boolToEither

toSVLWKWidget::(RD.DomBuilder t m
               ,RD.PostBuild t m)=>ElemWidget t m k v -> (k -> R.Dynamic t v -> R.Dynamic t Bool -> FR t m (R.Event t (FValidation v)))
toSVLWKWidget ew k dv db = do
   let divAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> db
   dva <- RD.elDynAttr "div" divAttrs $ ew k dv
   return $ R.updated $ unDynValidation dva  

-- something using selectView
buildSelectViewer::(FormInstanceC t m
                   , Traversable g
                   , LHFMap g
                   , LHFMapKey g ~ k
                   , VFormBuilderC t m k
                   , VFormBuilderC t m v)
  =>(g v->M.Map k T.Text)->MapElemWidgets g t m k v->LBBuildF' g t m k v
buildSelectViewer labelKeys (MapElemWidgets eW nWF) mFN dgvIn = fCol $ mdo
  newInputContainerEv <- dynAsEv dgvIn -- generate events when the input changes
  
  -- chooser widget with dropdown
  (maybeSelDyn, editedMapEv) <- selectWidget labelKeys eW dgvForDD
--  let editDiffEv = fmap avToMaybe . uncurry lhfMapSingleton <$> mapEditEv
--      editedMapEv = R.attachWith (flip LHF.lhfMapApplyDiff) (R.current dgv) editDiffEv  -- has edits; these don't get fed back in.

  -- we should make the button inactive if the container is empty
  deleteButtonEv <- fRow $ buttonNoSubmit' "delete"
  let deleteEv = R.fmapMaybe id $ R.tag (R.current maybeSelDyn) deleteButtonEv  -- Event t k, only fires if there is a current selection
      deleteDiffEv = flip lhfMapSingleton Nothing <$> deleteEv -- Event t (k, Nothing)  
        
  addEv <- fRow $ mdo -- Event t (k, v)
    addPairDV <- fRow $ nWF (fmap AccSuccess <$> dgv) newPairEv newInputContainerEv
    let newPairMaybeDyn = avToMaybe <$> unDynValidation addPairDV
    addButtonEv <- fItem $ buttonNoSubmit' "+" -- Event t ()
    let newPairEv = R.fmapMaybe id $ R.tag (R.current newPairMaybeDyn) addButtonEv
    return newPairEv
    
  let insertDiffEv = fmap Just . uncurry lhfMapSingleton <$> addEv
      insertDeleteDiffEv = R.leftmost [insertDiffEv, deleteDiffEv]
      mapAfterInsertDeleteEv = R.attachWith (flip LHF.lhfMapApplyDiff) (R.current dgv) insertDeleteDiffEv
      
  let updatedMapForDDEv = R.leftmost [newInputContainerEv, mapAfterInsertDeleteEv] --widget already knows about edits
      updatedMapEv = R.leftmost [updatedMapForDDEv, editedMapEv] -- order matters here.  mapEditEv on new map will not have the whole map.  Arbitrary patch.

  dgvForDD <- R.holdDyn lhfEmptyMap updatedMapForDDEv -- input to dropdown widget 
  dgv <- R.holdDyn lhfEmptyMap updatedMapEv -- authoritative value of edited container
  return $ DynValidation $ AccSuccess <$> dgv


selectWidget::(FormInstanceC t m
              , Traversable g
              , LHFMap g
              , LHFMapKey g ~ k
              , VFormBuilderC t m k
              , VFormBuilderC t m v)
  =>(g v->M.Map k T.Text)->ElemWidget t m k v->R.Dynamic t (g v)->FR t m (R.Dynamic t (Maybe k), R.Event t (g v))
selectWidget labelKeys eW dgv = do
  -- we need to deal differently with the null and non-null container case
  -- and we only want to know when we've changed from one to the other
  let (nonNullEv,nullEv) = fanBool . R.updated . R.uniqDyn $ lhfMapNull <$> dgv
      nullWidget = RD.el "div" (RD.text "Empty Container") >> return (R.constDyn Nothing, R.never)
      nullWidgetEv = nullWidget <$ nullEv
      -- This one needs to be prompt since the container just became non-null
      defaultKeyEv = R.fmapMaybe id $ R.tagPromptlyDyn (headMay . lhfMapKeys <$> dgv) nonNullEv -- headMay and fmapMaybe id are redundant here but...
      sWidget k0 = selectWidgetWithDefault labelKeys eW k0 dgv
      widgetEv = R.leftmost [nullWidgetEv, sWidget <$> defaultKeyEv]
  selWidget <- fRow $ RD.widgetHold nullWidget widgetEv
  let maybeSelDyn =  join $ fst <$> selWidget -- Dynamic t (Maybe k)
      mapEditEvDyn = snd <$> selWidget
  mapEditEvBeh <- R.hold R.never (R.updated mapEditEvDyn)
  let mapEditEv = R.switch mapEditEvBeh 
      editDiffEv = fmap avToMaybe . uncurry lhfMapSingleton <$> mapEditEv
      editedMapEv = R.attachWith (flip LHF.lhfMapApplyDiff) (R.current dgv) editDiffEv  -- has edits; these don't get fed back in.  
  return (maybeSelDyn, editedMapEv)


selectWidgetWithDefault::(FormInstanceC t m
                         , Traversable g
                         , LHFMap g
                         , LHFMapKey g ~ k
                         , VFormBuilderC t m k
                         , VFormBuilderC t m v)
  =>(g v->M.Map k T.Text)->ElemWidget t m k v->k->R.Dynamic t (g v)->FR t m (R.Dynamic t (Maybe k), R.Event t (k, FValidation v))
selectWidgetWithDefault labelKeys eW k0 dgv = mdo
  let keyLabelMap = labelKeys <$> dgv -- dynamic map for the dropdown/chooser.  dgvForDD will change on input change or new element add. Not edits. Deletes?
      ddConfig = RD.def & RD.dropdownConfig_attributes .~ R.constDyn ("size" =: "1")
      newK0 oldK0 m = if M.member oldK0 m then Nothing else headMay $ M.keys m  -- compute new default key           
      newk0Ev = R.attachWithMaybe newK0 (R.current k0Dyn) (R.updated keyLabelMap) -- has to be old k0, otherwise causality loop
      dropdownWidget k =  RD._dropdown_value <$> RD.dropdown k keyLabelMap ddConfig 
  k0Dyn <- R.holdDyn k0 newk0Ev 
  selDyn <- join <$> RD.widgetHold (dropdownWidget k0) (dropdownWidget <$> newk0Ev)        
  editEv <- LHF.selectViewListWithKeyLHFMap selDyn dgv (toSVLWKWidget eW)  -- NB: this map doesn't need updating from edits or deletes
  return (Just <$> selDyn, editEv) -- we need the selection to make the delete button work
  
    
buildLBWithSelect::(FormInstanceC t m
                   , LHFMap g
                   , LHFMapKey g ~ k
                   , Traversable g
                   , VFormBuilderC t m k
                   , VFormBuilderC t m v)
  =>(g v -> M.Map k T.Text)
  ->MapLike f g v 
  ->MapElemWidgets g t m k v
  ->Maybe FieldName
  ->DynMaybe t (f v)
  ->Form t m (f v)
buildLBWithSelect labelKeys (MapLike to from _) widgets mFN dmfa =  makeForm $ do
  let mapDyn0 = fmap maybeMapToMap . getCompose $ to <$> dmfa
  fmap from <$> buildSelectViewer labelKeys widgets mFN mapDyn0


{-
buildLBWithSelectEditOnly::(FormInstanceC t m
                           , LHFMap g
                           , LHFMapKey g ~ k
                           , Traversable g
                           , VFormBuilderC t m k
                           , VFormBuilderC t m v)
  =>(g v -> M.Map k T.Text)
  ->MapLike f g v 
  ->MapElemWidgets g t m k v
  ->Maybe FieldName
  ->DynMaybe t (f v)
  ->Form t m (f v)
buildLBWithSelectEditOnly labelKeys (MapLike to from _) (MapElemWidgets eW _)  mFN dmfv =  makeForm $ do
  let toG x = fmap maybeMapToMap . getCompose $ to <$> x
      dgv = toG dmfv
  newInputContainerEv <- dynAsEv dgv -- generate events when the input changes
  (_,editedMapEv) <- selectWidget labelKeys eW dgv
  res <- R.holdDyn lhfEmptyMap (R.leftmost [newInputContainerEv, editedMapEv])
  return . from . DynValidation $ AccSuccess <$> res
-}



    
showKeyEditVal::(FormInstanceC t m
                , VFormBuilderC t m k
                , VFormBuilderC t m v)=>ElemWidget t m k v
showKeyEditVal k vDyn = do
  vEv <- traceDynAsEv (const "showKeyEditVal") vDyn
  mvDyn <- R.holdDyn Nothing (Just <$> vEv)
  let showKey k = toReadOnly $ buildForm' Nothing (constDynMaybe (Just k))
  fRow $ do
    fItem . unF $ showKey k
    fItem . unF $ buildForm' Nothing (Compose mvDyn)


hideKeyEditVal::(FormInstanceC t m, VFormBuilderC t m v)=>ElemWidget t m k v
hideKeyEditVal _ vDyn = do
  vEv <- traceDynAsEv (const "hideKeyEditVal") vDyn
  mvDyn <- R.holdDyn Nothing (Just <$> vEv)
  fRow . fItem . unF $ buildForm' Nothing (Compose mvDyn)


editAndDeleteElemWidget::(FormInstanceC t m
                         , VFormBuilderC t m k
                         , VFormBuilderC t m v)
  =>ElemWidget t m k v->R.Dynamic t Bool->LBWidget t m k v
editAndDeleteElemWidget eW visibleDyn k vDyn = mdo
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> visibleDyn'
  (visibleDyn',outDyn') <- RD.elDynAttr "div" widgetAttrs . fRow $ do
    resDyn <- fItem $ eW k vDyn -- DynValidation t v
    resEv <- dynAsEv $ unDynValidation resDyn -- Event t (FValidation v)  
    delButtonEv <- fItem $ buttonNoSubmit' "-"
    selEv <- dynAsEv visibleDyn
    visDyn <-  R.holdDyn True $ R.leftmost
               [
                 selEv
               , False <$ delButtonEv -- delete button pressed, so hide
               , True <$ resEv -- value updated so make sure it's visible (in case of re-use of deleted key)
               ]
    outDyn <- R.holdDyn Nothing $ R.leftmost
              [
                Just <$> resEv
              , Nothing <$ delButtonEv
              ]
    return (visDyn,outDyn)
  return outDyn'


hiddenCSS::M.Map T.Text T.Text
hiddenCSS  = "style" =: "display: none"
visibleCSS::M.Map T.Text T.Text
visibleCSS = "style" =: "display: inline"

ddAttrsDyn::R.Reflex t=>(Int->Int)->R.Dynamic t Int->R.Dynamic t RD.AttributeMap
ddAttrsDyn sizeF = fmap (\n->if n==0 then hiddenCSS else visibleCSS <> ("size" =: (T.pack . show $ sizeF n)))



-- unused from here down but good for reference
type LBBuildF t m k v = Maybe FieldName->R.Dynamic t (M.Map k v)->FR t m (R.Dynamic t (M.Map k v))

buildLBEMapWithAdd::(FormInstanceC t m
                    , VFormBuilderC t m k
                    , VFormBuilderC t m v
                    , Ord k)
                  =>LBBuildF t m k v -- simple builder
                  ->LBBuildF t m k v
buildLBEMapWithAdd lbbf mFN mapDyn0 = fCol $ mdo
  initialMapEv <- dynAsEv mapDyn0
  editedMapDyn <- fItem $ lbbf mFN mapDyn -- Dynamic t (M.Map k v)
  addEv <- fRow $ mdo -- Event t (k,v)
    let newOneWidget = fmap avToMaybe . unDynValidation <$> (fRow . unF $ buildForm' Nothing (constDynMaybe Nothing)) -- m (Dynamic t (Maybe (k,v))
        addWidget = join <$> RD.widgetHold newOneWidget (newOneWidget <$ addButtonEv) 
    newOneDyn <- fItem addWidget -- Dynamic t (Maybe (k,v))
    addButtonEv <- fCenter LayoutVertical . fItemR . lift $ containerActionButton "+" -- Event t ()
    return $ R.attachWithMaybe const (R.current newOneDyn) addButtonEv -- fires only if newOneDyn is (Just x)
  let mapWithAdditionEv = R.attachWith (\m (k,v)->M.insert k v m) (R.current editedMapDyn) addEv
  mapDyn <- R.holdDyn M.empty (R.leftmost [initialMapEv, mapWithAdditionEv])
  return editedMapDyn


-- simplest.  Use listWithKey.  This will be for ReadOnly and fixed element (no adds or deletes allowed) uses. 
buildLBEMapLWK::(FormInstanceC t m
                 , VFormBuilderC t m v
                 , Ord k, Show k)
               =>LBBuildF t m k v
buildLBEMapLWK mFN map0Dyn = do
  mapOfDynMaybe <- LHF.listWithKeyLHFMap (R.traceDynWith (\m -> "LWK map0Dyn: " ++ show (M.keys m)) map0Dyn) editOne
  return $ M.mapMaybe id <$> (join $ R.distributeMapOverDynPure <$> mapOfDynMaybe)


editOne::(FormInstanceC t m, VFormBuilderC t m v, Show k)=>k->R.Dynamic t v->FR t m (R.Dynamic t (Maybe v))
editOne k valDyn = do
  fItem $ RD.el "div" $ RD.el "p" $ RD.text (T.pack $ show k)
  fItem $ fmap avToMaybe . unDynValidation <$> (unF $ buildForm' Nothing (Compose $ Just <$> R.traceDynWith (const "editOne valDyn") valDyn))

-- now do with ListViewWithKey so we can put in delete events
-- NB: ListViewWithKey returns an Event t (M.Map k v) but it contains only the keys for which things have changed
-- NB: ListViewWithKey gets only mapDyn0 as input.  Only need to update if something *else* changes the map.
buildLBEMapLVWK::(FormInstanceC t m
                  , VFormBuilderC t m v
                  , Ord k , Show k)
                => LBBuildF t m k v
buildLBEMapLVWK mFN mapDyn0 = mdo
  let editF k valDyn = R.updated <$> editOne k valDyn -- editOneEv (R.constDyn True) k valDyn
  newInputMapEv <- traceDynAsEv (\m->"LVWK mapDyn0" ++ show (M.keys m)) mapDyn0
  mapEditsEv  <- R.traceEventWith (\m->"LVWK mapEditsEv: " ++ show (M.keys m)) <$> RD.listViewWithKey mapDyn0 editF -- Event t (M.Map k (Maybe v)), carries only updates
  let editedMapEv = R.traceEventWith (\m->"LVWK editedMap: " ++ show (M.keys m)) $ R.attachWith (flip RD.applyMap) (R.current mapDyn) mapEditsEv
      mapEv = R.leftmost [newInputMapEv, editedMapEv]
  mapDyn <- R.holdDyn M.empty mapEv
  return (R.traceDynWith (\m -> "LVWK mapDyn: " ++ show (M.keys m)) mapDyn)




