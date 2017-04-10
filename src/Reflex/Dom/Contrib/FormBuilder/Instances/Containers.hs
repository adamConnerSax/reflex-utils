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
{-# LANGUAGE UndecidableInstances  #-}
module Reflex.Dom.Contrib.FormBuilder.Instances.Containers () where


import Reflex.Dom.Contrib.ReflexConstraints (MonadWidgetExtraC)
-- All the basic (primitive types, tuples, etc.) are in here
import Reflex.Dom.Contrib.FormBuilder.Instances.Basic (FormInstanceC,dynAsEv,traceDynAsEv)
import Reflex.Dom.Contrib.FormBuilder.Builder
import Reflex.Dom.Contrib.FormBuilder.DynValidation (accValidation)
import Reflex.Dom.Contrib.Layout.Types (LayoutOrientation(..))
import Reflex.Dom.Contrib.DynamicUtils (dynAsEv,traceDynAsEv,mDynAsEv)
import Reflex.Dom.Contrib.ListHoldFunctions.Maps (listWithKeyShallowDiffMap, listWithKeyMap)

-- reflex imports
import qualified Reflex as R 
import qualified Reflex.Dom as RD
import Reflex.Dom ((=:))

import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (lift,local)
import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad.Morph (hoist)
import Control.Lens (view)
import Data.Functor.Compose (Compose(Compose,getCompose))
import qualified Data.Text as T
import           Text.Read                        (readMaybe)
import Data.Validation (AccValidation(..))
import qualified Data.Foldable as F
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
                          , Ord k
                          , VFormBuilderC t m k
                          , VFormBuilderC t m v)
  =>MapLike f a k v
  ->MapElemWidgets t m k v
  ->BuildForm t m (f a)
buildAdjustableContainer ml mews va mFN dmfa  = validateForm va . makeForm $ do
    fType <- getFormType
    case fType of
      Interactive ->  unF $ buildLBAddDelete ml mews mFN dmfa 
      ObserveOnly ->  unF $ buildLBEditOnly  ml mews mFN dmfa
  

mapML::Ord k=>MapLike (M.Map k) v k v
mapML = MapLike id id RD.diffMapNoEq

mapEQML::(Ord k, Eq v)=>MapLike (M.Map k) v k v
mapEQML = mapML { diffMap = RD.diffMap }

mapEditWidget::(FormInstanceC t m, VFormBuilderC t m k, VFormBuilderC t m v)=>R.Dynamic t (M.Map k (FValidation v)) -> R.Event t (k,v) -> R.Event t (M.Map k v) -> FRW t m (k,v)
mapEditWidget _ newPair newMap = do
  let pairWidget = unF $ buildForm' Nothing (constDynMaybe Nothing)
      pairWidgetEv = pairWidget <$ R.leftmost [() <$ newPair, () <$ newMap]
  joinDynOfDynValidation <$> RD.widgetHold pairWidget pairWidgetEv -- DynValidation (k,v)

mapWidgets::(FormInstanceC t m, VFormBuilderC t m k, VFormBuilderC t m v)=>MapElemWidgets t m k v
mapWidgets = MapElemWidgets showKeyEditVal mapEditWidget

buildMap::(FormInstanceC t m,Ord k, VFormBuilderC t m k, VFormBuilderC t m v)=>BuildForm t m (M.Map k v)
buildMap = buildAdjustableContainer mapML mapWidgets

buildEqMap::(FormInstanceC t m,Ord k, Eq v, VFormBuilderC t m k, VFormBuilderC t m v)=>BuildForm t m (M.Map k v)
buildEqMap = buildAdjustableContainer mapEQML mapWidgets

instance (FormInstanceC t m, Ord k, VFormBuilderC t m k, VFormBuilderC t m a)=>FormBuilder t m (M.Map k a) where
  buildForm =  buildMap

listML::MapLike [] a Int a
listML = MapLike (M.fromAscList . zip [0..]) (fmap snd . M.toList) RD.diffMapNoEq

listEqML::Eq a=>MapLike [] a Int a
listEqML = listML { diffMap = RD.diffMap }

safeMaximum::Ord a=>[a]->Maybe a
safeMaximum l = if null l then Nothing else Just $ maximum l

listEditWidget::(FormInstanceC t m, VFormBuilderC t m a)=>R.Dynamic t (M.Map Int (FValidation a)) -> R.Event t (Int, a) -> R.Event t (M.Map Int a) -> FRW t m (Int,a)
listEditWidget _ newPairEv newMapEv = do
  let newKeyEv = R.leftmost [(+1) . fst <$>  newPairEv, maybe 0 (+1) . safeMaximum . M.keys <$> newMapEv]
      widget newKey = fRow $ do
        newElem <- fItem . unF $ buildForm' Nothing (constDynMaybe Nothing)
        return $ (,) <$> constDynValidation newKey <*> newElem
  joinDynOfDynValidation <$> RD.widgetHold (widget 0) (widget <$> newKeyEv) -- DynValidation (k,v)

listEditWidgetNoDup::(FormInstanceC t m, VFormBuilderC t m a, Eq a)=>R.Dynamic t (M.Map Int (FValidation a)) -> R.Event t (Int, a) -> R.Event t (M.Map Int a) -> FRW t m (Int,a)
listEditWidgetNoDup mapVDyn newPairEv newMapEv = do
  let newKeyEv = R.leftmost [(+1) . fst <$>  newPairEv, maybe 0 (+1) . safeMaximum . M.keys <$> newMapEv]
      widget newKey = fRow $ do
        newElem <- fItem . unF $ buildForm' Nothing (constDynMaybe Nothing)
        return $ (,) <$> constDynValidation newKey <*> newElem
      dupF (intKey,val) curMap = let isDup = elem val (M.elems curMap) in if isDup then AccFailure [FNothing] else AccSuccess (intKey,val)
  newPair <- joinDynOfDynValidation <$> RD.widgetHold (widget 0) (widget <$> newKeyEv) -- DynValidation (k,v)
  return . DynValidation . fmap mergeAccValidation . unDynValidation $ dupF <$> newPair <*> (DynValidation $ sequenceA <$> mapVDyn)

  
listWidgets::(FormInstanceC t m, VFormBuilderC t m a)=>MapElemWidgets t m Int a
listWidgets = MapElemWidgets hideKeyEditVal listEditWidget

buildList::(FormInstanceC t m, VFormBuilderC t m a)=>BuildForm t m [a]
buildList = buildAdjustableContainer listML listWidgets

buildEqList::(FormInstanceC t m, Eq a, VFormBuilderC t m a, Eq a)=>BuildForm t m [a]
buildEqList = buildAdjustableContainer listEqML listWidgets

instance (FormInstanceC t m, VFormBuilderC t m a)=>FormBuilder t m [a] where
  buildForm =  buildList

setEqML::Ord a=>MapLike S.Set a Int a
setEqML = MapLike (M.fromAscList . zip [0..] . S.toList) (S.fromList . fmap snd . M.toList) RD.diffMap

setEqWidgets::(FormInstanceC t m, VFormBuilderC t m a,Eq a)=>MapElemWidgets t m Int a
setEqWidgets = MapElemWidgets hideKeyEditVal listEditWidgetNoDup

buildSet::(FormInstanceC t m, VFormBuilderC t m a, Ord a)=>BuildForm t m (S.Set a)
buildSet = buildAdjustableContainer setEqML setEqWidgets

instance (FormInstanceC t m, VFormBuilderC t m a, Ord a)=>FormBuilder t m (S.Set a) where
  buildForm = buildSet
  
intMapML::MapLike IM.IntMap v Int v
intMapML = MapLike (M.fromAscList . IM.toAscList) (IM.fromAscList . M.toAscList) RD.diffMapNoEq

intMapEqML::Eq v=>MapLike IM.IntMap v Int v
intMapEqML = intMapML { diffMap = RD.diffMap } 

intMapWidgets::(FormInstanceC t m, VFormBuilderC t m Int, VFormBuilderC t m a)=>MapElemWidgets t m Int a
intMapWidgets = MapElemWidgets showKeyEditVal mapEditWidget

buildIntMap::(FormInstanceC t m, VFormBuilderC t m Int, VFormBuilderC t m a)=>BuildForm t m (IM.IntMap a)
buildIntMap = buildAdjustableContainer intMapML intMapWidgets

buildEqIntMap::(FormInstanceC t m, VFormBuilderC t m Int, VFormBuilderC t m a, Eq a)=>BuildForm t m (IM.IntMap a)
buildEqIntMap = buildAdjustableContainer intMapEqML intMapWidgets

instance (FormInstanceC t m, VFormBuilderC t m Int, VFormBuilderC t m a)=>FormBuilder t m (IM.IntMap a) where
  buildForm = buildIntMap
    
sequenceML::MapLike Seq.Seq a Int a
sequenceML = MapLike (M.fromAscList . zip [0..] . F.toList) (Seq.fromList . fmap snd . M.toList) RD.diffMapNoEq

sequenceEqML::Eq a=>MapLike Seq.Seq a Int a
sequenceEqML = sequenceML { diffMap = RD.diffMap }

buildSequence::(FormInstanceC t m, VFormBuilderC t m a)=>BuildForm t m (Seq.Seq a)
buildSequence  = buildAdjustableContainer sequenceML listWidgets

buildEqSequence::(FormInstanceC t m, VFormBuilderC t m a, Eq a)=>BuildForm t m (Seq.Seq a)
buildEqSequence  = buildAdjustableContainer sequenceEqML listWidgets

instance (FormInstanceC t m, VFormBuilderC t m a)=>FormBuilder t m (Seq.Seq a) where
  buildForm = buildSequence


hashMapML::(Ord k, Hashable k)=>MapLike (HML.HashMap k) v k v
hashMapML = MapLike (M.fromList . HML.toList) (HML.fromList . M.toList) RD.diffMapNoEq

hashMapEqML::(Hashable k, Ord k, Eq v)=>MapLike (HML.HashMap k) v k v
hashMapEqML = hashMapML { diffMap = RD.diffMap }

buildHashMap::(FormInstanceC t m,Ord k, Hashable k, VFormBuilderC t m k, VFormBuilderC t m v)=>BuildForm t m (HML.HashMap k v)
buildHashMap = buildAdjustableContainer hashMapML mapWidgets

buildEqHashMap::(FormInstanceC t m,Ord k, Hashable k, VFormBuilderC t m k, VFormBuilderC t m v, Eq v)=>BuildForm t m (HML.HashMap k v)
buildEqHashMap = buildAdjustableContainer hashMapEqML mapWidgets

instance (FormInstanceC t m, VFormBuilderC t m k, VFormBuilderC t m v, Ord k, Hashable k, Eq k)=>FormBuilder t m (HML.HashMap k v) where
  buildForm = buildHashMap

-- Can't do plain HashSet since we need (Eq a) for fromList. So we may as well take advantage in the diffing

hashSetEqML::(Eq a, Hashable a)=>MapLike HS.HashSet a Int a
hashSetEqML = MapLike (M.fromList . zip [0..] . HS.toList) (HS.fromList . fmap snd . M.toList) RD.diffMap

buildEqHashSet::(FormInstanceC t m,Hashable a, Eq a, VFormBuilderC t m a)=>BuildForm t m (HS.HashSet a)
buildEqHashSet = buildAdjustableContainer hashSetEqML setEqWidgets

instance (FormInstanceC t m, VFormBuilderC t m a, Hashable a, Eq a)=>FormBuilder t m (HS.HashSet a) where
  buildForm = buildEqHashSet

  
-- the various container builder components
type BuildF t m a    = FormValidator a->Maybe FieldName->DynMaybe t a->FRW t m a
type BuildForm t m a = FormValidator a->Maybe FieldName->DynMaybe t a->Form t m a

type LBBuildF' t m k v = Maybe FieldName->R.Dynamic t (M.Map k v)->FRW t m (M.Map k v)


data MapLike f a k v = MapLike { toMap::f a->M.Map k v
                               , fromMap::M.Map k v->f a
                               , diffMap::M.Map k v -> M.Map k v -> M.Map k (Maybe v)
                               }

data MapElemWidgets t m k v = MapElemWidgets { elemW::ElemWidget t m k v                                
                                             , newOneWF::R.Dynamic t (M.Map k (FValidation v)) -> R.Event t (k,v)->R.Event t (M.Map k v)->FRW t m (k,v) 
                                             }


instance (B.Validatable FValidation a, B.Validatable FValidation b)=>B.Validatable FValidation (a,b) where
  validator (a,b) = (,) <$> B.validator a <*> B.validator b


type ElemWidget t m k v = k->R.Dynamic t v->FRW t m v 
type LBWidget t m k v = k->R.Dynamic t v->FR t m (R.Dynamic t (Maybe (FValidation v)))

elemWidgetToLBWidget::(R.Reflex t, Functor m)=>ElemWidget t m k v->LBWidget t m k v
elemWidgetToLBWidget ew k vDyn = fmap Just . unDynValidation <$> ew k vDyn

maybeMapToMap::Maybe (M.Map k v) -> M.Map k v
maybeMapToMap = fromMaybe M.empty 

buildLBEditOnly::(FormInstanceC t m
                   , Ord k
                   , VFormBuilderC t m k
                   , VFormBuilderC t m v)
  =>MapLike f a k v
  ->MapElemWidgets t m k v
  ->Maybe FieldName
  ->DynMaybe t (f a)
  ->Form t m (f a)
buildLBEditOnly (MapLike to from _) (MapElemWidgets eW _) mFN dmfa =  makeForm $ do
  let mapDyn0 = fmap maybeMapToMap . getCompose $ to <$> dmfa
  fmap from <$> buildLBEMapLWK' (elemWidgetToLBWidget eW) mFN mapDyn0

buildLBDelete::(FormInstanceC t m
                   , Ord k
                   , VFormBuilderC t m k
                   , VFormBuilderC t m v)
  =>MapLike f a k v
  ->MapElemWidgets t m k v
  ->Maybe FieldName
  ->DynMaybe t (f a)
  ->Form t m (f a)
buildLBDelete (MapLike to from _) (MapElemWidgets eW _) mFN dmfa = makeForm $ do
  let eW' = editAndDeleteElemWidget eW (R.constDyn True)
      mapDyn0 = fmap maybeMapToMap . getCompose $ to <$> dmfa
  fmap from <$> buildLBEMapLWK' eW' mFN mapDyn0      



--NB: I see why tagPromtlyDyn is required for pairWidgetEv (so when the new one is drawn, it sees the updated map) but not quite why
-- that doesn't lead to a cycle.
buildLBAddDelete::(FormInstanceC t m
                  , Ord k
                  , VFormBuilderC t m k
                  , VFormBuilderC t m v)
  =>MapLike f a k v
  ->MapElemWidgets t m k v
  ->Maybe FieldName
  ->DynMaybe t (f a)
  ->Form t m (f a)
buildLBAddDelete (MapLike to from diffMapF) (MapElemWidgets eW nWF) mFN dmfa = makeForm $ fCol $ mdo
  let eW' k v0 vEv = R.holdDyn v0 vEv >>= \vDyn -> editAndDeleteElemWidget eW (R.constDyn True) k vDyn
      mapDyn0 = fmap maybeMapToMap . getCompose $ to <$> dmfa 
      diffMap' avOld new = case avOld of
        AccSuccess old -> diffMapF old new
        AccFailure _ -> Just <$> new
  newInputMapEv <- dynAsEv mapDyn0 
  updateMapDyn <- fItem $ listWithKeyShallowDiffMap M.empty diffMapEv eW' -- Dynamic t (Map k (Dynamic t (Maybe (FValidation v))))
  addEv <- fRow $ mdo
    addPairDV <- fRow $ nWF mapDyn newPairEv newInputMapEv
    let newPairMaybeDyn = avToMaybe <$> unDynValidation addPairDV
    addButtonEv <- fItem $ buttonNoSubmit' "+" -- Event t ()
    let newPairEv = R.fmapMaybe id $ R.tag (R.current newPairMaybeDyn) addButtonEv
    return newPairEv
  let newInputDiffEv = R.attachWith diffMap' (R.current $ sequenceA <$> mapDyn) newInputMapEv -- Event t (Map k (Maybe v))
      insertDiffEv = fmap Just . uncurry M.singleton <$> addEv  
      diffMapEv = R.leftmost [newInputDiffEv, insertDiffEv]
      mapEditsFVEv = R.updated . join $ R.distributeMapOverDynPure <$> updateMapDyn -- Event t (Map k (Maybe (FValidation v)))
      editedMapEv = R.attachWith (flip RD.applyMap) (R.current mapDyn) mapEditsFVEv -- Event t (Map k (FValidation v))
  mapDyn <- R.holdDyn M.empty $ R.leftmost
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
                 , Ord k
                 , VFormBuilderC t m k
                 , VFormBuilderC t m v)
  =>LBWidget t m k v->LBBuildF' t m k v
buildLBEMapLWK' editW _ mapDyn0 = do
  mapDynEv <- traceDynAsEv (const "buildLBEMapLWK'") mapDyn0
  mapDyn' <- R.holdDyn M.empty mapDynEv
  mapOfDyn <- listWithKeyMap (R.traceDynWith (const "LWK' mapDyn0") mapDyn') editW -- Dynamic t (M.Map k (Dynamic t (Maybe (FValidation v)))
  let mapFValDyn = M.mapMaybe id <$> (join $ R.distributeMapOverDynPure <$> mapOfDyn) -- Dynamic t (Map k (FValidation v))
  return . DynValidation $ sequenceA <$> mapFValDyn

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
  =>ElemWidget t m k v
  ->R.Dynamic t Bool
  ->LBWidget t m k v
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
  mapOfDynMaybe <- listWithKeyMap (R.traceDynWith (\m -> "LWK map0Dyn: " ++ show (M.keys m)) map0Dyn) editOne
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




