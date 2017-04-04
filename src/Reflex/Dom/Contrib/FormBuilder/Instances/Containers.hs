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
mapEQML = MapLike id id RD.diffMap

mapWidgets::(FormInstanceC t m, VFormBuilderC t m k, VFormBuilderC t m v)=>MapElemWidgets t m k v
mapWidgets = MapElemWidgets showKeyEditVal (const . unF $ buildForm' Nothing (constDynMaybe Nothing))

buildMap::(FormInstanceC t m,Ord k, VFormBuilderC t m k, VFormBuilderC t m v)=>BuildForm t m (M.Map k v)
buildMap = buildAdjustableContainer mapML mapWidgets

buildEqMap::(FormInstanceC t m,Ord k, Eq v, VFormBuilderC t m k, VFormBuilderC t m v)=>BuildForm t m (M.Map k v)
buildEqMap = buildAdjustableContainer mapEQML mapWidgets

instance (FormInstanceC t m, Ord k, VFormBuilderC t m k, VFormBuilderC t m a)=>FormBuilder t m (M.Map k a) where
  buildForm =  buildMap

listML::MapLike [] a Int a
listML = MapLike (M.fromList . zip [0..]) (fmap snd . M.toList) RD.diffMapNoEq

listEQML::Eq a=>MapLike [] a Int a
listEQML = listML { diffMap = RD.diffMap }

listEditWidget::(FormInstanceC t m, VFormBuilderC t m a)=>M.Map Int a -> FRW t m (Int,a)
listEditWidget curMap = do
  let newKey = (+1) . maximum . M.keys $ curMap
  newElem <- unF $ buildForm' Nothing (constDynMaybe Nothing)
  return $ (,) <$> constDynValidation newKey <*> newElem

listWidgets::(FormInstanceC t m, VFormBuilderC t m a)=>MapElemWidgets t m Int a
listWidgets = MapElemWidgets hideKeyEditVal listEditWidget

buildList::(FormInstanceC t m, VFormBuilderC t m a)=>BuildForm t m [a]
buildList = buildAdjustableContainer listML listWidgets

buildEqList::(FormInstanceC t m, Eq a, VFormBuilderC t m a)=>BuildForm t m [a]
buildEqList = buildAdjustableContainer listEQML listWidgets

instance (FormInstanceC t m, VFormBuilderC t m a)=>FormBuilder t m [a] where
  buildForm =  buildList


--buildList::(FormInstanceC t m,Ord k, VFormBuilderC t m a)=>BuildF t m (M.Map k v)



-- the various container builder components
type BuildF t m a    = FormValidator a->Maybe FieldName->DynMaybe t a->FRW t m a
type BuildForm t m a = FormValidator a->Maybe FieldName->DynMaybe t a->Form t m a

type LBBuildF' t m k v = Maybe FieldName->R.Dynamic t (M.Map k v)->FRW t m (M.Map k v)


data MapLike f a k v = MapLike
                       {
                         toMap::f a->M.Map k v
                       , fromMap::M.Map k v->f a
                       , diffMap::M.Map k v -> M.Map k v -> M.Map k (Maybe v)
                       }

data MapElemWidgets t m k v = MapElemWidgets 
                              {                     
                                elemW::ElemWidget t m k v
                              , newOneWF::M.Map k v->FRW t m (k,v) -- might need existing map to choose a new key
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
  updateMapDyn <- fItem $ RD.listWithKeyShallowDiff M.empty diffMapEv eW' -- Dynamic t (Map k (Dynamic t (Maybe (FValidation v))))
  addEv <- fRow $ mdo
    let pairWidgetEv = R.fmapMaybe (fmap nWF) $ R.tag (R.current $ avToMaybe . sequenceA <$> mapDyn) $ R.leftmost [() <$ newPairEv, () <$ newInputMapEv]
    addPairDV <- fRow $ joinDynOfDynValidation <$> RD.widgetHold (return $ dynValidationNothing) pairWidgetEv -- DynValidation (k,v)
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
  mapOfDyn <- RD.listWithKey (R.traceDynWith (const "LWK' mapDyn0") mapDyn') editW -- Dynamic t (M.Map k (Dynamic t (Maybe (FValidation v)))
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


type LBBuildF t m k v = Maybe FieldName->R.Dynamic t (M.Map k v)->FR t m (R.Dynamic t (M.Map k v))

{-
toBuildF::(R.Reflex t,Functor m)=>LBBuildF t m k v->BuildF t m (M.Map k v)
toBuildF lbbf mFN mMapDyn =
  let mapDyn = fromMaybe (R.constDyn M.empty) mMapDyn
  in DynValidation . fmap AccSuccess <$> lbbf mFN mapDyn 

-}


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
  mapOfDynMaybe <- RD.listWithKey (R.traceDynWith (\m -> "LWK map0Dyn: " ++ show (M.keys m)) map0Dyn) editOne
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


type WidgetConstraints t m k v = (RD.DomBuilder t m, RD.PostBuild t m, MonadFix m, R.MonadHold t m, RD.DomBuilderSpace m ~ RD.GhcjsDomSpace, Show v, Read v, Ord k, Show k)
editWidgetDyn::WidgetConstraints t m k v=>k->R.Dynamic t v-> m (R.Dynamic t (Maybe v))
editWidgetDyn k vDyn = do
  inputEv' <- traceDynAsEv (\x->"editWidget: v=" ++ show x) vDyn
  let inputEv = T.pack . show <$> inputEv'
      config = RD.def {RD._textInputConfig_setValue = inputEv }
  RD.el "span" $ RD.text (T.pack $ show k)
  valDyn <- RD._textInput_value <$> RD.textInput config
  return $ readMaybe . T.unpack <$> valDyn 

editAndDeleteWidgetEv::WidgetConstraints t m k v=>R.Dynamic t Bool->k->R.Dynamic t v-> m (R.Event t (Maybe v))
editAndDeleteWidgetEv selDyn k vDyn = mdo
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> visibleDyn
  (visibleDyn,outEv) <- RD.elDynAttr "div" widgetAttrs $ do
    resEv <-  R.updated <$> editWidgetDyn k vDyn
    delButtonEv <- buttonNoSubmit' "-"
    selEv <- dynAsEv selDyn
    visDyn <-  R.holdDyn True $ R.leftmost
               [
                 selEv
               , False <$ delButtonEv -- delete button pressed, so hide
               , True <$ R.updated vDyn -- value updated so make sure it's visible (in case of re-use of deleted key)
               ]
    let outEv' = R.leftmost
                 [
                   Just <$> R.fmapMaybe id resEv
                 , Nothing <$ delButtonEv
                 ]           
    return (visDyn,outEv')
  return outEv


editOneEv::(FormInstanceC t m, VFormBuilderC t m v,Ord k, Show k)=>R.Dynamic t Bool->k->R.Dynamic t v->FR t m (R.Event t (Maybe v))
editOneEv selDyn k valDyn = mdo
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> visibleDyn
  valEv <- traceDynAsEv (const "editWidgetDyn valEv") valDyn
  (visibleDyn,outEv) <- RD.elDynAttr "div" widgetAttrs $ fRow $ do
    resEv <-  R.updated <$> editOne k valDyn
    delButtonEv <- fItem $ fCenter LayoutVertical . fItemR . lift $ containerActionButton "-" -- Event t ()
    let outEv' = R.traceEventWith (const "editOneEv outEv") $ R.leftmost
                 [
                   Just <$> R.fmapMaybe id resEv
                 , Just <$> valEv
                 , Nothing <$ delButtonEv]
    inputSelEv <- dynAsEv selDyn
    visibleDyn' <- R.holdDyn True $ R.leftmost
                   [
                     inputSelEv -- calling widget
                   , False <$ delButtonEv -- delete button pressed, so hide
                   , True <$ R.updated valDyn -- value updated so make sure it's visible (in case of re-use of deleted key)
                   ]
    
    return (visibleDyn',outEv')
  return outEv



-- now with ListViewWithKeyShallowDiff just so I understand things.
buildLBEMapLVWKSD::(FormInstanceC t m
                  , VFormBuilderC t m v
                  , Ord k, Show k)
                => LBBuildF t m k v
buildLBEMapLVWKSD mf mapDyn0 = mdo
  newInputMapEv <- dynAsEv mapDyn0
  updateEvsDyn <- RD.listWithKeyShallowDiff M.empty diffMapEv editOneSD -- Dynamic t (Map k (Event t (Maybe v)))
  let mapEditsEv =  R.switch $ R.mergeMap <$> R.current updateEvsDyn -- Event t (Map k (Maybe v))
      diffMapEv = R.traceEventWith (\m -> "new Input to buildLBEMapLVWKSD: " ++ show (M.keys m)) $ fmap Just <$> newInputMapEv 
      editedMapEv = R.attachWith (flip RD.applyMap) (R.current mapDyn) mapEditsEv
      newMapEv = R.leftmost [newInputMapEv, editedMapEv]
  mapDyn <- R.holdDyn M.empty newMapEv
  return (R.traceDynWith (\m -> "LVWKSD mapDyn: " ++ show (M.keys m)) mapDyn)
  
editOneSD::(FormInstanceC t m, VFormBuilderC t m v, Ord k, Show k)=>k->v->R.Event t v->FR t m (R.Event t (Maybe v))
editOneSD k v0 vEv = R.holdDyn v0 vEv >>= editOneEv (R.constDyn True) k
  
hiddenCSS::M.Map T.Text T.Text
hiddenCSS  = "style" =: "display: none"
visibleCSS::M.Map T.Text T.Text
visibleCSS = "style" =: "display: inline"


ddAttrsDyn::R.Reflex t=>(Int->Int)->R.Dynamic t Int->R.Dynamic t RD.AttributeMap
ddAttrsDyn sizeF = fmap (\n->if n==0 then hiddenCSS else visibleCSS <> ("size" =: (T.pack . show $ sizeF n)))


{-
data CRepI (fa :: *) (gb :: *) = CRepI { toRep::fa -> gb, fromRep::gb -> fa }

idRep::CRepI fa fa
idRep = CRepI id id

data FAppendableI (fa :: * ) (g :: * -> *) (b :: *) = FAppendableI
  {
    cRep::CRepI fa (g b)
  , emptyT::g b
  , insertB::b->fa->fa
  , sizeFa::fa->Int
  }

toT::FAppendableI fa g b->(fa -> g b)
toT = toRep . cRep

fromT::FAppendableI fa g b->(g b -> fa)
fromT = fromRep . cRep

data FDeletableI (g :: * -> *) (b :: *) (k :: *) (s :: *) = FDeletableI
  {
    getKey::b -> s -> k
  , initialS::s
  , updateS::s->s
  , delete::k -> g b -> g b
  }

-- This helps insure the types in Appendable and Deletable line up for any particular instance
data FAdjustableI fa g b k s= FAdjustableI { sfAI::FAppendableI fa g b, sfDI::FDeletableI g b k s }

{-

buildAdjustableContainer::(FormInstanceC t m, VFormBuilderC t m b,Traversable g)
  =>FAdjustableI fa g b k s
  ->FormValidator fa
  ->Maybe FieldName
  ->Maybe (R.Dynamic t fa)
  ->Form t m fa
buildAdjustableContainer sfAdj va mFN mfaDyn = validateForm va . makeForm  $ do
  fType <- getFormType
  case fType of
    ObserveOnly ->  buildReadOnlyContainer (cRep . sfAI $ sfAdj) mFN mfaDyn
    Interactive ->  buildTraversableFA (cRep . sfAI $ sfAdj) mFN mfaDyn --buildSFContainer (sfAI sfAdj) (buildDeletable (sfDI sfAdj)) mFN mfaDyn
-}



listAppend::a->[a]->[a]
listAppend a as = as ++ [a]

listDeleteAt::Int->[a]->[a]
listDeleteAt n as = take n as ++ drop (n+1) as

listFA::FAppendableI [a] [] a
listFA = FAppendableI idRep [] listAppend L.length

listFD::FDeletableI [] a Int Int
listFD = FDeletableI (\_ n->n) 0 (+1) listDeleteAt

instance (FormInstanceC t m,VFormBuilderC t m a)=>FormBuilder t m [a] where
  buildForm = buildAdjustableContainer (FAdjustableI listFA listFD)


mapFA::Ord k=>FAppendableI (M.Map k v) (M.Map k) (k,v)
mapFA = FAppendableI (CRepI (M.mapWithKey (\k v->(k,v))) (\m->M.fromList $ snd <$> M.toList m)) mempty (\(k,x) m->M.insert k x m) M.size

mapFD::Ord k=>FDeletableI (M.Map k) (k,v) k ()
mapFD = FDeletableI (\(k,_) _ ->k) () id M.delete

buildReadOnlyContainer::(FormInstanceC t m, VFormBuilderC t m b,Traversable g)=>CRepI fa (g b)->BuildF t m fa
buildReadOnlyContainer = buildTraversableFA  



        
intMapFA::FAppendableI (IM.IntMap v) IM.IntMap (IM.Key,v)
intMapFA = FAppendableI (CRepI (IM.mapWithKey (\k v->(k,v))) (\m->IM.fromList $ snd <$> IM.toList m)) mempty (\(k,x) m->IM.insert k x m) IM.size

intMapFD::FDeletableI IM.IntMap (IM.Key,v) IM.Key ()
intMapFD = FDeletableI (\(k,_) _ ->k) () id IM.delete

--instance (FormInstanceC t m, VFormBuilderC t m IM.Key, VFormBuilderC t m a)=>FormBuilder t m (IM.IntMap a) where
--  buildForm = buildAdjustableContainer (FAdjustableI intMapFA intMapFD)

seqFA::FAppendableI (Seq.Seq a) Seq.Seq a
seqFA = FAppendableI idRep Seq.empty (flip (Seq.|>)) Seq.length

seqFD::FDeletableI Seq.Seq a Int Int
seqFD = FDeletableI (\_ index->index) 0 (+1) (\n bs -> Seq.take n bs Seq.>< Seq.drop (n+1) bs)

instance (FormInstanceC t m,VFormBuilderC t m a)=>FormBuilder t m (Seq.Seq a) where
  buildForm = buildAdjustableContainer (FAdjustableI seqFA seqFD)

-- we transform to a map since Set is not Traversable and we want map-like semantics on the as. We could fix this with lens Traversables maybe?
setFA::Ord a=>FAppendableI (S.Set a) (M.Map a) a
setFA = FAppendableI (CRepI (\s ->M.fromList $ (\x->(x,x)) <$> S.toList s) (\m->S.fromList $ snd <$> M.toList m)) M.empty S.insert S.size

setFD::Ord a=>FDeletableI (M.Map a) a a ()
setFD = FDeletableI const () id M.delete

instance (FormInstanceC t m, Ord a, VFormBuilderC t m a)=>FormBuilder t m (S.Set a) where
  buildForm = buildAdjustableContainer (FAdjustableI setFA setFD)

hashMapFA::(Eq k,Hashable k)=>FAppendableI (HML.HashMap k v) (HML.HashMap k) (k,v)
hashMapFA = FAppendableI (CRepI (HML.mapWithKey (\k v->(k,v))) (\m->HML.fromList $ snd <$> HML.toList m)) mempty (\(k,x) m->HML.insert k x m) HML.size

hashMapFD::(Eq k, Hashable k)=>FDeletableI (HML.HashMap k) (k,v) k ()
hashMapFD = FDeletableI (\(k,_) _ ->k) () id HML.delete


instance (FormInstanceC t m, Eq k, Hashable k, VFormBuilderC t m k, VFormBuilderC t m v)=>FormBuilder t m (HML.HashMap k v) where
  buildForm = buildAdjustableContainer (FAdjustableI hashMapFA hashMapFD)


-- we transform to a HashMap since Set is not Traversable and we want map-like semantics on the as. We could fix this with lens Traversables maybe?
hashSetFA::(Eq a,Hashable a)=>FAppendableI (HS.HashSet a) (HML.HashMap a) a
hashSetFA = FAppendableI (CRepI (\hs ->HML.fromList $ (\x->(x,x)) <$> HS.toList hs) (\hm->HS.fromList $ snd <$> HML.toList hm)) HML.empty HS.insert HS.size

hashSetFD::(Eq a, Hashable a)=>FDeletableI (HML.HashMap a) a a ()
hashSetFD = FDeletableI const () id HML.delete

instance (FormInstanceC t m, Eq a, Hashable a, VFormBuilderC t m a)=>FormBuilder t m (HS.HashSet a) where
  buildForm = buildAdjustableContainer (FAdjustableI hashSetFA hashSetFD)




-- This feels like a lot of machinery just to get removables...but it does work...
newtype SFR s t m a = SFR { unSFR::StateT s (FR t m) (DynValidation t a) }

instance (R.Reflex t, R.MonadHold t m)=>Functor (SFR s t m) where
  fmap f sfra = SFR $ fmap f <$> unSFR sfra 

instance (R.Reflex t, R.MonadHold t m)=>Applicative (SFR s t m) where
  pure x = SFR $ return $ pure x
  sfrF <*> sfrA = SFR $ do
    dmF <- unSFR sfrF
    dmA <- unSFR sfrA
    return $ dmF <*> dmA

liftLF'::Monad m=>(forall b.m b->m b)->StateT s m a -> StateT s m a
liftLF' = hoist 

-- utilities for collapsing
-- should be using display hidden for this?
widgetToggle::(RD.DomBuilder t m, R.MonadHold t m)=>Bool->R.Event t Bool -> m a -> m a -> m (R.Dynamic t a)
widgetToggle startCond condEv trueW falseW = do
  let condW b = if b then trueW else falseW
  RD.widgetHold (condW startCond) (condW <$> condEv)

widgetToggleDyn::(RD.DomBuilder t m, R.MonadHold t m)=>Bool->R.Event t Bool -> m (R.Dynamic t a) -> m (R.Dynamic t a) -> m (R.Dynamic t a)
widgetToggleDyn startCond condEv trueW falseW = join <$> widgetToggle startCond condEv trueW falseW

-- unstyled, for use within other instances which will deal with the styling.
-- this needs redoing using a distribute function to avoid the widgetHold
buildTraversableFA'::(FormInstanceC t m, VFormBuilderC t m b,Traversable g)=>CRepI fa (g b)->BuildF t m b->BuildF t m fa
buildTraversableFA' crI buildOne _ mfaDyn =
  case mfaDyn of
    Nothing -> return dynValidationNothing
    Just faDyn -> do
      postbuild <- RD.getPostBuild
      let buildStatic fa = unF $ fromRep crI <$> traverse (liftF fRow . makeForm . buildOne Nothing . Just . R.constDyn) (toRep crI fa)
          startWidgetEv = buildStatic <$> R.tag (R.current faDyn) postbuild
          newWidgetEv   = R.updated $ buildStatic <$> faDyn -- Dynamic t (SFRW t m fa)
      joinDynOfDynValidation <$> RD.widgetHold (return dynValidationNothing) (R.leftmost [startWidgetEv, newWidgetEv])

-- styled, in case we ever want an editable container without add/remove
buildTraversableFA::forall t m b g fa.(FormInstanceC t m, VFormBuilderC t m b,Traversable g)=>CRepI fa (g b)->BuildF t m fa 
buildTraversableFA crI md mfa = do
  validClasses <- validDataClasses
  let attrsDyn = R.constDyn $ cssClassAttr validClasses :: R.Dynamic t (M.Map T.Text T.Text)
  fColDynAttr attrsDyn $ fCollapsible "" CollapsibleStartsOpen $ buildTraversableFA' crI (\x -> fItemL . unF . buildForm' x) md mfa


-- TODO: need a new version to do widgetHold over whole thing if collapsible depends on size
buildFContainer::(FormInstanceC t m, VFormBuilderC t m b, Traversable g)=>FAppendableI fa g b->BuildF t m (g b)->BuildF t m fa
buildFContainer aI buildTr mFN mfaDyn = mdo
  attrsDyn <- fAttrs dmfa mFN Nothing
  let initial = Just $ maybe (R.constDyn $ emptyT aI) (fmap (toT aI)) mfaDyn
      newInputEv = maybe R.never R.updated mfaDyn -- Event t fa
  dmfa <- fColDynAttr attrsDyn $ fCollapsible "" CollapsibleStartsOpen $ mdo
    dmfa' <- unF $ fromT aI <$> (makeForm $ joinDynOfDynValidation <$> RD.widgetHold (buildTr mFN initial) newFREv)
    let udmfa' = unDynValidation dmfa' -- Dynamic t (SFValidation fa)
        sizeDM = fmap (sizeFa aI) dmfa' -- Dynamic t (SFValidation Int)
        newSizeEv = R.updated . R.uniqDyn $ unDynValidation sizeDM -- Event t (SFValidation Int)
        resizedFaEv = R.attachPromptlyDynWithMaybe (\mFa ms -> accValidation (const Nothing) (const $ avToMaybe mFa) ms) udmfa' newSizeEv -- Event t (SFValidation fa)
          
    addEv <- fRow $  do
      let emptyB = unF $ buildForm' Nothing Nothing
      -- we have to clear it once it's used. For now we replace it with a new one.
      dmb <- fItemL $ joinDynOfDynValidation <$> RD.widgetHold emptyB (emptyB <$ newFaEv) 
      clickEv <-  fCenter LayoutVertical . fItemR . lift $ containerActionButton "+" 
      return $ R.attachPromptlyDynWithMaybe (\a b -> avToMaybe a) (unDynValidation dmb) clickEv -- only fires if button is clicked when mb is a Just.
        
    let insert vfa' b = avToMaybe $ insertB aI <$> AccSuccess b <*> vfa'  
        newFaEv = R.attachPromptlyDynWithMaybe insert udmfa' addEv -- Event t (tr a), only fires if (Maybe fa) is not Nothing
        newFREv = fmap (buildTr mFN . Just . R.constDyn . toT aI) (R.leftmost [newFaEv,resizedFaEv,newInputEv]) -- Event t (SFRW t m (g b))
    return dmfa'
  return dmfa


buildFContainer'::(FormInstanceC t m,VFormBuilderC t m b,Traversable g)=>FAppendableI fa g b->BuildF t m (g b)->BuildF t m fa
buildFContainer' aI buildTr mFN mfaDyn = mdo
  attrsDyn <- fAttrs dmfa mFN Nothing
  let initial = Just $ maybe (R.constDyn $ emptyT aI) (fmap (toT aI)) mfaDyn
      newInputEv = maybe R.never R.updated mfaDyn -- Event t fa
  dmfa <- fColDynAttr attrsDyn $ mdo
    dmfa' <- unF $ fromT aI <$> (makeForm $ joinDynOfDynValidation <$> RD.widgetHold (buildTr mFN initial) newFREv)
    let udmfa' = unDynValidation dmfa'
        sizeDM = fmap (sizeFa aI) dmfa'
        newSizeEv = R.updated . R.uniqDyn $ unDynValidation sizeDM
        resizedFaEv = R.attachPromptlyDynWithMaybe (\mFa ms -> accValidation (const Nothing) (const $ avToMaybe mFa) ms) udmfa' newSizeEv
          
    addEv <- fRow $  do
      let emptyB = unF $ buildForm' Nothing Nothing
      -- we have to clear it once it's used. For now we replace it with a new one.
      dmb <- fItemL $ joinDynOfDynValidation <$> RD.widgetHold emptyB (emptyB <$ newFaEv) 
      clickEv <-  fCenter LayoutVertical . fItemR . lift $ containerActionButton "+" 
      return $ R.attachPromptlyDynWithMaybe (\a b -> avToMaybe a) (unDynValidation dmb)  clickEv -- only fires if button is clicked when mb is a Just.
        
    let insert vfa' b = avToMaybe $ insertB aI <$> AccSuccess b <*> vfa'  
        newFaEv = R.attachPromptlyDynWithMaybe insert udmfa' addEv -- Event t (tr a), only fires if (Maybe fa) is not Nothing
        newFREv = fmap (buildTr mFN . Just . R.constDyn . toT aI) (R.leftmost [newFaEv,resizedFaEv,newInputEv]) -- Event t (SFRW t m (g b))
    return dmfa'
  return dmfa

buildOneDeletable::(FormInstanceC t m, VFormBuilderC t m b)
                   =>FDeletableI g b k s->Maybe FieldName->Maybe (R.Dynamic t b)->StateT ([R.Event t k],s) (FR t m) (DynValidation t b)
buildOneDeletable dI mFN mbDyn = liftLF' fRow $ do     
    (evs,curS) <- get
    dmb <- lift . fItemL . unF $ buildForm' mFN mbDyn
    ev  <- lift . fCenter LayoutVertical . fItemR . lift $ containerActionButton "-" 
    let ev' = R.attachPromptlyDynWithMaybe (\va' _ -> getKey dI <$> (avToMaybe va') <*> Just curS) (unDynValidation dmb) ev
    put (ev':evs,updateS dI curS)
    return dmb



buildDeletable::forall t m b g k s.(FormInstanceC t m, VFormBuilderC t m b, Traversable g)=>FDeletableI g b k s->BuildF t m (g b)
buildDeletable dI _ mgbDyn = 
  case mgbDyn of
    Nothing -> return dynValidationNothing
    Just gbDyn -> mdo
      postbuild <- RD.getPostBuild
      let buildStatic x = runStateT (unSFR $ traverse (SFR . liftLF' fRow  . buildOneDeletable dI Nothing . Just . R.constDyn) x) ([],initialS dI)
      let f::(FormInstanceC t m, VFormBuilderC t m b, Traversable g)=>R.Dynamic t (g b)->FR t m (DynValidation t (g b),(R.Dynamic t (RD.Event t k)))
          f gbDyn' = do
            let startWidgetEv = buildStatic <$> R.tag (R.current gbDyn') postbuild
                newWidgetEv = R.updated $ buildStatic <$> gbDyn'
            (ddmgb',stateDyn) <- R.splitDynPure <$> RD.widgetHold (return $ (dynValidationNothing, ([],initialS dI))) (R.leftmost [startWidgetEv, newWidgetEv])
            let dmgb' = joinDynOfDynValidation ddmgb'
--            (dmgb',(evs,_)) <- runStateT (unSSFR $ traverse (SSFR . liftLF' sfRow  . buildOneDeletable dI Nothing . Just) gb') ([],initialS dI)
            return  (dmgb',R.leftmost . fst <$> stateDyn) -- DynValidation t gb,Dynamic t (Event t k)
      (ddmgb,ddEv) <- R.splitDynPure <$> RD.widgetHold (f gbDyn) (f . R.constDyn <$> newgbEv)
      let dmgb = join (unDynValidation <$> ddmgb)
          dEv = join ddEv
--      dEv <- RD.switch <$> R.hold R.never (R.updated $ join ddEv) -- ??
      let newgbEv = R.attachPromptlyDynWithMaybe (\mgb' key-> delete dI key <$> (avToMaybe mgb')) dmgb (R.switchPromptlyDyn dEv)
      return $ DynValidation dmgb

-}
