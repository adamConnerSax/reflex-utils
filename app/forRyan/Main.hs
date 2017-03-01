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

import qualified Reflex 
import qualified Reflex.Dom

import qualified Data.Map as M

newtype LBCMap k v = LBCMap { lbcMap::M.Map (Maybe k) (Maybe v) }
newtype LBCSelection k = LBCSelection { lbcSelection::Maybe k }

data LBCSelectionUpdate k = SelectionUpdate (Maybe k) 
updateSelection::LBCSelectionUpdate k->LBCSelection k->LBCSelection k
updateSelection (SelectionUpdate mk) _ = LBCSelection mk -- this no longer checks if mk is in map so we need to make sure

data LBCMapUpdate k v = MapChangeValue k v
updateMap::Ord k=>LBCMapUpdate k v->LBCMap k v->LBCMap k v
updateMap (MapChangeValue k v) (LBCMap m) = LBCMap $ M.adjust (const $ Just v) (Just k) m



buildLBEditableMap::(DomBuilder t m, Read v, Ord k,Show k)=>M.Map k v->m (Dynamic t (Maybe (M.Map k v)))  
buildLBEditableMap mMap = sfRow $ mdo
  let map0 = fromMaybe M.empty mMap
      mk0 = if M.null map0 then Nothing else Just . head $ M.keys map0
      labelF k _ = T.pack $ show k
      editedToCI (Nothing,_) = Nothing -- shouldn't happen
      editedToCI (Just k, v) = Just $ MapChangeValue k v
      editF Nothing _ _ = return R.never
      editF (Just _) valDyn selDyn = editOne (fromJust <$> valDyn) selDyn -- will be okay but relies on getting LBCState manipulation right
      editWidgets x = R.fmapMaybe editedToCI <$> RD.selectViewListWithKey selectionDyn x editF

  lbcSelDyn <- R.foldDyn updateSelection (LBCSelection mk0) selUpdateEv
  let selectionDyn = lbcSelection <$> lbcSelDyn
  selectEv <- sfCol $ selectableKeyList 10 labelF lbcMapDyn
  let selUpdateEv = R.leftmost [selectEv]

  
  mapUpdateEv <- editWidgets mapDyn -- Event t (MapChangeValue k v)
  -- NO: lbcMapDyn <- foldDyn updateMap (LBCMap $ toMaybeMap map0) mapEvent
  let newMapEv = R.attachWith (flip updateMap) (R.current lbcMapDyn) mapUpdateEv
  lbcMapDyn <- R.holdDyn (LBCMap $ toMaybeMap map0) newMapEv
  let mapDyn = lbcMap <$> lbcMapDyn 
  return . DynValidation $ (AccSuccess . fromMaybeMap <$> mapDyn)


myWidgetList::(RD.DomBuilder t m, R.MonadHold t m, R.PostBuild t m,R.Reflex t,Monad m,Eq k)
  => R.Dynamic t k
  -> R.Dynamic t (M.Map k v)
  -> (v->R.Dynamic t Bool->m (R.Event t a))
  -> m (R.Event t (k,a))
myWidgetList selDyn mapDyn widgetF = do
  let selWidgetF keyDyn f (key,val) = fmap (key,) <$> f val ((==key) <$> keyDyn)  -- Dynamic t k -> (v->Dynamic t Bool->m (Event t a))->(k,v)->m (Event t (k,a))
      listOfWidgetsDyn = fmap (selWidgetF selDyn widgetF) .  M.toList <$> mapDyn -- Dynamic t [m (Event t (k,a)], this is bad, redoes all widgets
      eventWidgetDyn = (fmap R.leftmost) . sequence <$> listOfWidgetsDyn -- Dynamic t (m (Event t (k,a)))
  eventEv <- RD.dyn eventWidgetDyn -- Event t (Event t (k,a)).  NB, this is not an efficient way to handle this!
  R.switch <$> R.hold R.never eventEv -- does this hold make these outputs suitable for input?
--  return $ R.switch eventDyn


editOne::(DomBuilder t m, Read v)=>R.Dynamic t v->R.Dynamic t Bool->SFR t m (R.Event t v)
editOne valDyn selDyn = do
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> selDyn
  resDynAV <-  RD.elDynAttr "div" widgetAttrs $ unDynValidation <$> (unSF $ buildValidatedDynamic B.validate Nothing (Just valDyn)) -- Dynamic t (AccValidation) val
  let resDyn = accValidation (const Nothing) Just <$> resDynAV -- Dynamic t (Maybe v)
  return $ R.traceEventWith (const "editOne") $ R.fmapMaybe id $ R.updated resDyn 


editOneSimple::(SimpleFormInstanceC t m, VBuilderC t m v)=>v->R.Dynamic t Bool->SFR t m (R.Event t v)
editOneSimple val selDyn = do
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> selDyn
  resDynAV <-  RD.elDynAttr "div" widgetAttrs $ unDynValidation <$> (unSF $ B.buildA Nothing (Just val)) -- Dynamic t (AccValidation) val
  let resDyn = accValidation (const Nothing) Just <$> resDynAV -- Dynamic t (Maybe v)
  return $ R.traceEventWith (const "editOneSimple") $ R.fmapMaybe id $ R.updated resDyn 


hiddenCSS::M.Map T.Text T.Text
hiddenCSS  = "style" =: "display: none"

visibleCSS::M.Map T.Text T.Text
visibleCSS = "style" =: "display: inline"



toMaybeMap::Ord k=>M.Map k v->M.Map (Maybe k) (Maybe v)
toMaybeMap = M.insert Nothing Nothing . M.fromList . fmap (\(k,v)->(Just k,Just v)) . M.toList 

fromMaybeMap::Ord k=>M.Map (Maybe k) (Maybe v) -> M.Map k v
fromMaybeMap = M.fromList . catMaybes . fmap (\(mk, mv)-> (,) <$> mk <*> mv) . M.toList 

selectableKeyList::(RD.DomBuilder t m, MonadFix m, R.MonadHold t m, RD.PostBuild t m, Ord k, Show k)
  =>Int -- max size for dropdown
  ->(k->v->T.Text) -- function to label the map entries
  ->R.Dynamic t (LBCMap k v)  -- the map
  ->m (R.Event t (LBCSelectionUpdate k)) 
selectableKeyList maxDDSize labelF curMapDyn = do
  let mDyn = lbcMap <$> curMapDyn
      sizeF n = min maxDDSize (n+1)
      sizeDyn = M.size <$> mDyn
      ddAttrs = ddAttrsDyn sizeF sizeDyn
      labelF' Nothing _ = ""
      labelF' _ Nothing = ""
      labelF' (Just k) (Just v) = labelF k v
      ddMap = R.traceDyn "ddMap: " $ M.mapWithKey labelF' <$> mDyn
      config = RD.DropdownConfig R.never ddAttrs
  fmap SelectionUpdate . R.traceEvent "selectEv: " . RD._dropdown_change <$> RD.dropdown Nothing ddMap config    
  
ddAttrsDyn::R.Reflex t=>(Int->Int)->R.Dynamic t Int->R.Dynamic t RD.AttributeMap
ddAttrsDyn sizeF = fmap (\n->if n==0 then hiddenCSS else visibleCSS <> ("size" =: (T.pack . show $ sizeF n)))
