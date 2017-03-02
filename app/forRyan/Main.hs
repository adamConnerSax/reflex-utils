{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

import           GHCJS.DOM.Types                  (JSM)
import           Language.Javascript.JSaddle.Warp (run)
import           Reflex
import           Reflex.Dom                       hiding (mainWidget, run)
import           Reflex.Dom.Core                  (mainWidget)

import           Control.Monad.Fix                (MonadFix)

import qualified Data.Map                         as M
import           Data.Maybe                       (catMaybes, fromJust,
                                                   fromMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T

import           System.Process                   (spawnProcess)
import           Text.Read                        (readMaybe)


-- NB: This is just for warp.
main::IO ()
main = do
  let port :: Int = 3702
  pHandle <- spawnProcess "open" ["http://localhost:" ++ show port]
  run port testWidget

testWidget::JSM()
testWidget = mainWidget $ do
  let x = M.fromList [("A",1),("B",2)]
  res <- buildLBEditableMap x
  dynText $ T.pack . show <$> res
  return ()




newtype LBCMap k v = LBCMap { lbcMap::M.Map (Maybe k) (Maybe v) }
newtype LBCSelection k = LBCSelection { lbcSelection::Maybe k }

data LBCSelectionUpdate k = SelectionUpdate (Maybe k)
updateSelection::LBCSelectionUpdate k->LBCSelection k->LBCSelection k
updateSelection (SelectionUpdate mk) _ = LBCSelection mk

data LBCMapUpdate k v = MapChangeValue k v
updateMap::Ord k=>LBCMapUpdate k v->LBCMap k v->LBCMap k v
updateMap (MapChangeValue k v) (LBCMap m) = LBCMap $ M.adjust (const $ Just v) (Just k) m

reselect::LBCMapUpdate k v->Maybe k
reselect (MapChangeValue k v) = Just k


buildLBEditableMap::(DomBuilder t m, PostBuild t m, MonadHold t m,MonadFix m,
                      Read v, Show v,Ord k,Show k,DomBuilderSpace m ~ GhcjsDomSpace)
  =>M.Map k v
  ->m (Dynamic t (M.Map k v))
buildLBEditableMap map0 = mdo
  let mk0 = if M.null map0 then Nothing else Just . head $ M.keys map0
      labelF k _ = T.pack $ show k
      editedToCI (Nothing,_) = Nothing -- shouldn't happen
      editedToCI (Just k, v) = Just $ MapChangeValue k v
      editF Nothing _ _ = return never
      editF (Just _) valDyn selDyn = selectableWidget' (fromJust <$> valDyn) selDyn -- fromJust will be okay but relies on getting LBCState manipulation right
      editWidgets x = fmapMaybe editedToCI <$> selectViewListWithKey selectionDyn x editF
  postbuild <- getPostBuild
  lbcSelDyn <- foldDyn updateSelection (LBCSelection mk0) selUpdateEv
  let selectionDyn = lbcSelection <$> lbcSelDyn
  selectEv <- selectableKeyList 10 labelF lbcMapDyn reselectEv
  let selUpdateEv = leftmost [selectEv]


  mapUpdateEv <- editWidgets mapDyn -- Event t (MapChangeValue k v)
  let newMapEv = attachWith (flip updateMap) (current lbcMapDyn) mapUpdateEv
      reselectEv = leftmost [reselect <$> mapUpdateEv, mk0 <$ postbuild]
  lbcMapDyn <- holdDyn (LBCMap $ toMaybeMap map0) newMapEv
  let mapDyn = lbcMap <$> lbcMapDyn
  return $ fromMaybeMap <$> mapDyn

editWidget::(DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace, Show v, Read v)=>Dynamic t v -> m (Event t v)
editWidget vDyn = do
  postbuild <- getPostBuild
  let initialEv = T.pack . show <$> attachWith const (current vDyn) postbuild
      newvEv = T.pack . show <$> updated vDyn
      config = def {_textInputConfig_setValue = leftmost [initialEv,newvEv] }
  fmapMaybe (readMaybe . T.unpack) . _textInput_input <$> textInput config

selectableWidget::(DomBuilder t m,PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace, Read v, Show v)
  =>Dynamic t v
  ->Dynamic t Bool
  ->m (Event t v)
selectableWidget valDyn selDyn = do
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> selDyn
  elDynAttr "div" widgetAttrs $ editWidget valDyn


editWidget'::(DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace, Show v, Read v)=>v -> m (Event t v)
editWidget' v = fmapMaybe (readMaybe . T.unpack) . _textInput_input <$> textInput def { _textInputConfig_initialValue = T.pack . show $ v }

selectableWidget'::(MonadHold t m,DomBuilder t m,PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace, Read v, Show v)
  =>Dynamic t v
  ->Dynamic t Bool
  ->m (Event t v)
selectableWidget' valDyn selDyn = do
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> selDyn
  let dynWidget = editWidget' <$> valDyn -- Dynamic t (m (Event t v))
  evEv <- elDynAttr "div" widgetAttrs $ dyn dynWidget --  (Event t (Event t v))
  switch <$> hold never evEv



hiddenCSS::M.Map T.Text T.Text
hiddenCSS  = "style" =: "display: none"

visibleCSS::M.Map T.Text T.Text
visibleCSS = "style" =: "display: inline"

toMaybeMap::Ord k=>M.Map k v->M.Map (Maybe k) (Maybe v)
toMaybeMap = M.insert Nothing Nothing . M.fromList . fmap (\(k,v)->(Just k,Just v)) . M.toList

fromMaybeMap::Ord k=>M.Map (Maybe k) (Maybe v) -> M.Map k v
fromMaybeMap = M.fromList . catMaybes . fmap (\(mk, mv)-> (,) <$> mk <*> mv) . M.toList

selectableKeyList::(DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, Ord k, Show k)
  =>Int -- max size for dropdown
  ->(k->v->T.Text) -- function to label the map entries
  ->Dynamic t (LBCMap k v)  -- the map
  ->Event t (Maybe k)
  ->m (Event t (LBCSelectionUpdate k))
selectableKeyList maxDDSize labelF curMapDyn curSelEv = do
  let mDyn = lbcMap <$> curMapDyn
      sizeF n = min maxDDSize n
      sizeDyn = M.size <$> mDyn
      ddAttrs = ddAttrsDyn sizeF sizeDyn
      labelF' Nothing _ = ""
      labelF' _ Nothing = ""
      labelF' (Just k) (Just v) = labelF k v
      ddMap = traceDyn "ddMap: " $ M.mapWithKey labelF' <$> mDyn
      config = DropdownConfig curSelEv ddAttrs
  fmap SelectionUpdate . traceEvent "selectEv: " . _dropdown_change <$> dropdown Nothing ddMap config

ddAttrsDyn::Reflex t=>(Int->Int)->Dynamic t Int->Dynamic t AttributeMap
ddAttrsDyn sizeF = fmap (\n->if n==0 then hiddenCSS else visibleCSS <> ("size" =: (T.pack . show $ sizeF n)))

-- this is a (poor) substitute for selectViewListwithkey
myWidgetList::(DomBuilder t m, MonadHold t m, PostBuild t m,Reflex t,Monad m,Eq k)
  => Dynamic t k
  -> Dynamic t (M.Map k v)
  -> (v->Dynamic t Bool->m (Event t a))
  -> m (Event t (k,a))
myWidgetList selDyn mapDyn widgetF = do
  let selWidgetF keyDyn f (key,val) = fmap (key,) <$> f val ((==key) <$> keyDyn)
      listOfWidgetsDyn = fmap (selWidgetF selDyn widgetF) .  M.toList <$> mapDyn
      eventWidgetDyn = (fmap leftmost) . sequence <$> listOfWidgetsDyn -- Dynamic t (m (Event t (k,a)))
  eventEv <- dyn eventWidgetDyn -- Event t (Event t (k,a)).  NB, this is not an efficient way to handle this!
  switch <$> hold never eventEv -- does this hold make these outputs suitable for input?
