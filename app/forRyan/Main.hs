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
import           Reflex.Dom.Old (MonadWidget)
import Control.Monad.Ref
import Data.IORef

import           Control.Monad                    (join)
import           Control.Monad.Fix                (MonadFix)

import qualified Data.Map                         as M
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T

import           System.Process                   (spawnProcess)
import           Text.Read                        (readMaybe)
import Reflex.Dom.Contrib.Widgets.Common 


-- NB: This is just for warp.
main::IO ()
main = do
  let port :: Int = 3702
  pHandle <- spawnProcess "open" ["http://localhost:" ++ show port]
  run port testWidget


type ReflexConstraints t m = (MonadWidget t m, DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
type WidgetConstraints t m k v = (ReflexConstraints t m, Show v, Read v, Ord k, Show k, Read k)

type FieldWidgetDyn t m v = Maybe (Dynamic t v)-> m (Dynamic t (Maybe v))
type FieldWidgetEv t m v = Maybe (Dynamic t v)-> m (Event t (Maybe v))

data FieldWidget t m v = FWDyn (FieldWidgetDyn t m v) | FWEv (FieldWidgetEv t m v)

type FieldWidgetWithKey t m k v = k->FieldWidget t m v


addFixedKeyToWidget::ReflexConstraints t m=>(k->T.Text)->FieldWidget t m v -> FieldWidgetWithKey t m k v
addFixedKeyToWidget printK fw = \k -> case fw of
  (FWDyn wDyn) -> FWDyn $ \mvDyn -> el "span" $ text (printK k) >> wDyn mvDyn
  (FWEv wEv) -> FWEv $ \mvDyn -> el "span" $ text (printK k) >> wEv mvDyn
  
fieldWidgetEv::(Functor m, Reflex t)=>FieldWidget t m v->FieldWidgetEv t m v
fieldWidgetEv (FWEv wEv) mvDyn = wEv mvDyn
fieldWidgetEv (FWDyn wDyn) mvDyn = updated <$> wDyn mvDyn


fieldWidgetDyn::MonadHold t m=>FieldWidget t m v->FieldWidgetDyn t m v
fieldWidgetDyn (FWDyn wDyn) mvDyn = wDyn mvDyn
fieldWidgetDyn (FWEv wEv) mvDyn = wEv mvDyn >>= holdDyn Nothing 


type EditF t m k v = Dynamic t (M.Map k v)->m (Dynamic t (M.Map k v))

testWidget::JSM()
testWidget = mainWidget $ do
  let simpleWidget::(ReflexConstraints t m, Show v, Read v)=>FieldWidget t m v
      simpleWidget = FWDyn $ fieldWidgetDyn' (readMaybe . T.unpack)  (restrictWidget' blurOrEnter)
      textWidget::ReflexConstraints t m=>FieldWidget t m T.Text
      textWidget = FWDyn $ fieldWidgetDyn' Just (restrictWidget blurOrEnter)
      keyedWidget::(ReflexConstraints t m, Show v, Read v)=>T.Text->FieldWidget t m v
      keyedWidget = addFixedKeyToWidget id simpleWidget
      x0 = M.fromList [("A",1),("B",2)]

  el "h1" $ text "Using ListWithKey"
  testEditorWithWidget "edit only" (buildLBEMapLWK keyedWidget) x0
  testEditorWithWidget "edit and delete" (buildLBEMapWithDelete buildLBEMapLWK keyedWidget) x0
  testEditorWithWidget "edit and add and delete" (buildLBEMapWithAdd (buildLBEMapWithDelete buildLBEMapLWK keyedWidget) textWidget simpleWidget) x0
  
  el "h1" $ text "Using ListViewWithKey"
  testEditorWithWidget "edit and add and delete" (buildLBEMapWithAdd (buildLBEMapWithDelete buildLBEMapLVWK keyedWidget) textWidget simpleWidget) x0

  el "h1" $ text "Using ListViewWithKeyShallowDiff"
  testEditorWithWidget "edit and add and delete" (buildLBEMapWithAdd (buildLBEMapWithDelete buildLBEMapLVWKSD keyedWidget) textWidget simpleWidget) x0


testSingleWidget::(ReflexConstraints t m, Show v, Read v)=>T.Text->FieldWidget t m v->v->m ()
testSingleWidget label valWidget v0 = do
  el "h2" $ text label >> el "br" blank
  el "span" $ text $ "widget:  "
  widgetEv <- fieldWidgetEv valWidget (Just $ constDyn v0)
  resDyn <- holdDyn Nothing (Just <$> fmapMaybe id widgetEv)
  el "br" blank
  el "span" (text "dynText of holdDyn of widget events: ")
  dynText $ T.pack . show <$> resDyn
  bigBreak
  
testEditorWithWidget::WidgetConstraints t m T.Text v
  =>T.Text
  ->EditF t m T.Text v -- editor
  ->M.Map T.Text v -- initial map
  ->m ()
testEditorWithWidget label editWidget map0 = do
  el "h2" $ text label >> el "br" blank
  el "span" $ text "editable values:  "
  resDyn <- editWidget (constDyn map0)
  smallBreak
  el "span" $ text "dynText: "
  dynText $ T.pack . show <$> resDyn
  smallBreak
  el "span" $ text "showWidget: "
  _ <- buildLBEMapLWK (addFixedKeyToWidget id (FWDyn $ readOnlyFieldWidget)) resDyn
  bigBreak


smallBreak::DomBuilder t m=>m ()
smallBreak =   el "br" blank >> el "br" blank
  
bigBreak::DomBuilder t m=>m()
bigBreak =   el "br" blank >> el "h1" (text "") >> el "br" blank


-- simplest.  Use listWithKey.  This will be for ReadOnly and fixed element (no adds or deletes allowed) uses. 
buildLBEMapLWK::WidgetConstraints t m k v=>FieldWidgetWithKey t m k v->EditF t m k v
buildLBEMapLWK editOneValueWK map0Dyn = do
  let editW k vDyn = fieldWidgetDyn (editOneValueWK k) (Just vDyn)
  mapOfDyn <- listWithKey map0Dyn editW -- Dynamic t (M.Map k (Dynamic t (Maybe v)))
  return $ M.mapMaybe id <$> (join $ distributeMapOverDynPure <$> mapOfDyn)


-- NB: ListViewWithKey returns an Event t (M.Map k v) but it contains only the keys for which things have changed
-- NB: ListViewWithKey gets only mapDyn0 as input.  Only need to update if something *else* changes the map.
buildLBEMapLVWK::WidgetConstraints t m k v=>FieldWidgetWithKey t m k v->EditF t m k v
buildLBEMapLVWK editOneValueWK mapDyn0 = mdo
  let editW k vDyn = fieldWidgetEv (editOneValueWK k) (Just vDyn)
  newInputMapEv <- dynAsEv mapDyn0
  mapEditsEv  <- listViewWithKey mapDyn0 editW -- Event t (M.Map k (Maybe v)), carries only updates
  let editedMapEv = attachWith (flip applyMap) (current mapDyn) mapEditsEv
      mapEv = leftmost [newInputMapEv, editedMapEv]
  mapDyn <- holdDyn M.empty mapEv
  return mapDyn

-- now with ListViewWithKeyShallowDiff just so I understand things.
buildLBEMapLVWKSD::WidgetConstraints t m k v=>FieldWidgetWithKey t m k v->EditF t m k v
buildLBEMapLVWKSD editOneValueWK mapDyn0 = mdo
  let editW k v0 vEv = holdDyn v0 vEv >>= \vDyn -> (fieldWidgetEv (editOneValueWK k)) (Just vDyn)
  newInputMapEv <- dynAsEv mapDyn0
  updateEvsDyn <- listWithKeyShallowDiff M.empty diffMapEv editW -- Dynamic t (Map k (Event t (Maybe v)))
  let mapEditsEv =  switch $ mergeMap <$> current updateEvsDyn -- Event t (Map k (Maybe v))
      diffMapEv = traceEventWith (\m -> "new Input to buildLBEMapLVWKSD: " ++ show (M.keys m)) $ fmap Just <$> newInputMapEv 
      editedMapEv = attachWith (flip applyMap) (current mapDyn) mapEditsEv
      newMapEv = leftmost [newInputMapEv, editedMapEv]
  mapDyn <- holdDyn M.empty newMapEv
  return $ traceDynWith (\m -> "LVWKSD mapDyn: " ++ show (M.keys m)) mapDyn



buildLBEMapWithDelete::WidgetConstraints t m k v
  =>(FieldWidgetWithKey t m k v->EditF t m k v)
  ->FieldWidgetWithKey t m k v->EditF t m k v
buildLBEMapWithDelete buildBase valWidgetWK = buildBase (editAndDeleteFieldWidgetWithKey valWidgetWK (constDyn True)) 


buildLBEMapWithAdd::WidgetConstraints t m k v
  => EditF t m k v -- base map editor
  -> FieldWidget t m k -- single key editor
  -> FieldWidget t m v -- single value editor
  -> EditF t m k v 
buildLBEMapWithAdd baseEditor keyWidget valWidget map0Dyn = mdo
  initialMapEv <- dynAsEv map0Dyn
  editedMapDyn <- baseEditor mapDyn -- Dynamic t (M.Map k v)
  el "br" blank
  el "span" $ text "Add: "
  addEv <- mdo -- Event t (k,v)
    let newMaybePairWidget = mdo 
          newKey <- fieldWidgetDyn keyWidget Nothing    -- (Dynamic t (Maybe k)
          newVal <- fieldWidgetDyn valWidget Nothing  -- (Dynamic t (Maybe v)
          return $ (\(ma,mb) -> (,) <$> ma <*> mb) <$> zipDynWith (,) newKey newVal
        addMaybePairWidget = join <$> widgetHold newMaybePairWidget (newMaybePairWidget <$ newPairEv) 
    newMaybePairDyn <- addMaybePairWidget
    addButtonEv <- buttonNoSubmit "+" -- Event t ()
    let newPairEv = fmapMaybe id $ tag (current newMaybePairDyn) addButtonEv
    return newPairEv
  let mapWithAdditionEv = attachWith (\m (k,v)->M.insert k v m) (current editedMapDyn) addEv
  mapDyn <- holdDyn M.empty (leftmost [initialMapEv, mapWithAdditionEv])
  return editedMapDyn

-- single field widgets

readOnlyFieldWidget::(ReflexConstraints t m, Show v)=>FieldWidgetDyn t m v
readOnlyFieldWidget = 
  let makeReadOnly wFunc (WidgetConfig setVal initialVal dAttrs) =
        let dAttrs' = M.insert "readonly" "" <$> dAttrs
        in wFunc (WidgetConfig setVal initialVal dAttrs')
  in fieldWidgetDyn' (const Nothing) makeReadOnly

fieldWidgetDyn'::(ReflexConstraints t m, Show v)=>(T.Text -> Maybe v)->(GWidget t m T.Text->GWidget t m T.Text)->FieldWidgetDyn t m v
fieldWidgetDyn' parse f mvDyn = do
  inputEv' <- maybe (return never) (traceDynAsEv (\x->"editWidgeDyn' input: v=" ++ show x)) mvDyn -- traced so we can see when widgets are updated vs rebuilt vs left alone
  let inputEv = T.pack . show <$> inputEv'
      config = WidgetConfig inputEv "" (constDyn M.empty)
  valDyn <- _hwidget_value <$> f (htmlTextInput "text") config
  return $ parse <$> valDyn 

-- like Reflex.Dom.Contrib.Widgets.Common.restrictWidget but allows the set event to change the "authoritative value"
-- Q1:  Why doesn't this version update when built?
restrictWidget'::(DomBuilder t m, MonadHold t m)
  =>(HtmlWidget t a -> Event t a)
  -> GWidget t m a
  -> GWidget t m a
restrictWidget' restrictFunc wFunc cfg = do
  w <- wFunc cfg
  let e = leftmost [_widgetConfig_setValue cfg, restrictFunc w]
  v <- holdDyn (_widgetConfig_initialValue cfg) e
  return $ w { _hwidget_value = v
             , _hwidget_change = e
             }


editAndDeleteFieldWidgetWithKey::(ReflexConstraints t m, Read v, Show v)=>FieldWidgetWithKey t m k v->Dynamic t Bool->FieldWidgetWithKey t m k v
editAndDeleteFieldWidgetWithKey baseWidgetWK visibleDyn = \k -> FWEv $ \mvDyn -> mdo
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> visibleDyn'
      newInputEv = maybe never updated mvDyn 
  (visibleDyn',outEv) <- elDynAttr "div" widgetAttrs $ do
    resEv <-  fieldWidgetEv (baseWidgetWK k) mvDyn
    delButtonEv <- buttonNoSubmit "-"
    selEv <- dynAsEv visibleDyn
    visDyn <-  holdDyn True $ leftmost
               [
                 selEv
               , False <$ delButtonEv -- delete button pressed, so hide
               , True <$ newInputEv -- value updated so make sure it's visible (in case of re-use of deleted key)
               ]
    let outEv' = leftmost
                 [
                   Just <$> fmapMaybe id resEv
                 , Nothing <$ delButtonEv
                 ]           
    return (visDyn,outEv')
  return outEv

  
buttonNoSubmit::DomBuilder t m=>T.Text -> m (Event t ())
buttonNoSubmit t = (domEvent Click . fst) <$> elAttr' "button" ("type" =: "button") (text t)


dynAsEv::PostBuild t m=>Dynamic t a -> m (Event t a)
dynAsEv dyn = (\x->leftmost [updated dyn, tagPromptlyDyn dyn x]) <$> getPostBuild 


traceDynAsEv::PostBuild t m=>(a->String)->Dynamic t a->m (Event t a)
traceDynAsEv f dyn = do
  postbuild <- getPostBuild
  let f' prefix x = prefix ++ f x
      pbEv = traceEventWith (f' "postbuild-") $ tagPromptlyDyn dyn postbuild
      upEv = traceEventWith (f' "update-") $ updated dyn
  return $ leftmost [upEv, pbEv] 


hiddenCSS::M.Map T.Text T.Text
hiddenCSS  = "style" =: "display: none"

visibleCSS::M.Map T.Text T.Text
visibleCSS = "style" =: "display: inline"


