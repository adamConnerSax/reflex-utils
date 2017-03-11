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
type WidgetConstraints t m k v = (ReflexConstraints t m, Show v, Read v, Ord k, Read k, Show k)

type FieldWidgetDyn t m v = Maybe (Dynamic t v)-> m (Dynamic t (Maybe v))
type FieldWidgetEv t m v = Maybe (Dynamic t v)-> m (Event t (Maybe v))

data FieldWidget t m v = FWDyn (FieldWidgetDyn t m v) | FWEv (FieldWidgetEv t m v)

keyDisplayWidget::WidgetConstraints t m k v=>FieldWidget t m v->k->FieldWidget t m v
keyDisplayWidget vWidget k = \mvDyn ->
  case vWidget of
    (FWDyn wDyn) -> FWDyn $ el "span" $ text k >> wDyn mvDyn
    (FWEv wEv) -> FWEv $ el "span" $ text k >> wEv mvDyn

fieldWidgetEv::(Functor m, Reflex t)=>FieldWidget t m v->FieldWidgetEv t m v
fieldWidgetEv (FWEv wEv) mvDyn = wEv mvDyn
fieldWidgetEv (FWDyn wDyn) mvDyn = updated <$> wDyn mvDyn


fieldWidgetDyn::MonadHold t m=>FieldWidget t m v->FieldWidgetDyn t m v
fieldWidgetDyn (FWDyn wDyn) mvDyn = wDyn mvDyn
fieldWidgetDyn (FWEv wEv) mvDyn = wEv mvDyn >>= holdDyn Nothing 


type EditF t m k v = Dynamic t (M.Map k v)->m (Dynamic t (M.Map k v))

testWidget::JSM()
testWidget = mainWidget $ do
  let unrestrictedWidget::WidgetConstraints t m k v=>FieldWidget t m v
      unrestrictedWidget = FWDyn $ fieldWidgetDyn' id
      contribRestrictedWidget::WidgetConstraints t m v=>FieldWidget t m v
      contribRestrictedWidget = editAndDeleteFieldWidget (FWDyn $ fieldWidgetDyn' (restrictWidget blurOrEnter)) (constDyn True)
      modifiedRestrictedWidget::WidgetConstraints t m v=>FieldWidget t m v
      modifiedRestrictedWidget = FWDyn $ fieldWidgetDyn' (restrictWidget' blurOrEnter)
      x0 = M.fromList [("A",1),("B",2)]

{-      
  testSingleWidget "unrestricted" unrestrictedWidget (1::Int)
  testSingleWidget "restrictWidget" contribRestrictedWidget (1::Int)
  testSingleWidget "restrictWidget'" modifiedRestrictedWidget (1::Int) 
-} 

--  testLVWKWithWidget "unrestricted" unrestrictedWidget x0
  testEditorWithWidget "restrictWidget" buildLBEMapLVWK contribRestrictedWidget x0
--  testLVWKWithWidget "restrictWidget'" modifiedRestrictedWidget x0
  

testSingleWidget::WidgetConstraints t m T.Text v=>T.Text->FieldWidget t m v->v->m ()
testSingleWidget label keyWidget valWidget v0 = do
  el "h2" $ text label >> el "br" blank
  el "span" $ text $ "widget:  "
  widgetEv <- fieldWidgetEv widget (Just $ constDyn v0)
  resDyn <- holdDyn Nothing (Just <$> fmapMaybe id widgetEv)
  el "br" blank
  el "span" (text "dynText of holdDyn of widget events: ")
  dynText $ T.pack . show <$> resDyn
  bigBreak
  


testEditorWithWidget::WidgetConstraints t m k v
  =>T.Text
  ->(FieldWidget t m k->FieldWidget t m v ->EditF t m k v) -- widget to edit map
  ->FieldWidget t m k -- widget to edit one key value
  ->FieldWidget t m v -- widget to edit one map value
  ->M.Map k v -- initial map
  ->m ()
testEditorWithWidget label editWidget editOneWidget map0 = do
  el "h2" $ text label >> el "br" blank
  el "span" $ text "editWidget:  "
  resDyn <- editWidget editOneWidget (constDyn map0)
  smallBreak
  el "span" $ text "dynText: "
  dynText $ T.pack . show <$> resDyn
  smallBreak
  el "span" $ text "showWidget: "
  _ <- buildLBEMapLWK (FWDyn . readOnlyFieldWidget $  fieldWidgetDyn' id) resDyn
  bigBreak


smallBreak::DomBuilder t m=>m ()
smallBreak =   el "br" blank >> el "br" blank
  
bigBreak::DomBuilder t m=>m()
bigBreak =   el "br" blank >> el "h1" (text "") >> el "br" blank


-- simplest.  Use listWithKey.  This will be for ReadOnly and fixed element (no adds or deletes allowed) uses. 
buildLBEMapLWK::WidgetConstraints t m k v=>FieldWidget t m k->FieldWidget t m v->EditF t m k v
buildLBEMapLWK _ editOneValue map0Dyn = do
  let editW k vDyn = fieldWidgetDyn (keyDisplayWidget editOneValue) k (Just vDyn)
  mapOfDyn <- listWithKey map0Dyn editW -- Dynamic t (M.Map k (Dynamic t (Maybe v)))
  return $ M.mapMaybe id <$> (join $ distributeMapOverDynPure <$> mapOfDyn)

-- NB: ListViewWithKey returns an Event t (M.Map k v) but it contains only the keys for which things have changed
-- NB: ListViewWithKey gets only mapDyn0 as input.  Only need to update if something *else* changes the map.
buildLBEMapLVWK::WidgetConstraints t m k v=>FieldWidget t m k->FieldWidget t m v->EditF t m k v
buildLBEMapLVWK _ editOneValue mapDyn0 = mdo
  let editW k vDyn = fieldWidgetEv (keyDisplayWidget editOneWidget) k (Just vDyn)
  newInputMapEv <- dynAsEv mapDyn0
  mapEditsEv  <- listViewWithKey mapDyn0 editW -- Event t (M.Map k (Maybe v)), carries only updates
  let editedMapEv = attachWith (flip applyMap) (current mapDyn) mapEditsEv
      mapEv = leftmost [newInputMapEv, editedMapEv]
  mapDyn <- holdDyn M.empty mapEv
  return mapDyn


buildLBEMapWithAdd::WidgetConstraints t m k v
  => (FieldWidget t m k->FieldWidget t m v-> EditF t m k v) -- base map editor
  -> FieldWidget t m k -- single key editor
  -> FieldWidget t m v -- single value editor
  -> EditF t m k v 
buildLBEMapWithAdd baseEditor keyWidget fieldWidget map0Dyn = mdo
  initialMapEv <- dynAsEv mapDyn0
  editedMapDyn <- baseEditor keyWidget fieldWidget mapDyn -- Dynamic t (M.Map k v)
  addEv <- mdo -- Event t (k,v)
    newKey <- keyWidget Nothing    -- (Dynamic t (Maybe k)
    newVal <- fieldWidget Nothing  -- (Dynamic t (Maybe v)
    addButtonEv <- buttonNoSubmit "+" -- Event t ()
    let newPairEv = tag (current $ zipDynWith (,) newKey newVal) addButtonEv
    return $ fmapMaybe id newPairEv
  let mapWithAdditionEv = attachWith (\m (k,v)->M.insert k v m) (current editedMapDyn) addEv
  mapDyn <- holdDyn M.empty (leftmost [initialMapEv, mapWithAdditionEv])
  return editedMapDyn

-- single field widgets

readOnlyFieldWidget::(ReflexConstraints t m, Show v, Read v)=>FieldWidgetDyn t m v -> FieldWidgetDyn t m v
readOnlyFieldWidget fWidget = 
  let makeReadOnly wFunc (WidgetConfig setVal initialVal dAttrs) =
        let dAttrs' = M.insert "readonly" "" <$> dAttrs
        in wFunc (WidgetConfig setVal initialVal dAttrs')
  in fieldWidgetDyn' makeReadOnly

fieldWidgetDyn'::(ReflexConstraints t m, Show v, Read v)=>(GWidget t m T.Text->GWidget t m T.Text)->FieldWidgetDyn t m v
fieldWidgetDyn' f mvDyn = do
  inputEv' <- maybe (return never) (traceDynAsEv (\x->"editWidgeDyn' input: v=" ++ show x)) mvDyn -- traced so we can see when widgets are updated vs rebuilt vs left alone
  let inputEv = T.pack . show <$> inputEv'
      config = WidgetConfig inputEv "" (constDyn M.empty)
  valDyn <- _hwidget_value <$> f (htmlTextInput "text") config
  return $ readMaybe . T.unpack <$> valDyn 

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


editAndDeleteFieldWidget::(ReflexConstraints t m, Read v, Show v)=>FieldWidget t m v->Dynamic t Bool->FieldWidget t m v
editAndDeleteFieldWidget baseWidget visibleDyn = FWEv $ \k mvDyn -> mdo
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> visibleDyn'
      newInputEv = maybe never updated mvDyn 
  (visibleDyn',outEv) <- elDynAttr "div" widgetAttrs $ do
    resEv <-  fieldWidgetEv baseWidget k mvDyn
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


