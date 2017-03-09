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

testWidget::JSM()
testWidget = mainWidget $ do
  let x0 = M.fromList [("A",1),("B",2)]
  el "span" $ text "editWidget:  "
  res <- buildLBEMapLVWK (constDyn x0)
  el "br" blank
  el "br" blank
  el "span" $ text "dynText: "
  dynText $ T.pack . show <$> res
  el "br" blank
  el "br" blank  
  el "span" $ text "showWidget: "
  _ <- buildLBEMapLWK res
  return ()


type WidgetConstraints t m k v = (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace, Show v, Read v, Ord k, Show k)

-- simplest.  Use listWithKey.  This will be for ReadOnly and fixed element (no adds or deletes allowed) uses. 
buildLBEMapLWK::WidgetConstraints t m k v=>Dynamic t (M.Map k v)->m (Dynamic t (M.Map k v))
buildLBEMapLWK map0Dyn = do
  mapOfDyn <- listWithKey (traceDynWith (\m -> "LWK map0Dyn: " ++ show m) map0Dyn) editWidgetDyn -- Dynamic t (M.Map k (Dynamic t (Maybe v)))
  return $ M.mapMaybe id <$> (join $ distributeMapOverDynPure <$> mapOfDyn)

editWidgetDyn::WidgetConstraints t m k v=>k->Dynamic t v-> m (Dynamic t (Maybe v))
editWidgetDyn k vDyn = do
  inputEv' <- traceDynAsEv (\x->"editWidget: v=" ++ show x) vDyn
  let inputEv = T.pack . show <$> inputEv'
      config = def {_textInputConfig_setValue = inputEv }
  el "span" $ text (T.pack $ show k)
  valDyn <- _textInput_value <$> textInput config
  return $ readMaybe . T.unpack <$> valDyn 


-- NB: ListViewWithKey returns an Event t (M.Map k v) but it contains only the keys for which things have changed
-- NB: ListViewWithKey gets only mapDyn0 as input.  Only need to update if something *else* changes the map.
buildLBEMapLVWK::WidgetConstraints t m k v=>Dynamic t (M.Map k v)->m (Dynamic t (M.Map k v))
buildLBEMapLVWK mapDyn0 = mdo
  let editW = editAndDeleteWidgetEv (constDyn True)
  newInputMapEv <- traceDynAsEv (\m->"LVWK mapDyn0" ++ show m) mapDyn0
  mapEditsEv  <- listViewWithKey mapDyn0 editW -- Event t (M.Map k (Maybe v)), carries only updates
  let editedMapEv = traceEventWith (\m->"LVWK editedMap: " ++ show m) $ attachWith (flip applyMap) (current mapDyn) mapEditsEv
      mapEv = leftmost [newInputMapEv, editedMapEv]
  mapDyn <- holdDyn M.empty mapEv
  return (traceDynWith (\m -> "LVWK mapDyn: " ++ show (M.keys m)) mapDyn)


editWidgetEv::WidgetConstraints t m k v=>k->Dynamic t v-> m (Event t (Maybe v))
editWidgetEv k vDyn = updated <$> editWidgetDyn k vDyn

editWidgetEv'::WidgetConstraints t m k v=>k->Dynamic t v-> m (Event t (Maybe v))
editWidgetEv' k vDyn = do
  inputEv' <- traceDynAsEv (\x->"editWidget: v=" ++ show x) vDyn
  let inputEv = T.pack . show <$> inputEv'
      config = def {_textInputConfig_setValue = inputEv }
  el "span" $ text (T.pack $ show k)
  valEv <- _textInput_input <$> textInput config
  return $ readMaybe . T.unpack <$> valEv


editAndDeleteWidgetEv::WidgetConstraints t m k v=>Dynamic t Bool->k->Dynamic t v-> m (Event t (Maybe v))
editAndDeleteWidgetEv selDyn k vDyn = mdo
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> visibleDyn
  (visibleDyn,outEv) <- elDynAttr "div" widgetAttrs $ do
    resDyn <-  editWidgetDyn k vDyn
    delButtonEv <- buttonNoSubmit "-"
    selEv <- dynAsEv selDyn
    visDyn <-  holdDyn True $ leftmost
               [
                 selEv
               , False <$ delButtonEv -- delete button pressed, so hide
               , True <$ (updated vDyn) -- value updated so make sure it's visible (in case of re-use of deleted key)
               ]
    let outEv' = leftmost
                 [
                   Just <$> (fmapMaybe id $ updated resDyn)
                 , Nothing <$ delButtonEv
                 ]           
    return (visDyn,outEv')
  return outEv
  
buttonNoSubmit::DomBuilder t m=>T.Text -> m (Event t ())
buttonNoSubmit t = (domEvent Click . fst) <$> elAttr' "button" ("type" =: "button") (text t)

{-
editOneEv::(SimpleFormInstanceC t m, VFormBuilderC t m v,Show k)=>Dynamic t Bool->k->Dynamic t v->SFR t m (Event t (Maybe v))
editOneEv selDyn k valDyn = mdo
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> visibleDyn
  (visibleDyn,outEv) <- elDynAttr "div" widgetAttrs $ sfRow $ do
    sfItem $ el "div" $ text (T.pack $ show k)
    resDynAV <-  sfItem $ unDynValidation <$> (unSF $ buildForm' Nothing (Just valDyn)) -- Dynamic t (AccValidation) val
    delButtonEv <- sfItem $ sfCenter LayoutVertical . sfItemR . lift $ containerActionButton "-" -- Event t ()
    let resDyn = avToMaybe <$> resDynAV -- Dynamic t (Maybe v)
    inputSelEv <- dynAsEv selDyn
    visibleDyn' <- holdDyn True $ leftmost [inputSelEv -- calling widget
                                               , False <$ delButtonEv -- delete button pressed, so hide
                                               , True <$ (updated valDyn) -- value updated so make sure it's visible (in case of re-use of deleted key)
                                               ]
    return (visibleDyn',leftmost [Just <$> (fmapMaybe id $ updated resDyn), Nothing <$ delButtonEv])
  return outEv

-- now with ListViewWithKeyShallowDiff just so I understand things.
buildLBEMapLVWKSD::(SimpleFormInstanceC t m
                  , VFormBuilderC t m v
                  , Ord k, Show k)
                => LBBuildF t m k v
buildLBEMapLVWKSD mf mapDyn0 = mdo
  newInputMapEv <- dynAsEv mapDyn0
  updateEvsDyn <- listWithKeyShallowDiff M.empty diffMapEv editOneSD -- Dynamic t (Map k (Event t (Maybe v)))
  let mapEditsEv =  switch $ mergeMap <$> current updateEvsDyn -- Event t (Map k (Maybe v))
      diffMapEv = traceEventWith (\m -> "new Input to buildLBEMapLVWKSD: " ++ show (M.keys m)) $ fmap Just <$> newInputMapEv 
      editedMapEv = attachWith (flip applyMap) (current mapDyn) mapEditsEv
      newMapEv = leftmost [newInputMapEv, editedMapEv]
  mapDyn <- holdDyn M.empty newMapEv
  return (traceDynWith (\m -> "LVWKSD mapDyn: " ++ show (M.keys m)) mapDyn)

editWidgetSD::WidgetConstraints t m k v=>k->v->Event t v->SFR t m (Event t (Maybe v))
editWidgetSD k v0 vEv = holdDyn v0 vEv >>= editOneEv k


-}

dynAsEv::PostBuild t m=>Dynamic t a -> m (Event t a)
dynAsEv dyn = (\x->leftmost [updated dyn, tag (current dyn) x]) <$> getPostBuild 


traceDynAsEv::PostBuild t m=>(a->String)->Dynamic t a->m (Event t a)
traceDynAsEv f dyn = do
  postbuild <- getPostBuild
  let f' prefix x = prefix ++ f x
      pbEv = traceEventWith (f' "postbuild-") $ tag (current dyn) postbuild
      upEv = traceEventWith (f' "update-") $ updated dyn
  return $ leftmost [upEv, pbEv] 


hiddenCSS::M.Map T.Text T.Text
hiddenCSS  = "style" =: "display: none"

visibleCSS::M.Map T.Text T.Text
visibleCSS = "style" =: "display: inline"

