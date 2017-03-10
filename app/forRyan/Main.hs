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

testWidget::JSM()
testWidget = mainWidget $ do
  el "span" $ text "unrestricted editWidget"
  unrestrictedWidgetEv <- updated <$> editWidgetDyn' id ("unrestricted"::T.Text) (constDyn (1::Int))
  urwDyn <- holdDyn Nothing (Just <$> fmapMaybe id unrestrictedWidgetEv)
  el "br" blank
  dynText $ T.pack . show <$> urwDyn
  el "br" blank

  el "span" $ text "restrictWidget"
  restrictedWidgetEv <- updated <$> editWidgetDyn' (restrictWidget blurOrEnter) ("restrictWidget"::T.Text) (constDyn (1::Int))
  el "br" blank
  rwDyn <- holdDyn Nothing (Just <$> fmapMaybe id restrictedWidgetEv)
  dynText $ T.pack .show <$> rwDyn
  el "br" blank

  el "span" $ text "restrictWidget'"
  restrictedWidgetEv' <- updated <$> editWidgetDyn' (restrictWidget' blurOrEnter) ("restrictWidget'"::T.Text) (constDyn (1::Int))
  el "br" blank
  rwDyn' <- holdDyn Nothing (Just <$> fmapMaybe id restrictedWidgetEv')
  dynText $ T.pack .show <$> rwDyn'
  el "br" blank

  
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


type WidgetConstraints t m k v = (MonadWidget t m, DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace, Show v, Read v, Ord k, Show k)


-- simplest.  Use listWithKey.  This will be for ReadOnly and fixed element (no adds or deletes allowed) uses. 
buildLBEMapLWK::WidgetConstraints t m k v=>Dynamic t (M.Map k v)->m (Dynamic t (M.Map k v))
buildLBEMapLWK map0Dyn = do
  mapOfDyn <- listWithKey (traceDynWith (\m -> "LWK map0Dyn: " ++ show m) map0Dyn) editWidgetDyn -- Dynamic t (M.Map k (Dynamic t (Maybe v)))
  return $ M.mapMaybe id <$> (join $ distributeMapOverDynPure <$> mapOfDyn)

-- NB: ListViewWithKey returns an Event t (M.Map k v) but it contains only the keys for which things have changed
-- NB: ListViewWithKey gets only mapDyn0 as input.  Only need to update if something *else* changes the map.
buildLBEMapLVWK::WidgetConstraints t m k v=>Dynamic t (M.Map k v)->m (Dynamic t (M.Map k v))
buildLBEMapLVWK mapDyn0 = mdo
  let editW k vDyn = updated <$> restrictedEditWidgetDyn k vDyn
  newInputMapEv <- traceDynAsEv (\m->"LVWK mapDyn0" ++ show m) mapDyn0
  mapEditsEv  <- listViewWithKey mapDyn0 editW -- Event t (M.Map k (Maybe v)), carries only updates
  let editedMapEv = traceEventWith (\m->"LVWK editedMap: " ++ show m) $ attachWith (flip applyMap) (current mapDyn) mapEditsEv
      mapEv = leftmost [newInputMapEv, editedMapEv]
  mapDyn <- holdDyn M.empty mapEv
  return (traceDynWith (\m -> "LVWK mapDyn: " ++ show (M.keys m)) mapDyn)


restrictedEditWidgetDyn::WidgetConstraints t m k v=>k->Dynamic t v-> m (Dynamic t (Maybe v))
restrictedEditWidgetDyn = editWidgetDyn' (restrictWidget' blurOrEnter)

editWidgetDyn::WidgetConstraints t m k v=>k->Dynamic t v-> m (Dynamic t (Maybe v))
editWidgetDyn = editWidgetDyn' id

editWidgetDyn'::WidgetConstraints t m k v=>(GWidget t m T.Text->GWidget t m T.Text)->k->Dynamic t v-> m (Dynamic t (Maybe v))
editWidgetDyn' f k vDyn = do
  inputEv' <- traceDynAsEv (\x->"editWidgeDyn' input: v=" ++ show x) vDyn
  let inputEv = T.pack . show <$> inputEv'
      config = WidgetConfig inputEv "" (constDyn M.empty)
  el "span" $ text (T.pack $ show k)
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


{-
editAndDeleteWidgetEv::WidgetConstraints t m k v=>Dynamic t Bool->k->Dynamic t v-> m (Event t (Maybe v))
editAndDeleteWidgetEv selDyn k vDyn = mdo
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> visibleDyn
  (visibleDyn,outEv) <- elDynAttr "div" widgetAttrs $ do
    resEv <-  updated <$> editWidgetDyn k vDyn
    delButtonEv <- buttonNoSubmit "-"
    selEv <- dynAsEv selDyn
    visDyn <-  holdDyn True $ leftmost
               [
                 selEv
               , False <$ delButtonEv -- delete button pressed, so hide
               , True <$ updated vDyn -- value updated so make sure it's visible (in case of re-use of deleted key)
               ]
    let outEv' = leftmost
                 [
                   Just <$> fmapMaybe id resEv
                 , Nothing <$ delButtonEv
                 ]           
    return (visDyn,outEv')
  return outEv
-}
