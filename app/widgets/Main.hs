{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
module Main where

import           Reflex.Dom.Contrib.CssUtils                   (headElt)
import           Reflex.Dom.Contrib.Layout.ClayUtils           (cssToBS)
import           Reflex.Dom.Contrib.Layout.TabLayout
import           Reflex.Dom.Contrib.Layout.Types               (CssClass (..),
                                                                CssClasses (..),
                                                                LayoutDirection (..),
                                                                LayoutOrientation (..),
                                                                emptyCss,
                                                                oneClass)
import           Reflex.Dom.Contrib.ReflexConstraints          (MonadWidgetExtraC)

import qualified Reflex.Collections.Collections                as RC

import qualified Reflex.Dom.Contrib.Widgets.EditableCollection as EC
import           Reflex.Dom.Contrib.Widgets.SafeDropdown       (SafeDropdown (..),
                                                                SafeDropdownConfig (..),
                                                                safeDropdownOfLabelKeyedValue)
import           Reflex.Dom.Contrib.Widgets.WidgetResult       (dynamicWidgetResultToWidgetResult,
                                                                widgetResultToDynamic)

#ifdef USE_WKWEBVIEW
import           Language.Javascript.JSaddle.WKWebView         (run)
#endif

#ifdef USE_WARP
import           Language.Javascript.JSaddle.Warp              (run)
#endif


import qualified Reflex                                        as R
import qualified Reflex.Dom.Core                               as RD

import           GHCJS.DOM.Types                               (JSM)
import           Reflex.Dom.Contrib.CssUtils                   (CssLink,
                                                                CssLinks (..),
                                                                headElt)

import           Control.Monad.Fix                             (MonadFix)
import           Data.Bool                                     (bool)

import qualified Data.Map                                      as M
import           Data.Maybe                                    (isJust)
import           Data.Monoid                                   ((<>))
import           Data.Proxy                                    (Proxy (..))

import qualified Data.Text                                     as T
import           Text.Read                                     (readMaybe)

import           Prelude                                       hiding (div, rem,
                                                                span)
import qualified System.Process                                as SP

inputValue :: (RD.DomBuilder t m, MonadWidgetExtraC t m, RD.PostBuild t m, Read v) => m (R.Dynamic t (Maybe v))
inputValue = do
  let config = RD.TextInputConfig "text" "" R.never (R.constDyn M.empty)
  fmap (readMaybe . T.unpack) . RD._textInput_value <$> RD.textInput config

editValue :: (RD.DomBuilder t m, MonadWidgetExtraC tq m, RD.PostBuild t m, Show v, Read v) => R.Dynamic t v -> m (R.Dynamic t (Maybe v))
editValue valDyn = do
  postBuild <- RD.getPostBuild
  let inputEv = T.pack . show <$> R.leftmost [R.tag (R.current valDyn) postBuild, R.updated valDyn]
      config = RD.TextInputConfig "text" "" inputEv (R.constDyn M.empty)
  fmap (readMaybe . T.unpack) . RD._textInput_value <$> RD.textInput config

editValueEv :: (RD.DomBuilder t m, MonadWidgetExtraC tq m, RD.PostBuild t m, Show v, Read v) => R.Dynamic t v -> m (R.Event t v)
editValueEv valDyn = do
  postBuild <- R.getPostBuild
  let inputEv = T.pack . show <$> R.leftmost [R.tag (R.current valDyn) postBuild, R.updated valDyn]
      config = RD.TextInputConfig "text" "" inputEv (R.constDyn M.empty)
  R.fmapMaybe (readMaybe . T.unpack) . RD._textInput_input <$> RD.textInput config


editDeleteValue :: (RD.DomBuilder t m, MonadWidgetExtraC t m, RD.PostBuild t m, Show a, Read a) => R.Dynamic t a -> m (R.Event t (Maybe a))
editDeleteValue aDyn = RD.el "div" $ do
  maEv <- RD.el "span" $ editValueEv aDyn
  delEv <- RD.el "span" $ EC.buttonNoSubmit "-"
  return $ R.leftmost [Just <$> maEv, Nothing <$ delEv]

inputPair :: (RD.DomBuilder t m, MonadWidgetExtraC t m, RD.PostBuild t m, Read k, Read v) => m (R.Dynamic t (Maybe (k,v)))
inputPair = do
  let config = RD.TextInputConfig "text" "" R.never (R.constDyn M.empty)
  RD.el "div" $ do
    kMDyn <- fmap (readMaybe . T.unpack) . RD._textInput_value <$> RD.textInput config
    vMDyn <- fmap (readMaybe . T.unpack) . RD._textInput_value <$> RD.textInput config
    let pairDynMaybe = R.zipDyn kMDyn vMDyn
    return $ fmap (\(ma,mb) -> (,) <$> ma <*> mb) pairDynMaybe

inputKeyValue :: (RD.DomBuilder t m, MonadWidgetExtraC t m, RD.PostBuild t m, Read k)
  => m (R.Dynamic t (Maybe v)) -> m (R.Dynamic t (Maybe (k,v)))
inputKeyValue valWidget = do
  let config = RD.TextInputConfig "text" "" R.never (R.constDyn M.empty)
  RD.el "div" $ do
    kMDyn <- fmap (readMaybe . T.unpack) . RD._textInput_value <$> RD.textInput config
    vMDyn <- valWidget --fmap (readMaybe . T.unpack) . RD._textInput_value <$> RD.textInput config
    let pairDynMaybe = R.zipDyn kMDyn vMDyn
    return $ fmap (\(ma,mb) -> (,) <$> ma <*> mb) pairDynMaybe


newLine = RD.el "p" (RD.text "") >> RD.el "br" RD.blank

editableCollectionsWidget :: (RD.DomBuilder t m, MonadWidgetExtraC t m, RD.MonadHold t m, RD.PostBuild t m, MonadFix m) => m ()
editableCollectionsWidget = do
  let testMap :: M.Map T.Text Int = M.fromList [("A",1),("B",2),("C",3),("D",4)]
      testMapOfMaps :: M.Map T.Text (M.Map T.Text Int) = M.fromList [("A",testMap),("B",testMap)]
      testList :: [T.Text] = ["Hello","Goodbye","Cat"]
      testListOfLists :: [[Int]] = [[1,2],[3,4]]
      displayEach = (EC.DisplayEach (R.constDyn M.empty) (T.pack . show))
      display = EC.DisplayAll
  RD.el "p" $ RD.text "v1 @ Map"
  editMapDyn <- EC.simpleCollectionValueEditor display (const editValue) (R.constDyn testMap)
  newLine >> (RD.dynText $ fmap (T.pack . show) editMapDyn)
  editMapStructureDyn <- newLine >> EC.simpleCollectionEditor display (const editValue) inputPair editMapDyn
  newLine >> (RD.dynText $ fmap (T.pack . show) editMapStructureDyn)

  RD.el "p" $ RD.text "v2 @ Map"
  editMapDyn2 <- EC.simpleCollectionValueEditor display (const editValue) (R.constDyn testMap)
  newLine >> (RD.dynText $ fmap (T.pack . show) editMapDyn2)
  editMapStructureDyn2 <- newLine >> EC.simpleCollectionEditor2 (const editDeleteValue) inputPair editMapDyn2
  newLine >> (RD.dynText $ fmap (T.pack . show) editMapStructureDyn2)

  RD.el "p" $ RD.text "v1 @ List"
  editListDyn <- EC.simpleCollectionValueEditor display (const editValue) (R.constDyn testList)
  newLine >> (RD.dynText $ fmap (T.pack . show) editListDyn)
  editListStructureDyn <- newLine >> EC.simpleCollectionEditor display (const editValue) inputValue editListDyn
  newLine >> (RD.dynText $ fmap (T.pack . show) editListStructureDyn)

  RD.el "p" $ RD.text "v2 @ List"
  editListDyn2 <- EC.simpleCollectionValueEditor display (const editValue) (R.constDyn testList)
  newLine >> (RD.dynText $ fmap (T.pack . show) editListDyn2)
  editListStructureDyn2 <- newLine >> EC.simpleCollectionEditor2 (const editDeleteValue) inputValue editListDyn2
  newLine >> (RD.dynText $ fmap (T.pack . show) editListStructureDyn2)

{-

--  editListDyn <- newLine >> EC.simpleCollectionValueEditor displayEach (const editValue) (R.constDyn testList)
--  newLine >> (RD.dynText $ fmap (T.pack . show) editListDyn)
--  editListStructureDyn <- newLine >> EC.simpleCollectionEditor display (const editValue) inputValue (R.constDyn testList)
  editListStructureDyn <- newLine >> EC.simpleCollectionEditor2  (const editDeleteValue) inputValue (R.constDyn testList)
  newLine >> (RD.dynText $ fmap (T.pack . show) editListStructureDyn)
  let editListAsValue cDyn = fmap Just <$> EC.simpleCollectionEditor display (const editValue) inputValue cDyn
      inputList = editListAsValue (R.constDyn [])
      editMapAsValue cDyn = fmap Just <$> EC.simpleCollectionEditor display (const editValue) inputPair cDyn
      inputMap = inputKeyValue $ editMapAsValue (R.constDyn (mempty :: M.Map T.Text Int))
  editListOfListsDyn <- newLine >> EC.simpleCollectionEditor display (const editListAsValue) inputList (R.constDyn testListOfLists)
  newLine >> (RD.dynText $ fmap (T.pack . show) editListOfListsDyn)
  editMapOfMapsDyn <- newLine >> EC.simpleCollectionEditor display (const editMapAsValue) inputMap (R.constDyn testMapOfMaps)
  newLine >> (RD.dynText $ fmap (T.pack . show) editMapOfMapsDyn)
-}

editableCollectionTab :: (R.Reflex t, RD.DomBuilder t m, MonadWidgetExtraC t m, RD.MonadHold t m, RD.PostBuild t m, MonadFix m) => TabInfo t m ()
editableCollectionTab = TabInfo "Editable Collections" (R.constDyn ("Editable Collections", M.empty)) $ editableCollectionsWidget


test :: (RD.DomBuilder t m, MonadWidgetExtraC t m, RD.MonadHold t m, RD.PostBuild t m, MonadFix m) => m ()
test = do
  RD.el "p" (RD.text "")
  RD.el "br" RD.blank
  staticTabbedLayout RD.def editableCollectionTab
    [
      editableCollectionTab
    ]
  return ()

linkedCss::CssLinks
linkedCss = CssLinks []

--includedCss = RD.def --bootstrapSFIncludedCss
toLink = linkedCss {- <> (cssToLink includedCss) -}
toEmbed = {- flexCssBS <> -} tabCssBS {- <> (cssToEmbed includedCss) -}

widgetMain  :: JSM ()
widgetMain  =
  RD.mainWidgetWithHead (headElt "widgets demo" toLink toEmbed) $ test

{-
#ifdef USE_WKWEBVIEW
main::IO ()
main = run editorMain
#endif
-}

--this needs fixing if I want support webkit or whatever else as well.
#ifndef ghcjs_HOST_OS
main::IO ()
main = do
  let port :: Int = 3702
  _ <- SP.spawnProcess "open" ["-a","/Applications/Safari.App/Contents/MacOs/Safari", "http://localhost:" ++ show port]
--  _ <- SP.spawnProcess "open" ["http://localhost:" ++ show port]
  run port widgetMain
#endif


#ifdef ghcjs_HOST_OS
main :: IO ()
main = widgetMain
#endif

