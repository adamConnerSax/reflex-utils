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

editValue :: (RD.DomBuilder t m, MonadWidgetExtraC t m, RD.PostBuild t m, Show v, Read v) => R.Dynamic t v -> m (R.Dynamic t (Maybe v))
editValue valDyn = do
  postBuild <- RD.getPostBuild
  let inputEv = T.pack . show <$> R.leftmost [R.tag (R.current valDyn) postBuild, R.updated valDyn]
      config = RD.TextInputConfig "text" "" inputEv (R.constDyn M.empty)
  fmap (readMaybe . T.unpack) . RD._textInput_value <$> RD.textInput config

inputPair :: (RD.DomBuilder t m, MonadWidgetExtraC t m, RD.PostBuild t m, Read k, Read v) => m (R.Dynamic t (Maybe (k,v)))
inputPair = do
  let config = RD.TextInputConfig "text" "" R.never (R.constDyn M.empty)
  RD.el "div" $ do
    kMDyn <- fmap (readMaybe . T.unpack) . RD._textInput_value <$> RD.textInput config
    vMDyn <- fmap (readMaybe . T.unpack) . RD._textInput_value <$> RD.textInput config
    let pairDynMaybe = R.zipDyn kMDyn vMDyn
    return $ fmap (\(ma,mb) -> (,) <$> ma <*> mb) pairDynMaybe

newLine = RD.el "p" (RD.text "") >> RD.el "br" RD.blank

editableCollectionsWidget :: (RD.DomBuilder t m, MonadWidgetExtraC t m, RD.MonadHold t m, RD.PostBuild t m, MonadFix m) => m ()
editableCollectionsWidget = do
  let testMap :: M.Map T.Text Int = M.fromList [("A",1),("B",2),("C",3)]
      testList :: [T.Text] = ["Hello","Goodbye","Cat"]
  editMapDyn <- EC.simpleCollectionValueEditor EC.DisplayAll (const editValue) (R.constDyn testMap)
  newLine >> (RD.dynText $ fmap (T.pack . show) editMapDyn)
  editMapStructureDyn <- newLine >> EC.simpleCollectionEditor EC.DisplayAll (const editValue) inputPair editMapDyn
  newLine >> (RD.dynText $ fmap (T.pack . show) editMapStructureDyn)
  editMapStructureDyn' <- newLine >> EC.simpleCollectionEditor (EC.DisplayEach (R.constDyn M.empty) (T.pack . show)) (const editValue) inputPair editMapDyn
  newLine >> (RD.dynText $ fmap (T.pack . show) editMapStructureDyn')
  editListDyn <- newLine >> EC.simpleCollectionValueEditor EC.DisplayAll (const editValue) (R.constDyn testList)
  newLine >> (RD.dynText $ fmap (T.pack . show) editListDyn)
  editListStructureDyn <- newLine >> EC.simpleCollectionEditor EC.DisplayAll (const editValue) inputValue editListDyn
  newLine >> (RD.dynText $ fmap (T.pack . show) editListStructureDyn)


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

editorMain  :: JSM ()
editorMain  =
  RD.mainWidgetWithHead (headElt "widgets demo" toLink toEmbed) $ test


#ifdef USE_WKWEBVIEW
main::IO ()
main = run editorMain
#endif

#ifdef USE_WARP
main::IO ()
main = do
  let port :: Int = 3702
  _ <- SP.spawnProcess "open" ["http://localhost:" ++ show port]
  run port editorMain
#endif

#ifdef USE_GHCJS
main :: IO ()
main = editorMain
#endif

