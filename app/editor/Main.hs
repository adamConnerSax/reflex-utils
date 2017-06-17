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

import           Reflex.Dom.Contrib.Layout.ClayUtils                 (cssToBS)
import           Reflex.Dom.Contrib.Layout.FlexLayout                (flexCol,
                                                                      flexCol',
                                                                      flexCssBS,
                                                                      flexFill,
                                                                      flexItem,
                                                                      flexItem',
                                                                      flexRow,
                                                                      flexRow')
import           Reflex.Dom.Contrib.Layout.TabLayout
import           Reflex.Dom.Contrib.Layout.Types                     (CssClass (..),
                                                                      CssClasses (..),
                                                                      LayoutDirection (..),
                                                                      LayoutOrientation (..),
                                                                      emptyCss,
                                                                      oneClass)
import           Reflex.Dom.Contrib.ReflexConstraints                (MonadWidgetExtraC)
import           Reflex.Dom.Contrib.Widgets.WidgetResult             (widgetResultToDynamic)


#ifdef USE_WKWEBVIEW
import           Language.Javascript.JSaddle.WKWebView               (run)
#endif

#ifdef USE_WARP
import           Language.Javascript.JSaddle.Warp                    (run)
#endif


import           DataBuilder                                         as B
import           Reflex.Dom.Contrib.FormBuilder
import           Reflex.Dom.Contrib.FormBuilder.Configuration
import           Reflex.Dom.Contrib.FormBuilder.Editor
import           Reflex.Dom.Contrib.FormBuilder.Instances            (FormInstanceC)
import           Reflex.Dom.Contrib.FormBuilder.Instances.Containers (buildList, buildListWithSelect,
                                                                      buildMapWithSelect)

import           Reflex
import           Reflex.Dom.Core                                     hiding (InputElementConfig)

import           GHCJS.DOM.Types                                     (JSM)
import           Reflex.Dom.Contrib.CssUtils                         (CssLink, CssLinks (..),
                                                                      headElt)


--import           Css

import           Control.Lens                                        (view,
                                                                      (^.))
import           Control.Monad.Fix                                   (MonadFix)
import           Data.Functor.Compose                                (Compose (Compose),
                                                                      getCompose)
import qualified System.Process                                      as SP

import           Data.Monoid                                         ((<>))
import qualified Data.Text                                           as T
import qualified GHC.Generics                                        as GHC
import           Prelude                                             hiding
                                                                      (div, rem,
                                                                      span)



-- some editor examples.  Using applicative and categorical composition as well as using VL lenses to transform.

data Prod = Prod { int1 :: Int, int2 :: Int, text :: Text } deriving (GHC.Generic)
instance B.Generic Prod

data Sum = A Int | B Text | C Char deriving (GHC.Generic)
instance B.Generic Sum

editProd1 :: FormInstanceC t m => DynEditor t m Prod Prod
editProd1 = editField

editProd2 :: FormInstanceC t m => DynEditor t m Prod Prod
editProd2 = Prod <$> editField <*> editField <*> editField



simpleEditorW :: FormInstanceC t m => FormConfiguration t m -> m ()
simpleEditorW cfg = flexCol $ do
  let dmp = constDynMaybe $ Prod 1 2 "Prod"
  frp <- flexItem $ runForm cfg $ runEditor editProd1 dmp
  _ <- flexItem $ runForm cfg $ runEditor editProd2 (formResultToDynMaybe frp)
  return ()


simpleEditorTab :: FormInstanceC t m => FormConfiguration t m -> TabInfo t m ()
simpleEditorTab cfg = TabInfo "Simple Editor" (constDyn ("Simple Editor Example", M.empty)) $ simpleEditorW cfg

test :: FormInstanceC t m => FormConfiguration t m -> m ()
test cfg = do
  el "p" (text "")
  el "br" blank
  staticTabbedLayout def (simpleEditorTab cfg)
    [
      simpleEditorTab cfg
    ]
  return ()

linkedCss::CssLinks
linkedCss = CssLinks []

customizeConfig::FormConfiguration t m -> FormConfiguration t m
customizeConfig = id

includedCss = def --bootstrapSFIncludedCss
toLink = linkedCss <> (cssToLink includedCss)
toEmbed = flexCssBS <> tabCssBS <> (cssToEmbed includedCss)

editorMain  :: JSM ()
editorMain  =
  mainWidgetWithHead (headElt "editor demo" toLink toEmbed) $ test (customizeConfig def)


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

