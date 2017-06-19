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

import           Control.Lens                                        (Traversal,
                                                                      makeLenses,
                                                                      view,
                                                                      (^.))
import           Control.Monad.Fix                                   (MonadFix)
import           Data.Functor.Compose                                (Compose (Compose),
                                                                      getCompose)
import qualified Data.Map                                            as M
import           Data.Monoid                                         ((<>))
import           Data.Profunctor                                     (lmap,
                                                                      rmap)
import           Data.Profunctor.Traversing                          (wander)
import qualified Data.Text                                           as T
import qualified GHC.Generics                                        as GHC
import           Prelude                                             hiding
                                                                      (div, rem,
                                                                      span)
import qualified System.Process                                      as SP



-- some editor examples.  Using applicative and categorical composition as well as using VL lenses to transform.

data Prod = Prod { _f1 :: Int, _f2 :: Int, _f3 :: T.Text } deriving (Show, GHC.Generic)
instance B.Generic Prod
instance B.HasDatatypeInfo Prod
instance FormInstanceC t m => FormBuilder t m Prod

makeLenses ''Prod

data Sum = A Int | B T.Text | C Char deriving (Show, GHC.Generic)
instance B.Generic Sum

-- using the generic instance
editProd1 :: FormInstanceC t m => DynEditor t m Prod Prod
editProd1 = editField Nothing


-- using applicative composition
editProd2 :: FormInstanceC t m => DynEditor t m Prod Prod
editProd2 = Prod
            <$> lmap (view f1) (editField Nothing)
            <*> lmap (view f2) (editField Nothing)
            <*> lmap (view f3) (editField Nothing)

-- using categorical composition

editFieldFR :: (FormInstanceC t m, VFormBuilderC t m a) => Maybe FieldName -> FormEditor t m a a
editFieldFR mFN = toFormEditor $ editField mFN

-- NB: we could change the order here and it would all still work.
-- The widgets do not need to be in the order of the structure.  This is different from the applicative case.
-- The point in (|>|) or (|<|) indicates the flow of data through the widgets
editProd3 :: FormInstanceC t m => FormEditor t m Prod Prod
editProd3 = (wander f1 $ editFieldFR Nothing) |>| (wander f2 $ editFieldFR Nothing) |>| (wander f3 $ editFieldFR Nothing)

simpleEditorW :: FormInstanceC t m => FormConfiguration t m -> m ()
simpleEditorW cfg = flexCol $ do
  let dmp = constDynMaybe $ Just $ Prod 1 2 "Prod"
  frp1 <- flexItem $ runForm cfg $ runEditor editProd1 dmp
  frp2 <- flexItem $ runForm cfg $ runEditor editProd2 (formResultToDynMaybe frp1)
  frp3 <- flexItem $ runForm cfg $ runEditor editProd3 frp2
  flexItem $ dynText $ T.pack . show <$> (getCompose $ formResultToDynMaybe $ frp3)
  return ()

-- of course, since we are sending the result of one into the other we could just use categorical composition here as well
categoricalEditorW :: FormInstanceC t m => FormConfiguration t m -> m ()
categoricalEditorW cfg = flexCol $ do
  let frpIn = dynMaybeToFormResult $ constDynMaybe $ Just $ Prod 1 2 "Prod"
      ed = liftE flexCol $
           liftE (flexItem' (oneClass "sf-outline-black")) (toFormEditor editProd1)
           |>| liftE (flexItem' (oneClass "sf-outline-black")) (toFormEditor editProd2)
           |>| liftE (flexItem' (oneClass "sf-outline-black")) editProd3
  frp <- runForm cfg $ runEditor ed frpIn
  flexItem $ dynText $ T.pack . show <$> (getCompose $ formResultToDynMaybe $ frp)

simpleEditorTab :: FormInstanceC t m => FormConfiguration t m -> TabInfo t m ()
simpleEditorTab cfg = TabInfo "Simple Editor" (constDyn ("Simple Editor Example", M.empty)) $ simpleEditorW cfg

categoricalEditorTab :: FormInstanceC t m => FormConfiguration t m -> TabInfo t m ()
categoricalEditorTab cfg = TabInfo "Categorical Editor" (constDyn ("Categorical Editor Example", M.empty)) $ categoricalEditorW cfg

test :: FormInstanceC t m => FormConfiguration t m -> m ()
test cfg = do
  el "p" (text "")
  el "br" blank
  staticTabbedLayout def (simpleEditorTab cfg)
    [
      simpleEditorTab cfg
    , categoricalEditorTab cfg
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

