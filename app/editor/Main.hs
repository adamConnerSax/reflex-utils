{-# LANGUAGE Arrows                    #-}
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
import           Reflex.Dom.Contrib.Widgets.SafeDropdown             (SafeDropdown (..),
                                                                      SafeDropdownConfig (..),
                                                                      safeDropdownOfLabelKeyedValue)
import           Reflex.Dom.Contrib.Widgets.WidgetResult             (dynamicWidgetResultToWidgetResult,
                                                                      widgetResultToDynamic)

#ifdef USE_WKWEBVIEW
import           Language.Javascript.JSaddle.WKWebView               (run)
#endif

#ifdef USE_WARP
import           Language.Javascript.JSaddle.Warp                    (run)
#endif


import           DataBuilder                                         as B
import           Reflex.Dom.Contrib.FormBuilder
import           Reflex.Dom.Contrib.FormBuilder.Configuration
import           Reflex.Dom.Contrib.FormBuilder.FormEditor
import           Reflex.Dom.Contrib.FormBuilder.Instances            (FormInstanceC)
import           Reflex.Dom.Contrib.FormBuilder.Instances.Containers (buildList, buildListWithSelect,
                                                                      buildMapWithSelect)

import           Reflex
import           Reflex.Dom.Core                                     hiding (InputElementConfig)

import           GHCJS.DOM.Types                                     (JSM)
import           Reflex.Dom.Contrib.CssUtils                         (CssLink, CssLinks (..),
                                                                      headElt)


--import           Css

import           Control.Lens                                        (Prism,
                                                                      Traversal,
                                                                      makeLenses,
                                                                      makePrisms,
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

-- using the generic instance
editProd1 :: FormInstanceC t m => FormEditor t m Prod Prod
editProd1 = editField Nothing

-- using applicative composition
editProd2 :: FormInstanceC t m => FormEditor t m Prod Prod
editProd2 = Prod
            <$> lmap (view f1) (editField Nothing)
            <*> lmap (view f2) (editField Nothing)
            <*> lmap (view f3) (editField Nothing)

-- using categorical composition

-- NB: we could change the order here and it would all still work.
-- The widgets do not need to be in the order of the structure.  This is different from the applicative case.
-- The point in (|>|) or (|<|) indicates the directional flow of data through the widgets
-- We need a synonym for wander.  "focusEditor"? "zoomEditor"?
editProd3 :: FormInstanceC t m => FormEditor t m Prod Prod
editProd3 = (wander f1 $ editField Nothing) |>| (wander f2 $ editField Nothing) |>| (wander f3 $ editField Nothing)

-- arrows!
--editProd4 :: FormInstanceC t m => FormEditor t m Prod Prod
--editProd4 = proc <- do

simpleEditorW :: FormInstanceC t m => FormConfiguration t m -> m ()
simpleEditorW cfg = flexCol $ do
  let dmp = constDynMaybe $ Just $ Prod 1 2 "Prod"
  fvp1 <- flexItem $ runForm cfg $ runEditor editProd1 $ dynMaybeToFormValue dmp
  fvp2 <- flexItem $ runForm cfg $ runEditor editProd2 fvp1
  fvp3 <- flexItem $ runForm cfg $ runEditor editProd3 fvp2
  flexItem $ dynText $ T.pack . show <$> (getCompose $ formValueToDynMaybe $ fvp3)
  return ()

-- of course, since we are sending the result of one into the other we could just use categorical composition here as well
categoricalEditorW :: FormInstanceC t m => FormConfiguration t m -> m ()
categoricalEditorW cfg = flexCol $ do
  let fvpIn = dynMaybeToFormValue $ constDynMaybe $ Just $ Prod 1 2 "Prod"
      ed = liftE flexCol $
           liftE (flexItem' (oneClass "sf-outline-black")) editProd1
           |>| liftE (flexItem' (oneClass "sf-outline-black")) editProd2
           |>| liftE (flexItem' (oneClass "sf-outline-black")) editProd3
  fvpOut <- runForm cfg $ runEditor ed fvpIn
  flexItem $ dynText $ T.pack . show <$> (getCompose $ formValueToDynMaybe $ fvpOut)

simpleProdEditorTab :: FormInstanceC t m => FormConfiguration t m -> TabInfo t m ()
simpleProdEditorTab cfg = TabInfo "Simple Editor" (constDyn ("Simple Editor Example", M.empty)) $ simpleEditorW cfg

categoricalEditorTab :: FormInstanceC t m => FormConfiguration t m -> TabInfo t m ()
categoricalEditorTab cfg = TabInfo "Categorical Editor" (constDyn ("Categorical Editor Example", M.empty)) $ categoricalEditorW cfg

data Color = Red | Black | Blue | Green deriving (Enum, Bounded, Show, GHC.Generic)
instance B.Generic Color
instance B.HasDatatypeInfo Color
instance FormInstanceC t m => FormBuilder t m Color

data Sum = A Int | B T.Text | C Color deriving (Show, GHC.Generic)
instance B.Generic Sum
instance B.HasDatatypeInfo Sum
instance FormInstanceC t m => FormBuilder t m Sum

makePrisms ''Sum

-- simple generic dropdown of sum constructors.  The default.
editSum1 :: FormInstanceC t m => FormEditor t m Sum Sum
editSum1 = editField Nothing

-- this one allows editing of the incoming sum but cannot change which constructor is in play.  But you could also select which are
-- editable, etc.
editSum2 :: FormInstanceC t m => FormEditor t m Sum Sum
editSum2 = (wander _A $ editField Nothing) |>| (wander _B $ editField Nothing) |>| (wander _C $ editField Nothing)

data ConstructorChoice t m s = ConstructorChoice { conName :: T.Text, conEd :: FormEditor t m s s }

chooseAmong :: FormInstanceC t m => [ConstructorChoice t m s] -> FormEditor t m s s
chooseAmong choices =
  let chooserMap = M.fromList $ zip [0..] choices
      runMaybeEditorOn x =  maybe (Compose $ return formValueNothing) (flip runEditor x)
  in Editor $ \ga -> makeForm $ flexRow $ do
    choice <- _safeDropdown_value <$> (flexItem $ safeDropdownOfLabelKeyedValue (\_ cc -> conName cc) Nothing (constDyn chooserMap) def)
    x <- dyn (getCompose . runMaybeEditorOn ga . fmap conEd <$> choice) -- Event t (FormResult t s)
    y <- holdDyn formValueNothing x
    return $ Compose $ dynamicWidgetResultToWidgetResult $ getCompose <$> y


editSum3 :: FormInstanceC t m => FormEditor t m Sum Sum
editSum3 = chooseAmong [ ConstructorChoice "A" (wander _A $ editField Nothing)
                       , ConstructorChoice "B" (wander _B $ editField Nothing)
                       , ConstructorChoice "C" (wander _C $ editField Nothing)
                        ]

sumEditW :: FormInstanceC t m => FormConfiguration t m -> m ()
sumEditW cfg = do
  let fvpIn = dynMaybeToFormValue $ constDynMaybe $ Just $ A 12
      ed = liftE flexCol $
           liftE (flexItem' (oneClass "sf-outline-black")) editSum1
           |>| liftE (flexItem' (oneClass "sf-outline-black")) editSum2
           |>| liftE (flexItem' (oneClass "sf-outline-black")) editSum3
  fvpOut <- runForm cfg $ runEditor ed fvpIn
  flexItem $ dynText $ T.pack . show <$> (getCompose $ formValueToDynMaybe $ fvpOut)

sumEditTab :: FormInstanceC t m => FormConfiguration t m -> TabInfo t m ()
sumEditTab cfg = TabInfo "Simple Sum" (constDyn ("Simple Sum Example", M.empty)) $ sumEditW cfg

test :: FormInstanceC t m => FormConfiguration t m -> m ()
test cfg = do
  el "p" (text "")
  el "br" blank
  staticTabbedLayout def (simpleProdEditorTab cfg)
    [
      simpleProdEditorTab cfg
    , categoricalEditorTab cfg
    , sumEditTab cfg
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

