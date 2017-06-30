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

import           Reflex.Dom.Contrib.Layout.ClayUtils          (cssToBS)
import           Reflex.Dom.Contrib.Layout.FlexLayout         (flexCol,
                                                               flexCol',
                                                               flexCssBS,
                                                               flexFill,
                                                               flexItem,
                                                               flexItem',
                                                               flexRow,
                                                               flexRow')
import           Reflex.Dom.Contrib.Layout.TabLayout
import           Reflex.Dom.Contrib.Layout.Types              (CssClass (..),
                                                               CssClasses (..),
                                                               LayoutDirection (..),
                                                               LayoutOrientation (..),
                                                               emptyCss,
                                                               oneClass)
import           Reflex.Dom.Contrib.ReflexConstraints         (MonadWidgetExtraC)
import           Reflex.Dom.Contrib.Widgets.SafeDropdown      (SafeDropdown (..),
                                                               SafeDropdownConfig (..),
                                                               safeDropdownOfLabelKeyedValue)
import           Reflex.Dom.Contrib.Widgets.WidgetResult      (dynamicWidgetResultToWidgetResult,
                                                               widgetResultToDynamic)

#ifdef USE_WKWEBVIEW
import           Language.Javascript.JSaddle.WKWebView        (run)
#endif

#ifdef USE_WARP
import           Language.Javascript.JSaddle.Warp             (run)
#endif


import           DataBuilder                                  as B
import           Reflex.Dom.Contrib.FormBuilder
import           Reflex.Dom.Contrib.FormBuilder.Configuration
import           Reflex.Dom.Contrib.FormBuilder.FormEditor
import           Reflex.Dom.Contrib.FormBuilder.Instances     (FormInstanceC)

import           Reflex
import           Reflex.Dom.Core                              hiding (InputElementConfig)

import           GHCJS.DOM.Types                              (JSM)
import           Reflex.Dom.Contrib.CssUtils                  (CssLink,
                                                               CssLinks (..),
                                                               headElt)


--import           Css

import           Control.Arrow                                (returnA)
import           Control.Lens                                 (Lens, Lens',
                                                               Prism, Prism',
                                                               Traversal, each,
                                                               makeLenses,
                                                               makePrisms, over,
                                                               preview, review,
                                                               set, view,
                                                               withPrism, (^.),
                                                               _1, _2, _3)
import           Control.Monad.Fix                            (MonadFix)
import           Data.Bool                                    (bool)
import           Data.Functor.Compose                         (Compose (Compose),
                                                               getCompose)
import qualified Data.Map                                     as M
import           Data.Maybe                                   (isJust)
import           Data.Monoid                                  ((<>))
import           Data.Profunctor                              (Choice, dimap,
                                                               lmap, right',
                                                               rmap)
import           Data.Profunctor.Traversing                   (wander)
import qualified Data.Text                                    as T
import qualified GHC.Generics                                 as GHC
import           Prelude                                      hiding (div, rem,
                                                               span)
import qualified System.Process                               as SP

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
editProd3 = (editPart f1 $ editField Nothing) |>| (editPart f2 $ editField Nothing) |>| (editPart f3 $ editField Nothing)

-- arrows! This is acting...odd
editProd4 :: FormInstanceC t m => FormEditor t m Prod Prod
editProd4 = proc x -> do
  p1 <- (editPart f1 $ editField Nothing) -< x
  p2 <- (editPart f2 $ editField Nothing) -< p1
  p3 <- (editPart f3 $ editField Nothing) -< p2
  returnA -< p3

simpleEditorW :: FormInstanceC t m => FormConfiguration t m -> m ()
simpleEditorW cfg = flexCol $ do
  let fvp0 = constFormValue $ Prod 1 2 "Prod"
  fvp1 <- flexItem $ runForm cfg $ runEditor editProd1 fvp0
  fvp2 <- flexItem $ runForm cfg $ runEditor editProd2 fvp1
  fvp3 <- flexItem $ runForm cfg $ runEditor editProd3 fvp2
  fvp4 <- flexItem $ runForm cfg $ runEditor editProd4 fvp3
  flexItem $ dynText $ T.pack . show <$> (getCompose $ formValueToDynMaybe $ fvp4)
  return ()

boxEditor = liftE (flexItem' (oneClass "sf-outline-black"))

-- of course, since we are sending the result of one into the other we could just use categorical composition here as well
categoricalEditorW :: FormInstanceC t m => FormConfiguration t m -> m ()
categoricalEditorW cfg = flexCol $ do
  let fvpIn = constFormValue $ Prod 1 2 "Prod"
      ed = liftE flexCol $ boxEditor editProd1 |>| boxEditor editProd2 |>| boxEditor editProd3 |>| boxEditor editProd4
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
-- editable by including only some of constructors
editSum2 :: FormInstanceC t m => FormEditor t m Sum Sum
editSum2 = (editOnly _A $ editField Nothing) |>| (editOnly _B $ editField Nothing) |>| (editOnly _C $ editField Nothing)

--
-- This one allows you to choose which you want to be able to edit but can only edit if it matches the input
editSum3 :: FormInstanceC t m => FormEditor t m Sum Sum
editSum3 = chooseAmong [ BuilderChoice "If A" (const False) (editOnly _A $ editField Nothing)
                       , BuilderChoice "If B" (const False) (editOnly _B $ editField Nothing)
                       , BuilderChoice "If C" (const False) (editOnly _C $ editField Nothing)
                       ]

-- this one lets you choose which to edit, forces the output to match.
-- If the input is the same constructor, then this will be set by the input.
editSum4 :: FormInstanceC t m => FormEditor t m Sum Sum
editSum4 = chooseAmong [ BuilderChoice "Force A" (const False) (editAndBeForm _A $ editField Nothing)
                       , BuilderChoice "Force B" (const False) (editAndBeForm _B $ editField Nothing)
                       , BuilderChoice "Force C" (const False) (editAndBeForm _C $ editField Nothing)
                       ]

-- this one lets you choose which to edit, forces the output to match.
-- Switches on input
editSum5 :: FormInstanceC t m => FormEditor t m Sum Sum
editSum5 = chooseAmong [ BuilderChoice "A" (isJust . preview _A) (editAndBeForm _A $ editField Nothing)
                       , BuilderChoice "B" (isJust . preview _B) (editAndBeForm _B $ editField Nothing)
                       , BuilderChoice "C" (isJust . preview _C) (editAndBeForm _C $ editField Nothing)
                       ]

sumEditW :: FormInstanceC t m => FormConfiguration t m -> m ()
sumEditW cfg = do
  let fvp0 = constFormValue $ A 12
      ed = boxEditor editSum1 |>| boxEditor editSum2 |>| boxEditor editSum3 |>| boxEditor editSum4  |>| boxEditor editSum5
  fvpOut <- flexItem $ runForm cfg $ runEditor ed fvp0
  flexItem $ dynText $ T.pack . show <$> (getCompose $ formValueToDynMaybe $ fvpOut)

sumEditTab :: FormInstanceC t m => FormConfiguration t m -> TabInfo t m ()
sumEditTab cfg = TabInfo "Simple Sum" (constDyn ("Simple Sum Example", M.empty)) $ sumEditW cfg

data SumTuple = ATuple Int Int | BTuple T.Text Color Double | CTuple (Int, Int) deriving (GHC.Generic, Show)
instance Generic SumTuple
instance HasDatatypeInfo SumTuple
instance FormInstanceC t m => FormBuilder t m SumTuple


makePrisms ''SumTuple

editSumTupleGeneric :: FormInstanceC t m => FormEditor t m SumTuple SumTuple
editSumTupleGeneric = editField Nothing

editSumTuple1 :: forall t m. FormInstanceC t m => FormEditor t m SumTuple SumTuple
editSumTuple1 =
  let aTupleEd = (editPart _1 $ editField Nothing) |>| (editPart _2 $ editField Nothing)
      bTupleEd = (editPart _1 $ editField Nothing) |>| (editPart _2 $ editField Nothing) |>| (editPart _3 $ editField Nothing)
      cTupleEd = editField Nothing --(editPart _1 $ editField Nothing) |>| (editPart _2 $ editField Nothing)
  in chooseAmong [ BuilderChoice "ATuple" (isJust . preview _ATuple) (editAndBeForm _ATuple aTupleEd)
                 , BuilderChoice "BTuple" (isJust . preview _BTuple) (editAndBeForm _BTuple bTupleEd)
                 , BuilderChoice "CTuple" (isJust . preview _CTuple) (editAndBeForm _CTuple cTupleEd)
                 ]

sumTupleEditW :: FormInstanceC t m => FormConfiguration t m -> m ()
sumTupleEditW cfg = do
  let fvp0 = constFormValue $ ATuple 2 2
      ed = boxEditor editSumTupleGeneric |>| boxEditor editSumTuple1
  fvpOut <- flexItem $ runForm cfg $ runEditor ed fvp0
  flexItem $ dynText $ T.pack . show <$> (getCompose $ formValueToDynMaybe $ fvpOut)


sumTupleEditTab :: FormInstanceC t m => FormConfiguration t m -> TabInfo t m ()
sumTupleEditTab cfg = TabInfo "Tuple Sum" (constDyn ("Tuple Sum Example", M.empty)) $ sumTupleEditW cfg


-- traversals

listOfS :: [Sum]
listOfS = [A 12, A 22, B "abc", B "def", C Green, C Blue]

editTraversable :: (FormInstanceC t m, Traversable q) => FormEditor t m a b -> FormEditor t m (q a) (q b)
editTraversable editOne = wander traverse editOne

listEditW :: FormInstanceC t m => FormConfiguration t m -> m ()
listEditW cfg = do
  let fvlIn = constFormValue listOfS
      ed = editTraversable editSum5
  fvlOut <- runForm cfg $ runEditor ed fvlIn
  flexItem $ dynText $ T.pack . show <$> (getCompose $ formValueToDynMaybe $ fvlOut)

listEditTab :: FormInstanceC t m => FormConfiguration t m -> TabInfo t m ()
listEditTab cfg = TabInfo "List Of Sum" (constDyn ("List Of Sum Example", M.empty)) $ listEditW cfg


test :: FormInstanceC t m => FormConfiguration t m -> m ()
test cfg = do
  el "p" (text "")
  el "br" blank
  staticTabbedLayout def (simpleProdEditorTab cfg)
    [
      simpleProdEditorTab cfg
    , categoricalEditorTab cfg
    , sumEditTab cfg
    , sumTupleEditTab cfg
    , listEditTab cfg
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

