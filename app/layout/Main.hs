{-# LANGUAGE CPP                   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecursiveDo       #-}
--{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import Reflex.Dom.Contrib.Layout.All
import Reflex.Dom.Contrib.Layout.LayoutM (SupportsLayoutM,runLayoutM)
import Reflex.Dom.Contrib.Layout.FlexLayout (flexSizedItem,flexRow,flexCol,flexItem,flexCssBS)
import qualified Reflex.Dom.Contrib.Layout.OptimizedFlexLayout as OF
import Reflex.Dom.Contrib.Layout.OptimizedFlexLayout ((##),(#$))

import Reflex
import Reflex.Dom.Core
import Reflex.PerformEvent.Base
import Reflex.Host.Class (MonadReflexCreateTrigger)
import qualified Reflex.Dom.Contrib.Widgets.Common as RDC

import GHCJS.DOM.Types (MonadJSM,JSM,liftJSM)

import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Exception (MonadAsyncException)
import Control.Monad.Trans (lift)
import Control.Monad.Ref (Ref,MonadRef)
import           Control.Monad.Fix               (MonadFix)
import Data.Monoid ((<>))
import Data.FileEmbed
import Control.Lens ((%~))

import Prelude hiding (rem,div,span)
import Clay hiding (button,col,row,element)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T
import Data.Proxy (Proxy(Proxy))

#ifdef USE_WKWEBVIEW
import Language.Javascript.JSaddle.WKWebView (run)
#endif

#ifdef USE_WARP
import Language.Javascript.JSaddle.Warp (run)
#endif

{-
-- This is what the GridConfig looks like for a particular Css Grid Framework

pureRow::CssClass
pureRow = "pure-g"

pureCols::[CssClass]
pureCols = fmap (\x->"pure-u-" ++ show x ++ "-24") [1..24]

pureGridConfig = LayoutConfig (CssGridConfig pureRow pureCols) [] []
-}

boxMargin m = sym margin (rem m)
cssBox m c = do
  boxMargin m
  border solid (px 2) c

cssBoxes = do
  ".demo-box" ? boxMargin 0.1
  ".demo-box-none" ? boxMargin 0.1 
  ".demo-box-black" ? cssBox 0.1 black
  ".demo-box-red" ? cssBox 0.1 red
  ".demo-box-green" ? cssBox 0.1 green
  ".demo-box-none-large" ? boxMargin 0.3 
  ".demo-box-black-large" ? cssBox 0.3 black
  ".demo-box-red-large" ? cssBox 0.3 red
  ".demo-box-green-large" ? cssBox 0.3 green


clayCssBS::B.ByteString
clayCssBS = B.concat . BL.toChunks . encodeUtf8  $ renderWith pretty [] $ cssBoxes

boxClassCss::M.Map T.Text T.Text
boxClassCss = ("class" =: "demo-box")

demoDiv::DomBuilder t m=>T.Text -> m ()
demoDiv x = elAttr "div" boxClassCss $ text x

innerBoxUpdaters = (\x->UpdateDynamic $ CssClasses [CssClass x] ) <$> ["demo-box-none",
                                                                       "demo-box-black",
                                                                       "demo-box-red",
                                                                       "demo-box-green",
                                                                       "demo-box-none-large",
                                                                       "demo-box-black-large",
                                                                       "demo-box-red-large",
                                                                       "demo-box-green-large"]

updaterLabel::CssUpdate->T.Text
updaterLabel (UpdateDynamic x) = T.pack $ L.drop (L.length ("demo-box-"::String) ) $ T.unpack (toCssString x)
updaterLabel (AddToDynamic x) = undefined

innerBoxDfltUpdater = innerBoxUpdaters !! 1
innerBoxClassDDEvent::(DomBuilder t m,DomBuilderSpace m ~ GhcjsDomSpace,
                       PostBuild t m,HasWebView m,{- MonadAsyncException m, -}
                       Ref m ~ Ref IO, Ref (Performable m) ~ Ref IO,
                       MonadFix m, MonadRef m, MonadRef (Performable m),
                       TriggerEvent t m, PerformEvent t m,
                       MonadReflexCreateTrigger t m, MonadHold t m,
                       MonadSample t (Performable m),
                       HasWebView (Performable m),
                       MonadAsyncException (Performable m),MonadJSM m,
                       MonadJSM (Performable m))=>Event t CssUpdate -> m (Event t CssUpdate)
innerBoxClassDDEvent setEv = RDC._widget0_change <$> RDC.htmlDropdownStatic innerBoxUpdaters updaterLabel
                             Prelude.id (RDC.WidgetConfig setEv innerBoxDfltUpdater (constDyn M.empty))

innerBoxInitialCss = CssClasses [CssClass "demo-box-black"]

row::(PostBuild t m ,SupportsLayoutM t m)=>LayoutM t m a->LayoutM t m a
row = lmFlexLayoutRow

col::(PostBuild t m ,SupportsLayoutM t m)=>Int->LayoutM t m a->LayoutM t m a
col = lmFlexCol

subWidgetSimple::(PostBuild t m,SupportsLayoutM t m,MonadIO (PushM t))=>LayoutM t m ()
subWidgetSimple = do
  row $ do
    col 1 $ demoDiv "C"
    col 1 $ demoDiv "C'"
  subWidget1


subWidget1::(PostBuild t m,SupportsLayoutM t m,MonadIO (PushM t))=>LayoutM t m ()
subWidget1 = do
  row $ demoDiv "D"
  row $ demoDiv "E"
  row $ demoDiv "F"

subWidget2::(PostBuild t m,SupportsLayoutM t m,MonadIO (PushM t))=>LayoutM t m ()
subWidget2 = do
  row $ do
    col 1 $ demoDiv "D"
    col 1 $ demoDiv "E"
    col 1 $ demoDiv "F"

subWidgetToggle::(PostBuild t m, SupportsLayoutM t m,MonadIO (PushM t))=>LayoutM t m ()
subWidgetToggle = do
  switchToEv <- row $ do
    col 1 $ demoDiv "C"
    switchEv <- col 1 $ button "Toggle"
    is1Dyn <- toggle True switchEv
    updated <$> mapDyn (\d -> if d then subWidget1 else subWidget2) is1Dyn
  widgetHold subWidget1 switchToEv
  return ()

testControl::(SupportsLayoutM t m, PostBuild t m, {- MonadAsyncException m, -}
              HasWebView m, MonadIO (PushM t),MonadJSM (Performable m),
              MonadJSM (LayoutM t m))=>Event t CssUpdate->LayoutM t m (Event t CssUpdate)
testControl setEv  = mdo
  (evSelf,evChildren) <- row $ do
    addKeyedCssUpdateEventBelow "row" innerBoxInitialCss evSelf 
    (evSelf,evChildren) <- col 2 $ do
      (evSelf,evChildren)<-row $ do
        evSelf <- col 2 $ row $ innerBoxClassDDEvent setEv
        evChildren <- col 2 $ row $ innerBoxClassDDEvent setEv
        return (evSelf,evChildren)
      row $ demoDiv "A"
      row $ demoDiv "B"
      return (evSelf,evChildren)
    col 3 $ subWidgetToggle
    return (evSelf,evChildren)
  return evChildren

simpleFlexWidget::(SupportsLayoutM t m,MonadIO (PushM t))=>m ()
simpleFlexWidget = do
  let w = flexRow $ do
        flexSizedItem 2 $ divClass "demo-box-black" $ text  "A"
        flexSizedItem 1 $ divClass "demo-box-green" $ text  "B"
  w
  flexFillR w
  flexHCenter w
  flexFillL w

sfTab::(SupportsLayoutM t m)=>TabInfo t m ()
sfTab = TabInfo "sf" "Simple Flex" simpleFlexWidget


optFlexWidget::(SupportsLayoutM t m, PostBuild t m,HasWebView m,{- MonadAsyncException m, -}MonadIO (PushM t),
                MonadJSM m, MonadJSM (Performable m))=>m ()
optFlexWidget = do
  let w = OF.flexRow #$ do
        OF.flexSizedItem 2 #$ (divClass "demo-box-black" $ text  "A")
        OF.flexSizedItem 1 #$ (divClass "demo-box-green" $ text  "B")
  OF.flexItem #$ w
  OF.flexItem #$ OF.flexFillR w
  OF.flexItem #$ OF.flexHCenter w
  OF.flexItem #$ OF.flexFillL w

{-
optFlexWidget::(MonadWidget t m, MonadIO (PushM t))=>m ()
optFlexWidget = do
  let dc::MonadWidget t m=>String->String->m ()
      dc cs s =  (divClass (cs ++ "_1") $ text s) >> (divClass (cs ++ "_2") $ text s)
      c::MonadWidget t m=>String->m ()
      c s = optFlexCol #$ dc "col" s
      r::MonadWidget t m=>String->m ()
      r s = optFlexRow #$ dc "row" s
  optFlexRow #$ do
    c "Col 1"
    c "Col 2"
    c "Col 3"
    optFlexItem ## optFlexCol #$ do
      r "Row 1"
      r "Row 2"
      r "Row 3"
-}

optFlexTab::(SupportsLayoutM t m{-, MonadAsyncException m -})=>TabInfo t m ()
optFlexTab = TabInfo "optFlex" "optFlex" optFlexWidget


boxesWidget::(SupportsLayoutM t m,PostBuild t m,HasWebView m, {- MonadAsyncException m, -}
              MonadIO (PushM t),MonadJSM (Performable m),MonadJSM (LayoutM t m))=>LayoutM t m ()
boxesWidget = do
  ev1 <- testControl never
  setter <- addKeyedCssUpdateEventBelow' "row" innerBoxInitialCss ev1
  row $ do
    ev2 <- col 1 $ testControl setter
    setter' <- addKeyedCssUpdateEventBelow' "row" innerBoxInitialCss ev2
    col 1 $ do
      ev3 <-  testControl setter'
      setter'' <- addKeyedCssUpdateEventBelow' "row" innerBoxInitialCss ev3
      ev4 <- testControl setter''
      return ()
    return ()

boxesTab::(SupportsLayoutM t m, {- MonadAsyncException m,-} MonadJSM (LayoutM t m))=>TabInfo t m ()
boxesTab = TabInfo "boxes" "Dynamic/Events" $ runLayoutMain (LayoutConfig emptyClassMap emptyDynamicCssMap) boxesWidget
{-
laidOut::(MonadWidget t m, MonadIO (PushM t))=>LayoutM t m () -> IO ()
laidOut w = mainWidgetWithCss allCss $
            runLayoutMain (LayoutConfig pure24GridConfig emptyClassMap emptyDynamicCssMap) $ w 
-}

tabbedWidget::(SupportsLayoutM t m, PostBuild t m, HasWebView m,
               {- MonadAsyncException m,-} MonadIO (PushM t),
               MonadJSM m, MonadJSM (Performable m),MonadJSM (LayoutM t m))=>m ([()])
tabbedWidget = do
  el "p" $ text ""
  el "br" $ blank
--  dynamicTabbedLayout sfTab (constDyn [sfTab,optFlexTab,boxesTab])
  staticTabbedLayout sfTab [sfTab,optFlexTab,boxesTab]

allCss = tabCssBS
         <> flexCssBS
         <> clayCssBS 
--         <> $(embedFile "/Users/adam/Development/webResources/css/pure-release-0.6.0/pure.css")
--         <> $(embedFile "/Users/adam/Development/webResources/css/flexboxgrid-6.3.0/flexboxgrid.css")


layoutMain::JSM ()
layoutMain = do
  liftIO $ B.putStr allCss 
  mainWidgetWithCss allCss $ do
      tabbedWidget
      return ()

#ifdef USE_WKWEBVIEW
main::IO ()
main = run layoutMain
#endif

#ifdef USE_WARP
main::IO ()
main = run 3709 layoutMain
#endif

#ifdef USE_GHCJS
main :: IO ()
main = layoutMain
#endif
