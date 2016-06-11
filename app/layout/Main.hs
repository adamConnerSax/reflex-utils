{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Reflex.Dom.Contrib.Layout.All
import Reflex.Dom.Contrib.Layout.LayoutM()
import Reflex.Dom.Contrib.Layout.GridConfigs
import Reflex.Dom.Contrib.Layout.FlexLayout (flexSizedItem,flexRow,flexCol,flexItem,flexCssBS)

import Reflex
import Reflex.Dom
import qualified Reflex.Dom.Contrib.Widgets.Common as RDC

--import Reflex.Dom.Contrib.Layout.LayoutP (doOptimizedLayout,doUnoptimizedLayout)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Data.Monoid ((<>))
import Data.FileEmbed

import Prelude hiding (rem,div,span)
import Clay hiding (button,col)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.List as T


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

boxClassCss = ("class" =: "demo-box")

demoDiv x = elAttr "div" boxClassCss $ text x

innerBoxUpdaters = (\x->UpdateDynamic $ CssClasses [CssClass x] ) <$> ["demo-box-none",
                                                                       "demo-box-black",
                                                                       "demo-box-red",
                                                                       "demo-box-green",
                                                                       "demo-box-none-large",
                                                                       "demo-box-black-large",
                                                                       "demo-box-red-large",
                                                                       "demo-box-green-large"]

updaterLabel::CssUpdate->String
updaterLabel (UpdateDynamic x) = T.drop (T.length ("demo-box-"::String) ) $ toCssString x
updaterLabel (AddToDynamic x) = undefined

innerBoxDfltUpdater = innerBoxUpdaters !! 1
innerBoxClassDDEvent::MonadWidget t m=>Event t CssUpdate -> m (Event t CssUpdate)
innerBoxClassDDEvent setEv = RDC._widget0_change <$> RDC.htmlDropdownStatic innerBoxUpdaters updaterLabel
                             Prelude.id (RDC.WidgetConfig setEv innerBoxDfltUpdater (constDyn M.empty))

innerBoxInitialCss = CssClasses [CssClass "demo-box-black"]

row::MonadWidget t m=>LayoutM t m a->LayoutM t m a
row = lmFlexLayoutRow

col::MonadWidget t m=>Int->LayoutM t m a->LayoutM t m a
col = lmFlexCol

subWidgetSimple::(MonadWidget t m,MonadIO (PushM t))=>LayoutM t m ()
subWidgetSimple = do
  row $ do
    col 1 $ demoDiv "C"
    col 1 $ demoDiv "C'"
  subWidget1


subWidget1::(MonadWidget t m, MonadIO (PushM t))=>LayoutM t m ()
subWidget1 = do
  row $ demoDiv "D"
  row $ demoDiv "E"
  row $ demoDiv "F"

subWidget2::(MonadWidget t m, MonadIO (PushM t))=>LayoutM t m ()
subWidget2 = do
  row $ do
    col 1 $ demoDiv "D"
    col 1 $ demoDiv "E"
    col 1 $ demoDiv "F"

subWidgetToggle::(MonadWidget t m,MonadIO (PushM t))=>LayoutM t m ()
subWidgetToggle = do
  switchToEv <- row $ do
    col 1 $ demoDiv "C"
    switchEv <- col 1 $ button "Toggle"
    is1Dyn <- toggle True switchEv
    updated <$> mapDyn (\d -> if d then subWidget1 else subWidget2) is1Dyn
  widgetHold subWidget1 switchToEv
  return ()

testControl::(MonadWidget t m, MonadIO (PushM t))=>Event t CssUpdate->LayoutM t m (Event t CssUpdate)
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

simpleFlexWidget::(MonadWidget t m, MonadIO (PushM t))=>m ()
simpleFlexWidget = do
  let w = flexRow $ do
        flexSizedItem 2 $ divClass "demo-box-black" $ text  "A"
        flexSizedItem 1 $ divClass "demo-box-green" $ text  "B"
  w
  flexFillR w
  flexHCenter w
  flexFillL w

sfTab::MonadWidget t m=>TabInfo t m ()
sfTab = TabInfo "sf" "Simple Flex" simpleFlexWidget


simpleLayoutPWidget::(MonadWidget t m, MonadIO (PushM t))=>m ()
simpleLayoutPWidget = do
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
   
sflpTab::MonadWidget t m=>TabInfo t m ()
sflpTab = TabInfo "sflp" "LayoutP" simpleLayoutPWidget


boxesWidget::(MonadWidget t m, MonadIO (PushM t))=>LayoutM t m ()
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

boxesTab::MonadWidget t m=>TabInfo t m ()
boxesTab = TabInfo "boxes" "Dynamic/Events" $ runLayoutMain (LayoutConfig pure24GridConfig emptyClassMap emptyDynamicCssMap) boxesWidget
{-
laidOut::(MonadWidget t m, MonadIO (PushM t))=>LayoutM t m () -> IO ()
laidOut w = mainWidgetWithCss allCss $
            runLayoutMain (LayoutConfig pure24GridConfig emptyClassMap emptyDynamicCssMap) $ w 
-}

tabbedWidget = do
  el "p" $ text ""
  el "br" $ blank
  dynamicTabbedLayout sfTab (constDyn [sfTab,sflpTab,boxesTab]) 

allCss = tabCssBS
         <> flexCssBS
         <> clayCssBS 
--         <> $(embedFile "/Users/adam/Development/webResources/css/pure-release-0.6.0/pure.css")
--         <> $(embedFile "/Users/adam/Development/webResources/css/flexboxgrid-6.3.0/flexboxgrid.css")


main::IO ()
main = do
  B.putStr allCss
  mainWidgetWithCss allCss $ do
      tabbedWidget
      return ()
