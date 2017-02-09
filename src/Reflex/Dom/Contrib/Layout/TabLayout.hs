{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecursiveDo           #-}
module Reflex.Dom.Contrib.Layout.TabLayout
       (
         tabCssBS
       , TabInfo(..)
       , staticTabbedLayout
       , dynamicTabbedLayout
       ) where



import Reflex.Dom.Contrib.Layout.ClayUtils
import Reflex.Dom.Contrib.Layout.FlexLayout
import qualified Reflex as R
import Reflex.Dynamic ()
import qualified Reflex.Dom as RD

import Reflex.Dom.Contrib.Widgets.DynTabs
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Foldable as F
import Data.Default (Default(..)) 

import Prelude hiding (div)
import Clay hiding (wrap)
import Clay.Flexbox (wrap)
import Data.Monoid ((<>))
import qualified Data.Text as T

unSelTabColor :: Color
unSelTabColor = grayish 238

selTabColor :: Color
selTabColor = white

tabCss :: Css
tabCss = do
  let tabAreaWidth = pct 96
  ".tabbed-area" ? do
    display flex
    flexDirection column
    flexWrap Clay.Flexbox.wrap
    "display" -: "-webkit-flex"
    "-webkit-flex-direction" -: "column"
    "-webkit-flex-wrap" -: "wrap"
    ".tab-row" ? do
      position relative
      width tabAreaWidth
      label ? do
        border solid (px 1) (grayish 211)
        padding nil (em 0.7) nil (em 0.7)
        margin nil (em 0.1) (em 0.1) (em 0.1) 
        cursor pointer
      label # ".unselected" ? do
        position relative
        background unSelTabColor
        zIndex 0
      label # ".selected" ? do      
        background selTabColor
        borderBottomColor white
        zIndex 2
      input # ("type" @= "radio") ? display none
    ".tab-row" # after ? do 
      position absolute
      "content" -: quote " "
      width tabAreaWidth
      bottom nil
      left nil
      borderBottom solid (px 1) black
      zIndex (-1)
    ".tab-pane" ? do
      zIndex 1
      width tabAreaWidth
--      marginTop (px (-1))
--      sym padding (em 1)
--      border solid (px 1) (grayish 211)
  ".dyn-tab-bar" ? do
    ul ? do
      listStyle none none none
      sym padding nil
      sym margin nil
    li ? do
      display inline
      background (grayish 200)
      borderStyle solid
      borderWidth4 (px 1) (px 1) nil (px 1)
      margin nil (px 5) nil nil
      a ? do
        paddingLeft (px 10)
        paddingRight (px 10)
    "li.active" ? do
        background white
  ".dyn-tab-pane" ? do
    display block

tabCssBS::B.ByteString
tabCssBS = cssToBS tabCss

data TabInfo t m a = TabInfo { tabID::T.Text,
                               tabName::T.Text,
                               tabWidget::(RD.DomBuilder t m,MonadIO (R.PushM t))=>m a }



instance Eq (TabInfo t m a) where
  (TabInfo _ x _) == (TabInfo _ y _) = x == y

instance Ord (TabInfo t m a) where
  compare (TabInfo _ x _) (TabInfo _ y _) = compare x y



data StaticTabConfig = StaticTabConfig
                       {
                         tabControlClass::T.Text, -- surrounds the entire control.  Allows styling and provides scope                         
                         tabPaneClass::T.Text, -- for the div around each pane
                         tabRowClass::T.Text,
                         indicatorOnClass::T.Text, -- for the tab indicator when the tab is selected
                         indicatorOffClass::T.Text -- for the tab indicator when the tab is un-selected
                       }

instance Default StaticTabConfig where
  def = StaticTabConfig "tabbed-area" "tab-pane" "tab-row" "selected" "unselected"
                       
staticTabbedLayout::(MonadIO (R.PushM t),RD.DomBuilder t m, RD.PostBuild t m, MonadFix m,
                      RD.MonadHold t m,Traversable f)=>StaticTabConfig->TabInfo t m a->f (TabInfo t m a)->m (f a)
staticTabbedLayout config curTab tabs = do
  let curTabchecked tab = if tab==curTab then "checked" RD.=: "" else M.empty
      tabInput tab = RD.elAttr "input" (("id" RD.=: (tabID tab))
                                        <> ("type" RD.=: "radio")
                                        <> ("name" RD.=: "grp")
                                        <> curTabchecked tab) RD.blank 
      makeLabelAttrs tab ct = attrsIf ("for" RD.=: (tabID tab)) ("class" RD.=: indicatorOnClass config) ("class" RD.=: indicatorOffClass config) ((==tab) <$> ct)
      makeTabAttrs tab ct = attrsIf ("class" RD.=: tabPaneClass config) ("style" RD.=: "display: block") ("style" RD.=: "display: none") ((==tab) <$> ct)
      tabLabelEv tab ct = fmap (const tab) . (RD.domEvent RD.Click . fst) <$> (RD.elDynAttr' "label" (makeLabelAttrs tab ct) $ RD.text (tabName tab))
      tabEv tab ct = flexItem (tabInput tab >> tabLabelEv tab ct)
      contentDiv ctDyn tab = RD.elDynAttr "div" (makeTabAttrs tab ctDyn) (tabWidget tab)
  RD.divClass (tabControlClass config) $ flexCol $ mdo
    curTabEv <- RD.leftmost . F.toList <$> (RD.divClass (tabRowClass config) $ flexRow (traverse (`tabEv` curTabDyn) tabs)) -- make the tab bar
    curTabDyn <- R.foldDyn const curTab curTabEv 
    flexRow $ traverse (contentDiv curTabDyn) tabs -- and now the tabs


attrsIf::R.Reflex t=>M.Map T.Text T.Text->M.Map T.Text T.Text -> M.Map T.Text T.Text -> R.Dynamic t Bool -> R.Dynamic t (M.Map T.Text T.Text)
attrsIf alwaysAttrs trueAttrs falseAttrs ifDyn = f <$> ifDyn where
  f True = alwaysAttrs <> trueAttrs
  f False = alwaysAttrs <> falseAttrs


staticTabbedLayout'::(MonadIO (R.PushM t),RD.DomBuilder t m,Traversable f)=>TabInfo t m a->f (TabInfo t m a)->m (f a)
staticTabbedLayout' curTab tabs = RD.divClass "tabbed-area" $ do
  let curTabchecked tab = if tab==curTab then "checked" RD.=: "" else M.empty
      tabInput tab = RD.elAttr "input" (("id" RD.=: (tabID tab))
                                         <> ("type" RD.=: "radio")
                                         <> ("name" RD.=: "grp")
                                         <> curTabchecked tab) RD.blank
      tabLabel tab = RD.elAttr "label" ("for" RD.=: (tabID tab)) $ RD.text (tabName tab)
      tabEv ti = tabInput ti >> tabLabel ti
      contentDiv tab = flexItem $ RD.divClass "tab-div" (tabWidget tab)
  traverse (\t -> tabEv t >> contentDiv t) tabs

--dynamic 
instance (RD.DomBuilder t m, RD.PostBuild t m)=>Tab t m (TabInfo t m a) where
  tabIndicator (TabInfo tabId tabLabel _) boolDyn = do
    let labelAttrs = M.fromList [("for",tabId)]
        radioAttrs = M.fromList [("id",tabId),("type","radio"),("style","display: none")]
        selectorAttrsDyn = addActiveClass boolDyn (RD.constDyn M.empty) 
    RD.elDynAttr "li" selectorAttrsDyn $ do    
      (e,_) <- RD.elAttr' "label" labelAttrs $ RD.text tabLabel
      RD.elAttr "input" radioAttrs RD.blank
      return $ RD.domEvent RD.Click e
      
--    RD.button tabLabel
--    RD.el "span" $ RD.text label 

-- this use of MonadWIdget should be fixed but it's tricky when calling into Reflex.Dom.Contrib functions that use MOnadWidget
dynamicTabbedLayout::(MonadIO (R.PushM t), R.MonadSample t m, RD.MonadWidget t m)=>
                     TabInfo t m a->R.Dynamic t [TabInfo t m a]->m (R.Dynamic t [a])
dynamicTabbedLayout cur allDyn = RD.divClass "tabbed-area" $ do
  allSampled <- R.sample $ R.current allDyn
  bar <- RD.divClass "dyn-tab-bar" $ RD.elAttr "ul" ("class" RD.=: "tab-pane") $ tabBar (TabBarConfig cur  allSampled (R.updated allDyn) R.never)
  let paneAttrs = M.empty
      setTabWidget t@(TabInfo _ _ w) = tabPane paneAttrs (_tabBar_curTab bar) t w -- m a 
      setTabWidgets = mapM setTabWidget -- m [a]
      widgetDyn = setTabWidgets <$> allDyn -- m (Dynamic t (m [a])) 
  RD.widgetHold (setTabWidgets allSampled) (R.updated widgetDyn)
