{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
module Reflex.Dom.Contrib.Layout.TabLayout
       (
         tabCssBS
       , TabInfo(..)
       , staticTabbedLayout
       , dynamicTabbedLayout
       ) where



import Reflex.Dom.Contrib.Layout.ClayUtils

import qualified Reflex as R
import qualified Reflex.Dom as RD
import Reflex.Dom.Contrib.Widgets.DynTabs
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as B
import qualified Data.Map as M

import Prelude hiding (div)
import Clay
import Data.Monoid ((<>))
import qualified Data.Text as T

unSelTabColor :: Color
unSelTabColor = grayish 238

selTabColor :: Color
selTabColor = white

tabCss :: Css
tabCss = do
  ".tabbed-area" ? do
    "display" -: "flex"
    "flex-direction" -: "column"
    "flex-wrap" -: "wrap"
    label ? do
      border solid (px 1) (grayish 211)
      padding nil (em 0.7) nil (em 0.7)
      cursor pointer
      zIndex 1
      marginLeft (px (-1))
    label # firstOfType ? do
      marginLeft (px 0)
    ".tab-div" ? do
      width (pct 100)
      marginTop (px (-1))
      sym padding (em 1)
      border solid (px 1) (grayish 211)
      display none
      "order" -: "1"
    input # ("type" @= "radio") ? display none
    input # ("type" @= "radio") # checked |+ label |+ div ? do
      display block
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
                               tabWidget::(RD.MonadWidget t m,MonadIO (R.PushM t))=>m a }


staticTabbedLayout::(MonadIO (R.PushM t),RD.MonadWidget t m)=>TabInfo t m a->[TabInfo t m a]->m [a]
staticTabbedLayout curTab tabs = RD.divClass "tabbed-area" $ do
  let curTabchecked tab = if tab==curTab then "checked" RD.=: "" else M.empty
      tabInput t@(TabInfo tabId _ _ )= RD.elAttr "input" (("id" RD.=: tabId)
                                                        <> ("type" RD.=: "radio")
                                                        <> ("name" RD.=: "grp")
                                                        <> curTabchecked t) RD.blank
      tabLabel (TabInfo tabId tabName _) = RD.elAttr "label" ("for" RD.=: tabId) $ RD.text tabName 
      contentDiv (TabInfo _ _ w) = RD.divClass "tab-div" w
  mapM (\t -> tabInput t >> tabLabel t >> contentDiv t) tabs
--  return ()


--dynamic 
instance Eq (TabInfo t m a) where
  (TabInfo _ x _) == (TabInfo _ y _) = x == y

instance Ord (TabInfo t m a) where
  compare (TabInfo _ x _) (TabInfo _ y _) = compare x y

instance RD.MonadWidget t m=>Tab t m (TabInfo t m a) where
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
