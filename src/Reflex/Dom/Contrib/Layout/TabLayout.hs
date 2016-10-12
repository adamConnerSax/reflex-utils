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



--import Reflex.Dom.Contrib.Layout.LayoutM
import Reflex.Dom.Contrib.Layout.Types
import Reflex.Dom.Contrib.Layout.ClayUtils


import qualified Reflex as R
import qualified Reflex.Dom as RD
import Reflex.Dom.Contrib.Widgets.DynTabs
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as B
import qualified Data.Map as M


import Prelude hiding (div)
import Clay
import Data.String (fromString)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Set as S

unSelTabColor = grayish 238
selTabColor = white

tabCss = do
  ".tabbed-area" ? do
    "display" -: "flex"
    "flex-wrap" -: "wrap"
    label ? do
      background (grayish 238)
      border solid (px 1) (grayish 211)
      padding (em 0.7) (em 0.7) (em 1) (em 1)
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
    input # ("type" @= "radio") # checked |+ label ? do
      background (grayish 255)
      borderBottom solid (px 1) (grayish 255)
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
  let curTabchecked tab = if tab==curTab then ("checked" RD.=: "") else M.empty
      tabInput t@(TabInfo tabId _ _ )= RD.elAttr "input" (("id" RD.=: tabId)
                                                        <> ("type" RD.=: "radio")
                                                        <> ("name" RD.=: "grp")
                                                        <> curTabchecked t) $ RD.blank
      tabLabel (TabInfo tabId name _) = RD.elAttr "label" ("for" RD.=: tabId) $ RD.text name 
      contentDiv (TabInfo _ _ w) = RD.divClass "tab-div" w
  mapM (\t -> tabInput t >> tabLabel t >> contentDiv t) tabs
--  return ()


--dynamic 
instance Eq (TabInfo t m a) where
  (TabInfo _ x _) == (TabInfo _ y _) = x == y

instance Ord (TabInfo t m a) where
  compare (TabInfo _ x _) (TabInfo _ y _) = compare x y

instance RD.MonadWidget t m=>Tab m (TabInfo t m a) where
  tabIndicator (TabInfo _ label _) = RD.el "span" $ RD.text label 

dynamicTabbedLayout::(MonadIO (R.PushM t), R.MonadSample t m, RD.MonadWidget t m)=>
                     TabInfo t m a->R.Dynamic t [TabInfo t m a]->m (R.Dynamic t [a])
dynamicTabbedLayout cur allDyn = do
  allSampled <- R.sample $ R.current allDyn
  curTabDyn <- tabBar "" cur allSampled (R.updated allDyn) R.never (R.constDyn S.empty)
  let setTabWidget t@(TabInfo _ _ w) = tabPane curTabDyn t w -- m a 
      setTabWidgets tabs = mapM setTabWidget tabs -- m [a]
      widgetDyn = setTabWidgets <$> allDyn -- m (Dynamic t (m [a])) 
  RD.widgetHold (setTabWidgets allSampled) (R.updated widgetDyn)
