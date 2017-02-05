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
    "display" -: "-webkit-flex"
    "flex-direction" -: "column"
    "-webkit-flex-direction" -: "column"
    "flex-wrap" -: "wrap"
    "-webkit-flex-wrap" -: "wrap"
    label ? do
      border solid (px 1) (grayish 211)
      padding nil (em 0.7) nil (em 0.7)
      cursor pointer
      zIndex 1
      marginLeft (px (-1))
      ".selected" ? do
        background (grayish 200)
      ".unselected" ?
        background white
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
  ".tab-div2" ? do
    width (pct 100)
    marginTop (px (-1))
    sym padding (em 1)
    border solid (px 1) (grayish 211)
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
      contentDiv tab = RD.divClass "tab-div" (tabWidget tab)
  traverse (\t -> tabEv t >> contentDiv t) tabs

staticTabbedLayout::(MonadIO (R.PushM t),RD.DomBuilder t m, RD.PostBuild t m, MonadFix m,
                      RD.MonadHold t m,Traversable f)=>TabInfo t m a->f (TabInfo t m a)->m (f a)
staticTabbedLayout curTab tabs = RD.divClass "tabbed-area" $ mdo
  let curTabchecked tab = if tab==curTab then "checked" RD.=: "" else M.empty
      tabInput tab = RD.elAttr "input" (("id" RD.=: (tabID tab))
                                         <> ("type" RD.=: "radio")
                                         <> ("name" RD.=: "grp")
                                         <> curTabchecked tab) RD.blank -- TabInfo m t () -> m ()
      makeLabelAttrs tab ct = attrsIf ("for" RD.=: (tabID tab)) ("class" RD.=: "selected") ("class" RD.=: "unselected") ((==tab) <$> ct)
      tabLabelEv tab ct = fmap (const tab) . (RD.domEvent RD.Click . fst) <$> (RD.elDynAttr' "label" (makeLabelAttrs tab ct) $ RD.text (tabName tab))
      tabEv tab ct = flexItem (tabInput tab >> tabLabelEv tab ct)
      makeTabAttrs tab ct = attrsIf ("class" RD.=: "tab-div2") ("style" RD.=: "display: block") ("style" RD.=: "display: none") ((==tab) <$> ct)
      contentDiv ctDyn tab = RD.elDynAttr "div" (makeTabAttrs tab ctDyn) (tabWidget tab)
  curTabEv <- RD.leftmost . F.toList <$> flexRow (traverse (`tabEv` curTabEv) tabs) -- Event t (TabInfo t m a)
  curTabDyn <- R.foldDyn const curTab curTabEv
  traverse (contentDiv curTabDyn) tabs


--dynamic 
instance Eq (TabInfo t m a) where
  (TabInfo _ x _) == (TabInfo _ y _) = x == y

instance Ord (TabInfo t m a) where
  compare (TabInfo _ x _) (TabInfo _ y _) = compare x y

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
