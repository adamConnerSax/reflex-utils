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
       , StaticTabConfig(..)
       , staticTabbedLayout
       , dynamicTabbedLayout
       ) where



import Reflex.Dom.Contrib.Layout.ClayUtils
import Reflex.Dom.Contrib.Layout.FlexLayout
import Reflex.Dom.Contrib.Layout.Types (CssClass(..),CssClasses(..),toCssString)
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

data TabInfo t m a = TabInfo { tabID :: T.Text
                             , tabLabel :: R.Dynamic t (T.Text, M.Map T.Text T.Text) 
                             , tabWidget :: m a }

instance Eq (TabInfo t m a) where
  (TabInfo x _ _) == (TabInfo y _ _) = x == y

instance Ord (TabInfo t m a) where
  compare (TabInfo x _ _) (TabInfo y _ _) = compare x y


-- NB, if you use other than the defaults you will need to add css to manage functionality
-- Might be easier to wrap tab in a class and then write styling from there.
-- This is not a good solution for nested tabs unless you style them all.
data StaticTabConfig = StaticTabConfig
                       {
                         tabSpecificClass::CssClass -- goes in every element for targeting css
                       , tabControlClass::CssClass -- surrounds the entire control.  Allows styling and provides scope                       
                       , tabPaneClass::CssClass  -- for the div around each pane
                       , tabRowClass::CssClass
                       , indicatorOnClass::CssClass  -- for the tab indicator when the tab is selected
                       , indicatorOffClass::CssClass -- for the tab indicator when the tab is un-selected
                       }

instance Default StaticTabConfig where
  def = StaticTabConfig (CssClass "") (CssClass "tabbed-area") (CssClass "tab-pane") (CssClass "tab-row") (CssClass "selected") (CssClass "unselected")
                       
staticTabbedLayout::({- MonadIO (R.PushM t), -}RD.DomBuilder t m, RD.PostBuild t m, MonadFix m,
                      RD.MonadHold t m,Traversable f)=>StaticTabConfig->TabInfo t m a->f (TabInfo t m a)->m (f a)
staticTabbedLayout config curTab tabs = do
  let curTabchecked tab = if tab==curTab then "checked" RD.=: "" else M.empty
      tabClass c = "class" RD.=: (toCssString $ CssClasses [tabSpecificClass config,c])
      tabInput tab = RD.elAttr "input" (("id" RD.=: (tabID tab))
                                        <> ("type" RD.=: "radio")
                                        <> ("name" RD.=: "grp")
                                        <> curTabchecked tab) RD.blank 
      makeLabelAttrs tab ct =
        let selAttrs = attrsIf ("for" RD.=: tabID tab) (tabClass $ indicatorOnClass config) (tabClass $ indicatorOffClass config) ((==tab) <$> ct)
            combineVals v1 v2 = v1 <> " " <> v2
        in R.zipDynWith (M.unionWith combineVals) selAttrs (snd <$> tabLabel tab)
      makeTabAttrs tab ct = attrsIf (tabClass $ tabPaneClass config) ("style" RD.=: "display: block") ("style" RD.=: "display: none") ((==tab) <$> ct)
      tabLabelEv tab ct = fmap (const tab) . (RD.domEvent RD.Click . fst) <$> (RD.elDynAttr' "label" (makeLabelAttrs tab ct) $ RD.dynText (fst <$> tabLabel tab))
      tabEv tab ct = flexItem (tabInput tab >> tabLabelEv tab ct)
      contentDiv ctDyn tab = RD.elDynAttr "div" (makeTabAttrs tab ctDyn) (tabWidget tab)
      tabControlClasses = CssClasses [tabSpecificClass config, tabControlClass config]
  RD.divClass (toCssString tabControlClasses) $ flexCol $ mdo
    let tabRowClasses = CssClasses [tabSpecificClass config, tabRowClass config]
    curTabEv <- RD.leftmost . F.toList <$> (RD.divClass (toCssString tabRowClasses) $ flexRow (traverse (`tabEv` curTabDyn) tabs)) -- make the tab bar
    curTabDyn <- R.foldDyn const curTab curTabEv 
    flexRow $ traverse (contentDiv curTabDyn) tabs -- and now the tabs


attrsIf::R.Reflex t=>M.Map T.Text T.Text->M.Map T.Text T.Text -> M.Map T.Text T.Text -> R.Dynamic t Bool -> R.Dynamic t (M.Map T.Text T.Text)
attrsIf alwaysAttrs trueAttrs falseAttrs ifDyn = f <$> ifDyn where
  f True = alwaysAttrs <> trueAttrs
  f False = alwaysAttrs <> falseAttrs

{-
staticTabbedLayout'::(MonadIO (R.PushM t),RD.DomBuilder t m,Traversable f)=>TabInfo t m a->f (TabInfo t m a)->m (f a)
staticTabbedLayout' curTab tabs = RD.divClass "tabbed-area" $ do
  let curTabchecked tab = if tab==curTab then "checked" RD.=: "" else M.empty
      tabInput tab = RD.elAttr "input" (("id" RD.=: (tabID tab))
                                         <> ("type" RD.=: "radio")
                                         <> ("name" RD.=: "grp")
                                         <> curTabchecked tab) RD.blank
      makeLabelAttrs tab ct =
        let selAttrs = attrsIf ("for" RD.=: tabID tab) (tabClass $ indicatorOnClass config) (tabClass $ indicatorOffClass config) ((==tab) <$> ct)
        in R.zipDynWith M.union selAttrs (tabLabelAttrs tab)
      tabLabel tab = RD.elDynAttr "label" makeLabelAttrs tab ct $ RD.dynText (fst <$> tabLabel tab)
      tabEv ti = tabInput ti >> tabLabel ti
      contentDiv tab = flexItem $ RD.divClass "tab-div" (tabWidget tab)
  traverse (\t -> tabEv t >> contentDiv t) tabs
-}

--dynamic 
instance (RD.DomBuilder t m, RD.PostBuild t m)=>Tab t m (TabInfo t m a) where
  tabIndicator (TabInfo idText labelTextAndAttrs _) boolDyn = do
    let labelAttrsDyn =
          let staticAttrsDyn = R.constDyn ("for" RD.=: idText) 
          in R.zipDynWith M.union staticAttrsDyn (snd <$> labelTextAndAttrs)
        radioAttrs = M.fromList [("id",idText),("type","radio"),("style","display: none")]
        selectorAttrsDyn = addActiveClass boolDyn (RD.constDyn M.empty) 
    RD.elDynAttr "li" selectorAttrsDyn $ do    
      (e,_) <- RD.elDynAttr' "label" labelAttrsDyn $ RD.dynText (fst <$> labelTextAndAttrs)
      RD.elAttr "input" radioAttrs RD.blank
      return $ RD.domEvent RD.Click e
      
--    RD.button tabLabel
--    RD.el "span" $ RD.text label 

-- this use of MonadWIdget should be fixed but it's tricky when calling into Reflex.Dom.Contrib functions that use MOnadWidget
dynamicTabbedLayout::({- MonadIO (R.PushM t),-} R.MonadSample t m, RD.MonadWidget t m)=>
                     TabInfo t m a->R.Dynamic t [TabInfo t m a]->m (R.Dynamic t [a])
dynamicTabbedLayout cur allDyn = RD.divClass "tabbed-area" $ do
  allSampled <- R.sample $ R.current allDyn
  bar <- RD.divClass "dyn-tab-bar" $ RD.elAttr "ul" ("class" RD.=: "tab-pane") $ tabBar (TabBarConfig cur  allSampled (R.updated allDyn) R.never)
  let paneAttrs = M.empty
      setTabWidget t@(TabInfo _ _ w) = tabPane paneAttrs (_tabBar_curTab bar) t w -- m a 
      setTabWidgets = mapM setTabWidget -- m [a]
      widgetDyn = setTabWidgets <$> allDyn -- m (Dynamic t (m [a])) 
  RD.widgetHold (setTabWidgets allSampled) (R.updated widgetDyn)
