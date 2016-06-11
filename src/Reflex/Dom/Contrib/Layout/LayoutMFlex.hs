module Reflex.Dom.Contrib.Layout.LayoutMFlex
  (
    lmFlexRow
  , lmFlexRow'
  , lmFlexLayoutRow
  , lmFlexLayoutRow'
  , lmFlexCol
  , lmFlexCol'
  , lmFlexLayoutCol
  , lmFlexLayoutCol'
  ) where

import Reflex.Dom.Contrib.Layout.LayoutM --TODO, qualify or explicitize
import Reflex.Dom.Contrib.Layout.Types --TODO, qualify or explicitize
import Reflex.Dom.Contrib.Layout.FlexLayout (flexCssBS,numberFlexGrowOptions) -- for re-export

import qualified Reflex as R
import qualified Reflex.Dom as RD

import qualified Data.Map as M

flexLayoutRowF::R.Reflex t=>LayoutF t
flexLayoutRowF _ lTree =
  let flexRowClasses = CssClasses [CssClass "gl-flex-row"]
  in addNewClassesToTreeTop flexRowClasses lTree

flexLayoutRowD::R.Reflex t=>[String]->LayoutDescription t
flexLayoutRowD tags = LayoutDescription flexLayoutRowF M.empty (["all","row","flexContainer"]++tags)

lmFlexLayoutRow::RD.MonadWidget t m=>LayoutM t m a->LayoutM t m a
lmFlexLayoutRow = addNewLayoutNode $ flexLayoutRowD []

lmFlexLayoutRow'::RD.MonadWidget t m=>[String]->LayoutM t m a->LayoutM t m a 
lmFlexLayoutRow' tags = addNewLayoutNode $ flexLayoutRowD tags

flexColF::R.Reflex t=>Int->LayoutF t
flexColF w _ lTree =
  let n = Prelude.max 1 $ Prelude.min w numberFlexGrowOptions
      flexColClasses = CssClasses [CssClass ("gl-flex-item-" ++ show n)]
  in addNewClassesToTreeTop flexColClasses lTree

flexColD::R.Reflex t=>[String]->Int->LayoutDescription t
flexColD tags w = LayoutDescription (flexColF w) M.empty (["all","col","flexItem"]++tags)

lmFlexCol::RD.MonadWidget t m=>Int->LayoutM t m a->LayoutM t m a
lmFlexCol w = addNewLayoutNode (flexColD [] w)

lmFlexCol'::RD.MonadWidget t m=>[String]->Int->LayoutM t m a->LayoutM t m a
lmFlexCol' tags w = addNewLayoutNode (flexColD tags w)

flexLayoutColF::R.Reflex t=>LayoutF t
flexLayoutColF _ lTree =
  let flexColClasses = CssClasses [CssClass "gl-flex-col"]
  in addNewClassesToTreeTop flexColClasses lTree

flexLayoutColD::R.Reflex t=>[String]->LayoutDescription t
flexLayoutColD tags = LayoutDescription flexLayoutColF M.empty (["all","col","flexContainer"] ++ tags)

lmFlexLayoutCol::RD.MonadWidget t m=>LayoutM t m a->LayoutM t m a
lmFlexLayoutCol = addNewLayoutNode (flexLayoutColD [])

lmFlexLayoutCol'::RD.MonadWidget t m=>[String]->LayoutM t m a->LayoutM t m a
lmFlexLayoutCol' tags = addNewLayoutNode (flexLayoutColD tags)


flexRowF::R.Reflex t=>Int->LayoutF t
flexRowF h _ lTree =
  let n = Prelude.max 1 $ Prelude.min h numberFlexGrowOptions
      flexRowClasses = CssClasses [CssClass ("gl-flex-item-" ++ show n)]
  in addNewClassesToTreeTop flexRowClasses lTree

flexRowD::R.Reflex t=>[String]->Int->LayoutDescription t
flexRowD tags h = LayoutDescription (flexRowF h) M.empty (["all","row","flexItem"]++tags)

lmFlexRow::RD.MonadWidget t m=>Int->LayoutM t m a->LayoutM t m a
lmFlexRow h = addNewLayoutNode (flexRowD [] h)

lmFlexRow'::RD.MonadWidget t m=>[String]->Int->LayoutM t m a->LayoutM t m a
lmFlexRow' tags h = addNewLayoutNode (flexRowD tags h)
