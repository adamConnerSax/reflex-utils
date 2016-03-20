{-# LANGUAGE FlexibleContexts #-}
module Reflex.Dom.Contrib.Layout.GridLayout
       (
         newRow
       , newRow'
       , newCol
       ) where

import qualified Reflex as R
import qualified Reflex.Dom as RD
import Reflex.Dom.Contrib.Layout.Core
import qualified Data.Map as M
import Control.Lens (set,(.~),(^.),(<>~),(<>=),(%=),(.=),use)
import Data.Monoid (mappend)
import Data.Maybe (fromJust)
import Data.List (foldl')
import Control.Monad (foldM)
import Control.Monad.Fix (MonadFix)

import Reflex.Dom.Contrib.Layout.Types


-- this uses internal structure of CssClasses which is likely a mistake. Though a very small one.
bestColumn::CssClasses->GridColWidth->GridColWidth->CssClass
bestColumn (CssClasses classes) colW allColW =
  let total = length classes
      best  = round $ (realToFrac $ colW * total)/(realToFrac allColW)
  in classes !! (best-1)

bestColumn'::R.Reflex t=>LayoutConfig t->GridColWidth->GridColWidth->CssClass
bestColumn' lc =
  let ccs = lc ^. (lcGridConfig.gcGridColumns) in bestColumn ccs

rowF::R.Reflex t=>LayoutF t
rowF lc lTree =
  let cols = lTree ^. lnChildren
      rowClasses = CssClasses [lc ^. (lcGridConfig.gcGridRow)] 
      rowInfo = (addNewClassesToTreeTop rowClasses lTree) ^. lnInfo
      getColWidth c = lpWidth $ getLayoutPropertyDef (LP_Width 1) LPK_Width c
      totalColW = foldl (\n c->n+getColWidth c) 0 cols
      colClasses c = CssClasses [bestColumn' lc (getColWidth c) totalColW] 
      newCols = (\c->addNewClassesToTreeTop (colClasses c) c) <$> cols 
  in LayoutNode rowInfo newCols

rowD::R.Reflex t=>LayoutDescription t
rowD = LayoutDescription rowF M.empty ["all","row"]

colF::R.Reflex t=>LayoutF t
colF _ lTree = lTree

colD::R.Reflex t=>GridColWidth->LayoutDescription t
colD w = LayoutDescription colF (LPK_Width RD.=: (LP_Width w)) ["all","col"]

newRow::RD.MonadWidget t m=>LayoutM t m a -> LayoutM t m a
newRow = addNewLayoutNode rowD

newCol::RD.MonadWidget t m=>GridColWidth->LayoutM t m a->LayoutM t m a
newCol w = addNewLayoutNode (colD w)


newRow'::RD.MonadWidget t m=>LayoutM t m a -> LayoutM t m a
newRow' x = newRow $ newCol 1 x
