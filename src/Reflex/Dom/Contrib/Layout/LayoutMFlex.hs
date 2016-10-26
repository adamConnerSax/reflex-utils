{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Reflex.Dom.Contrib.Layout.LayoutM (SupportsLayoutM,addNewLayoutNode) 
import Reflex.Dom.Contrib.Layout.Types (CssClass(..),CssClasses(..),LayoutDescription(..),LayoutM) 
import Reflex.Dom.Contrib.Layout.FlexLayout (numberFlexGrowOptions) -- for re-export

import qualified Reflex as R
import qualified Reflex.Dom as RD

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid ((<>))

flexLayoutRowD::R.Reflex t=>[T.Text]->LayoutDescription t
flexLayoutRowD tags =
  let flexRowClasses = CssClasses [CssClass "gl-flex-row"]
  in LayoutDescription flexRowClasses (["all","row","flexContainer"]++tags)

lmFlexLayoutRow::(RD.PostBuild t m ,SupportsLayoutM t m)=>LayoutM t m a->LayoutM t m a
lmFlexLayoutRow = addNewLayoutNode $ flexLayoutRowD []

lmFlexLayoutRow'::(RD.PostBuild t m ,SupportsLayoutM t m)=>[T.Text]->LayoutM t m a->LayoutM t m a 
lmFlexLayoutRow' tags = addNewLayoutNode $ flexLayoutRowD tags

flexColD::R.Reflex t=>[T.Text]->Int->LayoutDescription t
flexColD tags w =
  let n = Prelude.max 1 $ Prelude.min w numberFlexGrowOptions
      flexColClasses = CssClasses [CssClass ("gl-flex-item-" <> (T.pack $ show n))]
  in LayoutDescription flexColClasses (["all","col","flexItem"]++tags)

lmFlexCol::(RD.PostBuild t m ,SupportsLayoutM t m)=>Int->LayoutM t m a->LayoutM t m a
lmFlexCol w = addNewLayoutNode (flexColD [] w)

lmFlexCol'::(RD.PostBuild t m ,SupportsLayoutM t m)=>[T.Text]->Int->LayoutM t m a->LayoutM t m a
lmFlexCol' tags w = addNewLayoutNode (flexColD tags w)

flexLayoutColD::R.Reflex t=>[T.Text]->LayoutDescription t
flexLayoutColD tags =
  let flexColClasses = CssClasses [CssClass "gl-flex-col"]
  in LayoutDescription flexColClasses (["all","col","flexContainer"] ++ tags)

lmFlexLayoutCol::(RD.PostBuild t m ,SupportsLayoutM t m)=>LayoutM t m a->LayoutM t m a
lmFlexLayoutCol = addNewLayoutNode (flexLayoutColD [])

lmFlexLayoutCol'::(RD.PostBuild t m ,SupportsLayoutM t m)=>[T.Text]->LayoutM t m a->LayoutM t m a
lmFlexLayoutCol' tags = addNewLayoutNode (flexLayoutColD tags)

flexRowD::R.Reflex t=>[T.Text]->Int->LayoutDescription t
flexRowD tags h =
  let n = Prelude.max 1 $ Prelude.min h numberFlexGrowOptions
      flexRowClasses = CssClasses [CssClass ("gl-flex-item-" <> (T.pack $ show n))]
  in LayoutDescription flexRowClasses (["all","row","flexItem"]++tags)

lmFlexRow::(RD.PostBuild t m ,SupportsLayoutM t m)=>Int->LayoutM t m a->LayoutM t m a
lmFlexRow h = addNewLayoutNode (flexRowD [] h)

lmFlexRow'::(RD.PostBuild t m ,SupportsLayoutM t m)=>[T.Text]->Int->LayoutM t m a->LayoutM t m a
lmFlexRow' tags h = addNewLayoutNode (flexRowD tags h)
