{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Dom.Contrib.Layout.Types where

import Control.Lens (makeClassy)

import qualified Reflex as R
import Reflex.Dom ((=:))
import qualified Reflex.Dom as RD 
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid ((<>))
import qualified Data.Sequence as S

class IsCssClass a where
  toCssString::a->T.Text
  
toStaticAttributes::IsCssClass a=>a->M.Map T.Text T.Text
toStaticAttributes x = "class" =: toCssString x

data CssClass = CssClass T.Text deriving (Show,Ord,Eq)
instance IsCssClass CssClass where
  toCssString (CssClass c) = c

data CssClasses = CssClasses [CssClass] deriving (Show,Ord,Eq)
instance IsCssClass CssClasses where
  toCssString (CssClasses cs) = T.unwords $ toCssString <$> cs
  
instance Monoid CssClasses where
  mempty = CssClasses []
  (CssClasses a) `mappend` (CssClasses b) = CssClasses (a `mappend` b) 

emptyCss::CssClasses
emptyCss = CssClasses []

oneClass::T.Text -> CssClasses
oneClass x = CssClasses [CssClass x] 

-- for Layout specification
data LayoutDirection = LayoutLeft | LayoutRight | LayoutTop | LayoutBottom
data LayoutOrientation = LayoutHorizontal | LayoutVertical

-- For Layout optimization
data LNodeConstraint = OpensLNode | InLNode | ClosesLNode deriving (Show)

data LNodeType = LDiv deriving (Eq,Show)
nodeTypeTag::LNodeType -> T.Text
nodeTypeTag LDiv = "div"

data LNode = LNode LNodeType CssClasses deriving (Show)

lNodeToFunction::RD.DomBuilder t m=> LNode -> m a -> m a
lNodeToFunction (LNode LDiv css) = RD.divClass (toCssString css)

data OpenLNode = OpenLNode { olnType::LNodeType, olnCss::CssClasses } deriving (Show)
closeLNode::OpenLNode -> LNode
closeLNode (OpenLNode nt css) = LNode nt css

openNAddCss::CssClasses -> OpenLNode -> OpenLNode
openNAddCss css (OpenLNode nt css') = OpenLNode nt (css <> css')

data LayoutInstruction = LayoutInstruction LNodeConstraint OpenLNode deriving (Show)

type LISeq = S.Seq LayoutInstruction


-- for LayoutM
type CssGridRowClass = CssClass
type CssColumnClasses = CssClasses
type GridColWidth = Int

--data CssGridConfig = CssGridConfig { _gcGridRow::CssClass, _gcGridColumns::CssClasses } deriving (Show)

type LayoutClassKey = T.Text
type LayoutClassMap = M.Map LayoutClassKey CssClasses
data LayoutClassDynamic t = LayoutClassDynamic { _lcdInitDyn::R.Dynamic t CssClasses, _lcdEvent::R.Event t CssUpdate }
type LayoutClassDynamicMap t = M.Map LayoutClassKey (LayoutClassDynamic t)

data LayoutConfig t = LayoutConfig { _lcStaticCssMap::LayoutClassMap, _lcDynamicCssMap::LayoutClassDynamicMap t }

data CssUpdate = UpdateDynamic CssClasses | AddToDynamic CssClasses deriving (Eq,Show)


data LayoutDescription t = LayoutDescription { _ldClasses::CssClasses
                                             , _ldLayoutClassKeys::[LayoutClassKey]
                                             }

instance Show (LayoutDescription t) where
  show (LayoutDescription classes classKeys) = "classes=" ++ show classes ++ "; classKeys=" ++ show classKeys 

data LayoutInfo t  = LayoutInfo { _liDescription::LayoutDescription t
                                , _liNewClasses::CssClasses
                                , _liDynamicCss::Maybe (R.Dynamic t CssClasses)
                                } 


--data LayoutTree t = LayoutNode { _lnInfo::LayoutInfo t, _lnChildren::[LayoutTree t]} --Rose Tree

data LayoutS t = LayoutS {  _lsClassMap::LayoutClassMap
                          , _lsDynamicCssMap::LayoutClassDynamicMap t
                         }

makeClassy ''LayoutConfig
makeClassy ''LayoutClassDynamic
makeClassy ''LayoutDescription
makeClassy ''LayoutInfo
makeClassy ''LayoutS





