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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Dom.Contrib.Layout.Types where

import Control.Lens (makeLenses,makeClassy)
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.State  (MonadState)
import Control.Monad.Reader (MonadReader)

import qualified Control.Category as C
import qualified Reflex as R
import Reflex.Dom ((=:))
import qualified Reflex.Dom as RD 
import qualified Data.Map as M
import qualified Data.Text as T
import qualified GHCJS.DOM.Element as E
import Control.Monad.State (StateT)
import Control.Monad.Reader (ReaderT)
import Data.Monoid ((<>))
import qualified Data.Sequence as S
--import Data.Default (Default(..))

class IsCssClass a where
  toCssString::a->T.Text
  
toStaticAttributes::IsCssClass a=>a->M.Map T.Text T.Text
toStaticAttributes x = ("class" =: toCssString x)

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

-- For Layout optimization
data LNodeConstraint = OpensLNode | InLNode | ClosesLNode deriving (Show)

data LNodeType = LDiv deriving (Eq,Show)
nodeTypeTag::LNodeType -> T.Text
nodeTypeTag LDiv = "div"

data LNode = LNode LNodeType CssClasses deriving (Show)

lNodeToFunction::RD.MonadWidget t m=> LNode -> m a -> m a
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

data CssGridConfig = CssGridConfig { _gcGridRow::CssClass, _gcGridColumns::CssClasses } deriving (Show)

type LayoutClassKey = String
type LayoutClassMap = M.Map LayoutClassKey CssClasses
data LayoutClassDynamic t = LayoutClassDynamic { _lcdInitDyn::R.Dynamic t CssClasses, _lcdEvent::R.Event t CssUpdate }
type LayoutClassDynamicMap t = M.Map LayoutClassKey (LayoutClassDynamic t)

data LayoutConfig t = LayoutConfig { _lcGridConfig::CssGridConfig, _lcStaticCssMap::LayoutClassMap, _lcDynamicCssMap::LayoutClassDynamicMap t }

data CssUpdate = UpdateDynamic CssClasses | AddToDynamic CssClasses deriving (Eq,Show)


data LayoutPropertyKey = LPK_Width deriving (Ord,Eq,Show)
data LayoutProperty = LP_Width { lpWidth::Int } deriving (Show)
type LayoutPropertyMap = M.Map LayoutPropertyKey LayoutProperty



type LayoutF t = LayoutConfig t->LayoutTree t->LayoutTree t

data LayoutDescription t = LayoutDescription { _ldLayoutF::LayoutF t
                                             , _ldProperties::LayoutPropertyMap
                                             , _ldLayoutClassKeys::[LayoutClassKey]
                                             } 


data LayoutInfo t = LayoutInfo { _liDescription::LayoutDescription t
                               , _liNewClasses::CssClasses
                               , _liDynamicCss::Maybe (R.Dynamic t CssClasses)
                               , _liElt::Maybe E.Element
                               } 


data LayoutTree t = LayoutNode { _lnInfo::LayoutInfo t, _lnChildren::[LayoutTree t]} --Rose Tree


data LayoutS t = LayoutS {  _lsTree::LayoutTree t
                          , _lsClassMap::LayoutClassMap
                          , _lsDynamicCssMap::LayoutClassDynamicMap t
                         }


newtype LayoutM t m a = LayoutM { unLayoutM::StateT (LayoutS t) (ReaderT (LayoutConfig t) m) a } deriving (Functor,Applicative,Monad,MonadFix,MonadIO,MonadException,MonadAsyncException,MonadState (LayoutS t), MonadReader (LayoutConfig t))




makeClassy ''CssGridConfig
makeClassy ''LayoutConfig
makeClassy ''LayoutClassDynamic
makeClassy ''LayoutDescription
makeClassy ''LayoutInfo
makeClassy ''LayoutTree
makeClassy ''LayoutS


