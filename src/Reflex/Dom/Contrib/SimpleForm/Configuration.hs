{-# LANGUAGE RankNTypes             #-}  
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
module Reflex.Dom.Contrib.SimpleForm.Configuration
  (
    Placeholder
  , Title
  , LabelText
  , LabelPosition(..)
  , LabelConfig(..)
  , FormType(..)
  , CollapsibleInitialState(..)
  , SFR
  , SFRW
  , SFLayoutF
  , liftLF
  , DynAttrs
  , LayoutConfiguration(..)
  , CssConfiguration(..)
  , InputElementConfig(..)
  , BuilderFunctions(..)
  , SimpleFormConfiguration(..)
  , SimpleFormIncludedCss(..)
  , SFConfigChanger
  , HasCssConfiguration(..)
  , HasInputElementConfig(..)
  , HasSimpleFormConfiguration(..)
  , setInputConfig
  , wrapperClasses
  , itemClasses
  , validDataClasses
  , invalidDataClasses
  , setToObserve
  , getFormType
  , sfItemL
  , sfItemR
  , sfRow
  , sfCol
  , sfItem
  , sfCenter
  , sfOrient
  , sfFill
  , sfCollapsible
  , sfDynamicDiv
  ) where

import Reflex.Dom.Contrib.Layout.Types (CssClasses,LayoutOrientation(..),LayoutDirection(..))
import Reflex.Dom.Contrib.SimpleForm.DynValidation

import qualified DataBuilder                     as B
import Reflex (Dynamic)

import Control.Lens.TH
import Control.Monad.Reader (ReaderT,ask,local)
import Control.Monad.Morph (hoist)
import Data.Text (Text)
import Data.Map (Map)
import Data.ByteString (ByteString)
import Reflex.Dom.Contrib.CssUtils (CssLinks)

type Placeholder = Text
type Title = Text
type LabelText = Text
data LabelPosition = LabelBefore | LabelAfter

-- do the attributes below need to be dynamic?  That would complicate things...
data LabelConfig = LabelConfig { labelText::LabelText, labelAttrs::Map Text Text }
data FormType = Interactive | ObserveOnly deriving (Eq)

data CollapsibleInitialState = CollapsibleStartsOpen | CollapsibleStartsClosed deriving (Show,Eq,Ord,Enum,Bounded)

type SFR t m = ReaderT (SimpleFormConfiguration t m) m
type SFRW t m a = SFR t m (DynValidation t a)

type SFLayoutF t m = forall a.(SFR t m a -> SFR t m a)

liftLF::Monad m=>(forall a.m a->m a)->SFLayoutF t m
liftLF = hoist

type DynAttrs t = Dynamic t (Map Text Text)

data LayoutConfiguration t m = LayoutConfiguration 
  {
    layoutWrapper::FormType -> SFLayoutF t m
  , layoutItem::FormType->SFLayoutF t m
  , layoutOriented::FormType->LayoutOrientation->SFLayoutF t m
  , layoutFill::FormType->LayoutDirection->SFLayoutF t m
  , layoutCentered::FormType->LayoutOrientation->SFLayoutF t m
  , layoutCollapsible::FormType->Text -> CollapsibleInitialState->SFLayoutF t m
  }

data CssConfiguration = CssConfiguration
  {
    _cssWrapper::FormType->CssClasses
  , _cssAllItems::FormType->CssClasses
--  , _cssAllData::FormType->CssClasses
  , _cssValidData::FormType->CssClasses
  , _cssInvalidData::FormType->CssClasses -- ??
  }

data InputElementConfig = InputElementConfig
  {
    _inputPlaceholder::Maybe Placeholder
  , _inputTitle::Maybe Title
  , _inputLabelConfig::Maybe LabelConfig
  }

data BuilderFunctions t m = BuilderFunctions
  {
    failureF::forall a. Text->SFRW t m a
  , sumF::forall a.[(B.ConName,SFRW t m a)]->Maybe B.ConName->SFRW t m a
  , dynamicDiv::DynAttrs t->SFLayoutF t m
  }

data SimpleFormConfiguration t m = SimpleFormConfiguration 
  {
    _formType::FormType
  , _builderFunctions::BuilderFunctions t m
  , _layoutConfig::LayoutConfiguration t m
  , _cssConfig::CssConfiguration
  , _inputConfig::InputElementConfig
  }

type SFConfigChanger t m = SimpleFormConfiguration t m -> SimpleFormConfiguration t m 

data SimpleFormIncludedCss = SimpleFormIncludedCss { cssToEmbed::ByteString, cssToLink::CssLinks }

--makeLenses ''LayoutConfiguration
makeClassy ''CssConfiguration
makeClassy ''InputElementConfig
makeClassy ''SimpleFormConfiguration


-- these functions allow direct use from within the reader

setInputConfig::Monad m=>InputElementConfig->SFLayoutF t m
setInputConfig ic = local (\cfg -> cfg {_inputConfig = ic })

getClasses::Monad m=>(CssConfiguration -> (FormType->CssClasses))->SFR t m CssClasses
getClasses selector = do
  fType <- _formType <$> ask
  cssF <- (selector . _cssConfig) <$> ask
  return $ cssF fType
  
wrapperClasses::Monad m=>SFR t m CssClasses
wrapperClasses = getClasses _cssWrapper

itemClasses::Monad m=>SFR t m CssClasses 
itemClasses = getClasses _cssAllItems

{-
allDataClasses::Monad m=>SFR t m CssClasses 
allDataClasses = getClasses _cssAllData
-}

validDataClasses::Monad m=>SFR t m CssClasses 
validDataClasses = getClasses _cssValidData

invalidDataClasses::Monad m=>SFR t m CssClasses
invalidDataClasses = getClasses _cssInvalidData

getFormType::Monad m=>SFR t m FormType
getFormType = _formType <$> ask

setToObserve::Monad m=>SFLayoutF t m
setToObserve = local (\fc -> fc {_formType = ObserveOnly })
--  fc <- getFormConfig
--  setFormConfig fc{ formType = ObserveOnly }  w

sfWrapper::Monad m=>SFLayoutF t m
sfWrapper ra = do
  fType <- _formType <$> ask
  f <- layoutWrapper . _layoutConfig <$> ask
  f fType ra  

sfItem::Monad m=>SFLayoutF t m 
sfItem ra = do
  fType <- _formType <$> ask
  f <- layoutItem . _layoutConfig <$> ask
  f fType ra

sfCenter::Monad m=>LayoutOrientation->SFLayoutF t m
sfCenter o ra = do
  fType <- _formType <$> ask
  f <- layoutCentered . _layoutConfig <$> ask
  f fType o ra

sfFill::Monad m=>LayoutDirection->SFLayoutF t m
sfFill d ra = do
  fType <- _formType <$> ask
  f <- layoutFill . _layoutConfig <$> ask
  f fType d ra

sfOrient::Monad m=>LayoutOrientation->SFLayoutF t m
sfOrient o ra = do
  fType <- _formType <$> ask
  f <- layoutOriented . _layoutConfig <$> ask
  f fType o ra

sfCollapsible::Monad m=>Text->CollapsibleInitialState->SFLayoutF t m
sfCollapsible t is ra = do
  fType <- _formType <$> ask
  f <- layoutCollapsible . _layoutConfig <$> ask
  f fType t is ra
  
sfItemL::Monad m=>SFLayoutF t m
sfItemL = sfFill LayoutRight . sfItem 

sfItemR::Monad m=>SFLayoutF t m
sfItemR = sfFill LayoutLeft . sfItem

sfRow::Monad m=>SFLayoutF t m
sfRow  = sfItem . sfOrient LayoutHorizontal

sfCol::Monad m=>SFLayoutF t m
sfCol = sfItem . sfOrient LayoutVertical

sfDynamicDiv::Monad m=>DynAttrs t->SFLayoutF t m
sfDynamicDiv dynAttrs ra = do
  f <- dynamicDiv . _builderFunctions <$> ask
  f dynAttrs ra
