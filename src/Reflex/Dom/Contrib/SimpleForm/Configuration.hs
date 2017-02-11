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
  , HasCssConfiguration(..)
  , HasInputElementConfig(..)
  , HasSimpleFormConfiguration(..)
  , setInputConfig
  , formItemStyle
  , validInputStyle
  , invalidInputStyle
  , observerOnlyStyle
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

type Placeholder = Text
type Title = Text
type LabelText = Text
data LabelPosition = LabelBefore | LabelAfter

-- do the attributes below need to be dynamic?  That would complicate things...
data LabelConfig = LabelConfig { labelPosition::LabelPosition, labelText::LabelText, labelAttrs::Map Text Text }
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
    formItem::SFLayoutF t m
  , layoutOriented::LayoutOrientation->SFLayoutF t m
  , layoutFill::LayoutDirection->SFLayoutF t m
  , layoutCentered::LayoutOrientation->SFLayoutF t m
  , layoutCollapsible::Text -> CollapsibleInitialState->SFLayoutF t m
  }

data CssConfiguration = CssConfiguration
  {
    _cssFormWrapper::CssClasses
  , _cssForm::CssClasses
  , _cssObserver::CssClasses
  , _cssAllItems::CssClasses
  , _cssAllInputs::CssClasses
  , _cssValidInputs::CssClasses
  , _cssInvalidInputs::CssClasses
  , _cssReadOnly::CssClasses
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


--makeLenses ''LayoutConfiguration
makeClassy ''CssConfiguration
makeClassy ''InputElementConfig
makeClassy ''SimpleFormConfiguration

-- these functions allow direct use from within the reader

setInputConfig::Monad m=>InputElementConfig->SFLayoutF t m
setInputConfig ic = local (\cfg -> cfg {_inputConfig = ic })

formItemStyle::Monad m=>SFR t m CssClasses 
formItemStyle = (_cssAllItems . _cssConfig) <$> ask

validInputStyle::Monad m=>SFR t m CssClasses 
validInputStyle = (_cssValidInputs . _cssConfig) <$> ask

invalidInputStyle::Monad m=>SFR t m CssClasses
invalidInputStyle = (_cssInvalidInputs . _cssConfig) <$> ask 

observerOnlyStyle::Monad m=>SFR t m CssClasses
observerOnlyStyle = (_cssReadOnly . _cssConfig) <$> ask

getFormType::Monad m=>SFR t m FormType
getFormType = _formType <$> ask

setToObserve::Monad m=>SFLayoutF t m
setToObserve = local (\fc -> fc {_formType = ObserveOnly })
--  fc <- getFormConfig
--  setFormConfig fc{ formType = ObserveOnly }  w

sfItem::Monad m=>SFLayoutF t m 
sfItem ra = do
  f <- formItem . _layoutConfig <$> ask
  f ra

sfCenter::Monad m=>LayoutOrientation->SFLayoutF t m
sfCenter o ra = do
  f <- layoutCentered . _layoutConfig <$> ask
  f o ra

sfFill::Monad m=>LayoutDirection->SFLayoutF t m
sfFill d ra = do
  f <- layoutFill . _layoutConfig <$> ask
  f d ra

sfOrient::Monad m=>LayoutOrientation->SFLayoutF t m
sfOrient o ra = do
  f <- layoutOriented . _layoutConfig <$> ask
  f o ra

sfCollapsible::Monad m=>Text->CollapsibleInitialState->SFLayoutF t m
sfCollapsible t is ra = do
  f <- layoutCollapsible . _layoutConfig <$> ask
  f t is ra
  
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
