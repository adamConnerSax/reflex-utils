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
  , SFLayoutF
  , liftLF
  , LayoutConfiguration(..)
  , CssConfiguration(..)
  , InputElementConfig(..)
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
  ) where

import Reflex.Dom.Contrib.Layout.Types (CssClasses,LayoutOrientation(..),LayoutDirection(..))
--import Reflex (MonadHold)
--import Reflex.Dom (DomBuilder)
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

type SFR m = ReaderT (SimpleFormConfiguration m) m
type SFLayoutF m = forall a.(SFR m a -> SFR m a)

liftLF::Monad m=>(forall a.m a->m a)->SFLayoutF m
liftLF = hoist


data LayoutConfiguration m = LayoutConfiguration 
  {
    formItem::SFLayoutF m
  , layoutOriented::LayoutOrientation->SFLayoutF m
  , layoutFill::LayoutDirection->SFLayoutF m
  , layoutCentered::LayoutOrientation->SFLayoutF m
  , layoutCollapsible::Text -> CollapsibleInitialState->SFLayoutF m
  }

data CssConfiguration = CssConfiguration
  {
    _cssAllItems::CssClasses
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

data SimpleFormConfiguration m = SimpleFormConfiguration 
  {
    _formType::FormType
  , _layoutConfig::LayoutConfiguration m
  , _cssConfig::CssConfiguration
  , _inputConfig::InputElementConfig
  }


--makeLenses ''LayoutConfiguration
makeClassy ''CssConfiguration
makeClassy ''InputElementConfig
makeClassy ''SimpleFormConfiguration

-- these functions allow direct use from within the reader

setInputConfig::Monad m=>InputElementConfig->SFLayoutF m
setInputConfig ic = local (\cfg -> cfg {_inputConfig = ic })

formItemStyle::Monad m=>SFR m CssClasses 
formItemStyle = (_cssAllItems . _cssConfig) <$> ask

validInputStyle::Monad m=>SFR m CssClasses 
validInputStyle = (_cssValidInputs . _cssConfig) <$> ask

invalidInputStyle::Monad m=>SFR m CssClasses
invalidInputStyle = (_cssInvalidInputs . _cssConfig) <$> ask 

observerOnlyStyle::Monad m=>SFR m CssClasses
observerOnlyStyle = (_cssReadOnly . _cssConfig) <$> ask

getFormType::Monad m=>SFR m FormType
getFormType = _formType <$> ask

setToObserve::Monad m=>SFLayoutF m
setToObserve = local (\fc -> fc {_formType = ObserveOnly })
--  fc <- getFormConfig
--  setFormConfig fc{ formType = ObserveOnly }  w

sfItem::Monad m=>SFLayoutF m 
sfItem ra = do
  f <- formItem . _layoutConfig <$> ask
  f ra

sfCenter::Monad m=>LayoutOrientation->SFLayoutF m
sfCenter o ra = do
  f <- layoutCentered . _layoutConfig <$> ask
  f o ra

sfFill::Monad m=>LayoutDirection->SFLayoutF m
sfFill d ra = do
  f <- layoutFill . _layoutConfig <$> ask
  f d ra

sfOrient::Monad m=>LayoutOrientation->SFLayoutF m
sfOrient o ra = do
  f <- layoutOriented . _layoutConfig <$> ask
  f o ra

sfCollapsible::Monad m=>Text->CollapsibleInitialState->SFLayoutF m
sfCollapsible t is ra = do
  f <- layoutCollapsible . _layoutConfig <$> ask
  f t is ra
  
sfItemL::Monad m=>SFLayoutF m
sfItemL = sfFill LayoutRight . sfItem 

sfItemR::Monad m=>SFLayoutF m
sfItemR = sfFill LayoutLeft . sfItem

sfRow::Monad m=>SFLayoutF m
sfRow  = sfItem . sfOrient LayoutHorizontal

sfCol::Monad m=>SFLayoutF m
sfCol = sfItem . sfOrient LayoutVertical



