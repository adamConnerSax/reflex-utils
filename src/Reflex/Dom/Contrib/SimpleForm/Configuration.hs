{-# LANGUAGE RankNTypes #-}  
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
module Reflex.Dom.Contrib.SimpleForm.Configuration where

import Reflex.Dom.Contrib.Layout.Types (CssClasses,LayoutOrientation,LayoutDirection)
--import Reflex (MonadHold)
--import Reflex.Dom (DomBuilder)
import Control.Lens.TH
import Control.Monad.Reader (ReaderT)
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

type SFLayoutF m = forall a.(ReaderT (SimpleFormConfiguration m) m a -> ReaderT (SimpleFormConfiguration m) m a)

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
makeLenses ''CssConfiguration
makeLenses ''InputElementConfig
makeLenses ''SimpleFormConfiguration
