{-# LANGUAGE RankNTypes #-}  
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
module Reflex.Dom.Contrib.SimpleForm.Configuration where

import Reflex.Dom.Contrib.Layout.Types (CssClasses,LayoutOrientation,LayoutDirection)
import Reflex (MonadHold)
import Reflex.Dom (DomBuilder)
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

--data InputConfig = InputConfig { placeHolder::Maybe Placeholder, title::Maybe Title, labelConfig::Maybe LabelConfig }
--data FormStyles = FormStyles { item::CssClasses, validInput::CssClasses, invalidInput::CssClasses, observeOnly::CssClasses }
--data FormConfig = FormConfig { styles::FormStyles, formType::FormType } -- need a way to add a class to wrapping div

--nullInputConfig::InputConfig
--nullInputConfig = InputConfig Nothing Nothing Nothing

type SFLayoutFC t m = (DomBuilder t m, MonadHold t m)
type BaseLayoutF = forall a t m.SFLayoutFC t m=>m a->m a
type SFLayoutF = forall a t m.(SFLayoutFC t m=>ReaderT SimpleFormConfiguration m a -> ReaderT SimpleFormConfiguration m a)

liftLF::BaseLayoutF->SFLayoutF
liftLF = hoist


data LayoutConfiguration = LayoutConfiguration
  {
    formItem::SFLayoutF
  , layoutOriented::LayoutOrientation->SFLayoutF
  , layoutFill::LayoutDirection->SFLayoutF
  , layoutCentered::LayoutOrientation->SFLayoutF
  , layoutCollapsible::Text -> SFLayoutF
  }

data CssConfiguration = StyleConfiguration
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
  , _imputLabelConfig::Maybe LabelConfig
  }

data SimpleFormConfiguration = SimpleFormConfiguration
  {
    _formType::FormType
  , _layoutConfig::LayoutConfiguration
  , _cssConfig::CssConfiguration
  , _inputConfig::InputElementConfig
  }


--makeLenses ''LayoutConfiguration
makeLenses ''CssConfiguration
makeLenses ''InputElementConfig
makeLenses ''SimpleFormConfiguration
