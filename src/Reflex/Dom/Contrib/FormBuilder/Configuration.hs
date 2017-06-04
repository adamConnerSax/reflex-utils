{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
--{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Reflex.Dom.Contrib.FormBuilder.Configuration
  (
    Placeholder
  , Title
  , LabelText
  , LabelPosition(..)
  , LabelConfig(..)
  , FormType(..)
  , CollapsibleInitialState(..)
  , FR
  , FormResult
  , formResultErrors
  , formResultError
  , formResultNothing
  , FRW
  , FLayoutF
  , liftLF
  , DynAttrs
  , LayoutConfiguration(..)
  , CssConfiguration(..)
  , InputElementConfig(..)
  , BuilderFunctions(..)
  , FormConfiguration(..)
  , FormIncludedCss(..)
  , FConfigChanger
  , HasCssConfiguration(..)
  , HasInputElementConfig(..)
  , HasFormConfiguration(..)
  , setInputConfig
  , wrapperClasses
  , itemClasses
  , inputClasses
  , validDataClasses
  , invalidDataClasses
  , setToObserve
  , getFC
  , getFormType
  , fItemL
  , fItemR
  , fRow
  , fCol
  , fWrapper
  , fItem
  , fCenter
  , fOrient
  , fFill
  , fCollapsible
  , fDynamicDiv
  ) where

import           Reflex.Dom.Contrib.FormBuilder.DynValidation
import           Reflex.Dom.Contrib.Layout.Types              (CssClasses, LayoutDirection (..),
                                                               LayoutOrientation (..))
import           Reflex.Dom.Contrib.Widgets.WidgetResult      (WrappedWidgetResult,
                                                               constWidgetResult)

import qualified DataBuilder                                  as B
import           Reflex                                       (Dynamic, Event,
                                                               Reflex)

import           Control.Lens.TH
import           Control.Monad.Morph                          (hoist)
import           Control.Monad.Reader                         (ReaderT, ask,
                                                               local)
import           Data.ByteString                              (ByteString)
import           Data.Functor.Compose                         (Compose (Compose))
import           Data.Map                                     (Map)
import           Data.Text                                    (Text)
import           Reflex.Dom.Contrib.CssUtils                  (CssLinks)


type Placeholder = Text
type Title = Text
type LabelText = Text
data LabelPosition = LabelBefore | LabelAfter

-- do the attributes below need to be dynamic?  That would complicate things...
data LabelConfig = LabelConfig { labelText :: LabelText, labelAttrs :: Map Text Text }
data FormType = Interactive | ObserveOnly deriving (Eq)

data CollapsibleInitialState = CollapsibleStartsOpen | CollapsibleStartsClosed deriving (Show,Eq,Ord,Enum,Bounded)

type FR t m = ReaderT (FormConfiguration t m) m
type FormResult t = WrappedWidgetResult t FValidation

formResultErrors :: Reflex t => FormErrors -> FormResult t a
formResultErrors = Compose . constWidgetResult . AccFailure

formResultError :: Reflex t => FormError -> FormResult t a
formResultError = formResultErrors . pure

formResultNothing :: Reflex t => FormResult t a
formResultNothing = formResultError FNothing

type FRW t m a = FR t m (FormResult t a)

type FLayoutF t m = forall a.(FR t m a -> FR t m a)

liftLF :: Monad m => (forall a. m a -> m a) -> FLayoutF t m
liftLF = hoist

type DynAttrs t = Dynamic t (Map Text Text)

data LayoutConfiguration t m = LayoutConfiguration
  {
    layoutWrapper :: FormType -> FLayoutF t m
  , layoutItem :: FormType -> FLayoutF t m
  , layoutOriented :: FormType -> LayoutOrientation -> FLayoutF t m
  , layoutFill:: FormType -> LayoutDirection -> FLayoutF t m
  , layoutCentered :: FormType -> LayoutOrientation -> FLayoutF t m
  , layoutCollapsible :: FormType -> Text -> CollapsibleInitialState -> FLayoutF t m
  }

data CssConfiguration = CssConfiguration
  {
    _cssWrapper     :: FormType -> CssClasses
  , _cssAllItems    :: FormType -> CssClasses
  , _cssAllInputs   :: FormType -> CssClasses
  , _cssValidData   :: FormType -> CssClasses
  , _cssInvalidData :: FormType -> CssClasses -- ??
  }

data InputElementConfig t = InputElementConfig
  {
    _inputPlaceholder :: Maybe Placeholder
  , _inputTitle       :: Maybe Title
  , _inputLabelConfig :: Maybe LabelConfig
  }

data BuilderFunctions t m = BuilderFunctions
  {
    failureF   :: forall a. Text -> FRW t m a
  , sumF       :: forall a. [(B.ConName, Maybe Text, Event t (), FRW t m a)] -> FRW t m a
--  , chooserF::Dynamic t [B.ConName]->ConName->FR t m (Dynamic t B.ConName)
  , dynamicDiv :: DynAttrs t -> FLayoutF t m
  }

data FormConfiguration t m = FormConfiguration
  {
    _formType         :: FormType
  , _builderFunctions :: BuilderFunctions t m
  , _layoutConfig     :: LayoutConfiguration t m
  , _cssConfig        :: CssConfiguration
  , _inputConfig      :: InputElementConfig t
  }

type FConfigChanger t m = FormConfiguration t m -> FormConfiguration t m

data FormIncludedCss = FormIncludedCss { cssToEmbed :: ByteString, cssToLink :: CssLinks }

{-
type Validator a = a -> Either Text a

data BuilderConfiguration t m a = BuilderConfiguration { formConfig::FormConfiguration t m, validationF::Validator a}
-}

makeClassy ''CssConfiguration
makeClassy ''InputElementConfig
makeClassy ''FormConfiguration


-- these functions allow direct use from within the reader

setInputConfig :: Monad m => InputElementConfig t -> FLayoutF t m
setInputConfig ic = let
  f (FormConfiguration ft bf lc cc _) = FormConfiguration ft bf lc cc ic
  in local f

getFC :: Monad m => FR t m (FormConfiguration t m)
getFC = ask

getClasses :: Monad m => (CssConfiguration -> (FormType -> CssClasses)) -> FR t m CssClasses
getClasses selector = do
  fType <- _formType <$> getFC
  cssF <- (selector . _cssConfig) <$> ask
  return $ cssF fType

wrapperClasses :: Monad m => FR t m CssClasses
wrapperClasses = getClasses _cssWrapper

itemClasses :: Monad m => FR t m CssClasses
itemClasses = getClasses _cssAllItems

inputClasses :: Monad m => FR t m CssClasses
inputClasses = getClasses _cssAllInputs

validDataClasses :: Monad m => FR t m CssClasses
validDataClasses = getClasses _cssValidData

invalidDataClasses :: Monad m => FR t m CssClasses
invalidDataClasses = getClasses _cssInvalidData

getFormType :: Monad m => FR t m FormType
getFormType = _formType <$> getFC

setToObserve :: FormConfiguration t m -> FormConfiguration t m
setToObserve cfg = cfg {_formType = ObserveOnly }

fWrapper :: Monad m => FLayoutF t m
fWrapper ra = do
  fType <- _formType <$> getFC
  f <- layoutWrapper . _layoutConfig <$> ask
  f fType ra

fItem :: Monad m => FLayoutF t m
fItem ra = do
  fType <- _formType <$> getFC
  f <- layoutItem . _layoutConfig <$> ask
  f fType ra

fCenter :: Monad m => LayoutOrientation -> FLayoutF t m
fCenter o ra = do
  fType <- _formType <$> getFC
  f <- layoutCentered . _layoutConfig <$> ask
  f fType o ra

fFill :: Monad m => LayoutDirection -> FLayoutF t m
fFill d ra = do
  fType <- _formType <$> getFC
  f <- layoutFill . _layoutConfig <$> ask
  f fType d ra

fOrient :: Monad m => LayoutOrientation -> FLayoutF t m
fOrient o ra = do
  fType <- _formType <$> getFC
  f <- layoutOriented . _layoutConfig <$> getFC
  f fType o ra

fCollapsible :: Monad m => Text -> CollapsibleInitialState -> FLayoutF t m
fCollapsible t is ra = do
  fType <- _formType <$> getFC
  f <- layoutCollapsible . _layoutConfig <$> getFC
  f fType t is ra

fItemL :: Monad m => FLayoutF t m
fItemL = fFill LayoutRight . fItem

fItemR :: Monad m => FLayoutF t m
fItemR = fFill LayoutLeft . fItem

fRow :: Monad m => FLayoutF t m
fRow  = fItem . fOrient LayoutHorizontal

fCol :: Monad m => FLayoutF t m
fCol = fItem . fOrient LayoutVertical

fDynamicDiv :: Monad m => DynAttrs t -> FLayoutF t m
fDynamicDiv dynAttrs ra = do
  f <- dynamicDiv . _builderFunctions <$> getFC
  f dynAttrs ra
