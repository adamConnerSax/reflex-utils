{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module Reflex.Dom.Contrib.SimpleForm.Builder
       (
         SimpleFormError(..)
       , SimpleFormErrors
       , DynValidation(..)
       , accValidation
       , avToMaybe
       , maybeToAV
       , dynValidationNothing
       , joinDynOfDynValidation
       , makeSimpleForm
       , makeSimpleForm'
       , observeDynamic
       , observeDynValidation
       , observeWidget
       , observeFlow
       , deriveSFRowBuilder
       , deriveSFColBuilder
       , SFRW
       , SimpleFormR(..)
       , CollapsibleInitialState(..)
       , SimpleFormBuilderFunctions(..)
       , SimpleFormLayoutFunctions(..)
       , SFLayoutF
       , runSimpleFormR
       , SimpleFormC
       , module ReflexExport
       , module BExport
       , module GSOP
       , liftF
       , liftLF
       , liftTransform
       , liftRAction
       , liftAction
       , switchingSFR
       , validItemStyle
       , invalidItemStyle
       , observerOnlyStyle
       , getFormType
       , LabelText
       , LabelPosition(..)
       , LabelConfig(..)
       , Placeholder
       , Title
       , InputConfig(..)
       , nullInputConfig
       , FormStyles(..)
       , FormType(..)
       , FormConfig(..)
       , fieldSet
       , itemL
       , itemR
       , formRow
       , formRow'
       , formCol
       , formCol'
       , attrs0
       , titleAttr
       , cssClassAttr
       , sfAttrs
       ) where


import           Reflex.Dom.Contrib.Layout.Types (CssClass, CssClasses (..),
                                                  IsCssClass (..),emptyCss,
                                                  LayoutDirection(..),LayoutOrientation(..))

import           DataBuilder                     as BExport (Builder (..),
                                                             FieldName,
                                                             GBuilder (..),
                                                             MDWrapped (..),
                                                             buildAFromConList)
import qualified DataBuilder                     as B
import           DataBuilder.GenericSOP          as GSOP (Generic,
                                                          HasDatatypeInfo,
                                                          deriveGeneric)

import           Reflex                          as ReflexExport (PushM)
import qualified Reflex                          as R
import qualified Reflex.Dom                      as RD

import           Control.Arrow                   ((&&&))
import           Control.Monad                   (join)
import           Control.Monad.Fix               (MonadFix)
import           Control.Monad.Morph
import           Control.Monad.Reader            (ReaderT, ask, runReaderT)
import qualified Data.Map                        as M
import           Data.Maybe                      (fromMaybe, isJust)
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T
import           Data.Validation                 (AccValidation (..))
import           Language.Haskell.TH


data SimpleFormError  = SFNothing | SFNoParse T.Text | SFInvalid T.Text deriving (Show,Eq)

type SimpleFormErrors = [SimpleFormError]

accValidation::(err->b)->(a->b)->AccValidation err a->b
accValidation f _ (AccFailure e) = f e
accValidation _ f (AccSuccess a) = f a

avToMaybe::AccValidation e a->Maybe a
avToMaybe (AccFailure _) = Nothing
avToMaybe (AccSuccess a) = Just a

maybeToAV::Maybe a->AccValidation SimpleFormErrors a
maybeToAV Nothing = AccFailure [SFNothing]
maybeToAV (Just a) = AccSuccess a

newtype DynValidation t a = DynValidation { unDynValidation::R.Dynamic t (AccValidation SimpleFormErrors a) }

dynValidationNothing::R.Reflex t=>DynValidation t a
dynValidationNothing = DynValidation $ R.constDyn (AccFailure [SFNothing])

dynValidationErr::R.Reflex t=>SimpleFormErrors->DynValidation t a
dynValidationErr = DynValidation . R.constDyn . AccFailure

joinDynOfDynValidation::R.Reflex t =>R.Dynamic t (DynValidation t a) -> DynValidation t a
joinDynOfDynValidation = DynValidation . join . fmap unDynValidation

instance R.Reflex t=>Functor (DynValidation t) where
  fmap f dva = DynValidation $ fmap (fmap f) (unDynValidation dva)

instance R.Reflex t=>Applicative (DynValidation t) where
  pure = DynValidation . R.constDyn . AccSuccess
  dvf <*> dva = DynValidation $ R.zipDynWith (<*>) (unDynValidation dvf) (unDynValidation dva)

-- no Monad instance because it would fail to satisfy <*> = `ap' due to that being impossible for AccValidation

type SFRW e t m a = ReaderT e m (DynValidation t a)

-- This is necessary because this functor and applicative are different from that of SFRW
newtype SimpleFormR e t m a = SimpleFormR { unSF::SFRW e t m a }

instance (R.Reflex t, R.MonadHold t m)=>Functor (SimpleFormR e t m) where
  fmap f sfra = SimpleFormR $ fmap (fmap f) (unSF sfra)

instance (R.Reflex t, R.MonadHold t m)=>Applicative (SimpleFormR e t m) where
  pure = SimpleFormR . return . pure
  sfrF <*> sfrA = SimpleFormR $ do
    dmF <- unSF sfrF
    dmA <- unSF sfrA
    return $ dmF <*> dmA

runSimpleFormR::Monad m=>e->SimpleFormR e t m a->m (DynValidation t a)
runSimpleFormR cfg sfra = runReaderT (unSF sfra) cfg

type SimpleFormC e t m = (RD.DomBuilder t m, R.MonadHold t m,
                          SimpleFormBuilderFunctions e t m,
                          SimpleFormLayoutFunctions e m)

switchingSFR::SimpleFormC e t m=>(a->SimpleFormR e t m b)->a->R.Event t a->SimpleFormR e t m b
switchingSFR widgetGetter widgetHolder0 newWidgetHolderEv = SimpleFormR $ do
  cfg <- ask
  let f = runSimpleFormR cfg . widgetGetter
  lift $ joinDynOfDynValidation <$> RD.widgetHold (f widgetHolder0) (fmap f newWidgetHolderEv)

asSimpleForm::RD.DomBuilder t m=>CssClass->m a->m a
asSimpleForm formClass = RD.elClass "form" (toCssString formClass)

asSimpleObserver::RD.DomBuilder t m=>CssClass->m a->m a
asSimpleObserver observerClass = RD.divClass (toCssString observerClass)


makeSimpleForm::(SimpleFormC e t m
                , B.Builder (SimpleFormR e t m) a)=>e->CssClass->Maybe a->m (DynValidation t a)
makeSimpleForm cfg formClass ma =
  asSimpleForm formClass $ runSimpleFormR cfg $ B.buildA Nothing ma

makeSimpleForm'::(SimpleFormC e t m, RD.MonadHold t m
                 , B.Builder (SimpleFormR e t m) a)=>
                 e-> -- form config
                 CssClass-> -- css class for form
                 Maybe a-> -- initial values
                 m (RD.Event t ())-> -- submit control
                 m (RD.Event t a)
makeSimpleForm' cfg formClass ma submitWidget = do
  dva <- unDynValidation <$> makeSimpleForm cfg formClass ma
  submitEv <- submitWidget
  return $ RD.attachPromptlyDynWithMaybe const (avToMaybe <$> dva) submitEv -- fires when control does but only if form entries are valid


observeDynamic::(SimpleFormC e t m,RD.PostBuild t m
                ,B.Builder (SimpleFormR e t m) a)=>e->CssClass->R.Dynamic t a->m (DynValidation t a)
observeDynamic cfg observerClass aDyn = observeDynValidation cfg observerClass $ DynValidation $ fmap AccSuccess aDyn

observeDynValidation::(SimpleFormC e t m,RD.PostBuild t m
                      ,B.Builder (SimpleFormR e t m) a)=>e->CssClass->DynValidation t a->m (DynValidation t a)
observeDynValidation cfg observerClass aDynM =
  asSimpleObserver observerClass $ runSimpleFormR cfg . SimpleFormR . setToObserve $ do
    let makeForm = accValidation (return . dynValidationErr) (unSF . buildA Nothing . Just)
        builtDyn = fmap makeForm (unDynValidation aDynM)  -- Dynamic t (ReaderT e m (DynValidation t a))
    newDynEv <- RD.dyn builtDyn -- Event t (DynValidation t a)
    lift $ joinDynOfDynValidation <$> R.holdDyn aDynM newDynEv --R.foldDyn (\_ x-> x) aDynM newDynEv -- DynValidation t a


observeWidget::(SimpleFormC e t m
               ,B.Builder (SimpleFormR e t m) a)=>e->CssClass->m a->m (DynValidation t a)
observeWidget cfg observerClass wa =
  asSimpleObserver observerClass $ runSimpleFormR cfg . SimpleFormR . setToObserve $ do
  a <- lift wa
  unSF . buildA Nothing . Just $ a


observeFlow::(SimpleFormC e t m, MonadFix m
             , B.Builder (SimpleFormR e t m) a
             , B.Builder (SimpleFormR e t m) b)=>e->CssClass->CssClass->(a->m b)->a->m (DynValidation t b)
observeFlow cfg formClass observerClass flow initialA = runSimpleFormR cfg . SimpleFormR  $ do
  let initialWidget = flow initialA
      obF = observeWidget cfg observerClass
  dva <- liftLF (asSimpleForm formClass) (unSF $ buildA Nothing (Just initialA)) -- DynValidation t a
  dwb <- lift $ R.foldDynMaybe (\ma _ -> flow <$> ma) initialWidget (avToMaybe <$> R.updated (unDynValidation dva)) -- Dynamic t (m b)
  lift $ joinDynOfDynValidation <$> RD.widgetHold (obF initialWidget) (obF <$> R.updated dwb)


type SFLayoutF e m a = ReaderT e m a -> ReaderT e m a
type DynAttrs t = R.Dynamic t (M.Map T.Text T.Text)

liftLF::Monad m=>(forall b.m b->m b)->SFLayoutF e m a
liftLF = hoist

liftF::(forall b.SFLayoutF e m b)->SimpleFormR e t m a->SimpleFormR e t m a
liftF f = SimpleFormR . f . unSF

liftTransform::Monad m=>(forall b.m b->m b)->SimpleFormR e t m a->SimpleFormR e t m a
liftTransform f = liftF (liftLF f)

liftRAction::Monad m=>ReaderT e m b->SimpleFormR e t m a->SimpleFormR e t m a
liftRAction ac sf = SimpleFormR $ ac >> unSF sf

liftAction::Monad m=>m b->SimpleFormR e t m a->SimpleFormR e t m a
liftAction ac = liftRAction (lift ac)

data CollapsibleInitialState = CollapsibleStartsOpen | CollapsibleStartsClosed deriving (Show,Eq,Ord,Enum,Bounded)

-- | class to hold form configuration.  For different configurations, declare an env type and then
-- | instantiate the class for that type.
-- TODO: Should this all just be a data type (record-of-functions)?
class SimpleFormBuilderFunctions e t m where
  failureF::T.Text->SimpleFormR e t m a
  sumF::[(B.ConName,SimpleFormR e t m a)]->Maybe B.ConName->SimpleFormR e t m a
  dynamicDiv::DynAttrs t->SFLayoutF e m a

type Placeholder = T.Text
type Title = T.Text
type LabelText = T.Text
data LabelPosition = LabelBefore | LabelAfter

-- do the attributes below need to be dynamic?  That would complicate things... 
data LabelConfig = LabelConfig { labelPosition::LabelPosition, labelText::LabelText, labelAttrs::M.Map T.Text T.Text }
data FormType = Interactive | ObserveOnly deriving (Eq)

data InputConfig = InputConfig { placeHolder::Maybe Placeholder, title::Maybe Title, labelConfig::Maybe LabelConfig }
data FormStyles = FormStyles { valid::CssClasses, invalid::CssClasses, observeOnly::CssClasses }
data FormConfig = FormConfig { styles::FormStyles, formType::FormType } -- need a way to add a class to wrapping div

nullInputConfig::InputConfig
nullInputConfig = InputConfig Nothing Nothing Nothing

-- presumably, an instance of this will have fields for the configs and the sets will be done via Control.Monad.Reader.local

class Monad m=>SimpleFormLayoutFunctions e m where
  formItem::SFLayoutF e m a
  layoutOrientation::LayoutOrientation->SFLayoutF e m a
  layoutFill::LayoutDirection->SFLayoutF e m a
  layoutCentered::LayoutOrientation->SFLayoutF e m a
  layoutCollapsible::T.Text->CollapsibleInitialState->SFLayoutF e m a

  getFormConfig::ReaderT e m FormConfig
  setFormConfig::FormConfig->SFLayoutF e m a  

  getInputConfig::ReaderT e m InputConfig
  setInputConfig::InputConfig->SFLayoutF e m a

validItemStyle::SimpleFormLayoutFunctions e m=>ReaderT e m CssClasses
validItemStyle = valid . styles <$> getFormConfig

invalidItemStyle::SimpleFormLayoutFunctions e m=>ReaderT e m CssClasses
invalidItemStyle = invalid . styles <$> getFormConfig

observerOnlyStyle::SimpleFormLayoutFunctions e m=>ReaderT e m CssClasses
observerOnlyStyle = observeOnly . styles <$> getFormConfig

getFormType::SimpleFormLayoutFunctions e m=>ReaderT e m FormType
getFormType = formType <$> getFormConfig

setToObserve::SimpleFormLayoutFunctions e m=>SFLayoutF e m a
setToObserve w = do
  fc <- getFormConfig
  setFormConfig fc{ formType = ObserveOnly }  w

fieldSet::SimpleFormC e t m=>T.Text->SFLayoutF e m a
fieldSet legendText ra = RD.el "fieldset" $ do
    lift $ RD.el "legend" $ RD.text legendText
    ra

itemL::SimpleFormLayoutFunctions e m=>SFLayoutF e m a
itemL = layoutFill LayoutRight  . formItem

itemR::SimpleFormLayoutFunctions e m=>SFLayoutF e m a
itemR = layoutFill LayoutLeft . formItem

formRow::SimpleFormLayoutFunctions e m=>SFLayoutF e m a
formRow  = formItem . layoutOrientation LayoutHorizontal

formCol::SimpleFormLayoutFunctions e m=>SFLayoutF e m a
formCol = formItem . layoutOrientation LayoutVertical

formRow'::(RD.Reflex t, SimpleFormLayoutFunctions e m,SimpleFormBuilderFunctions e t m)=>DynAttrs t->SFLayoutF e m a
formRow' attrsDyn  = formItem . dynamicDiv attrsDyn . layoutOrientation LayoutHorizontal

formCol'::(RD.Reflex t,SimpleFormLayoutFunctions e m, SimpleFormBuilderFunctions e t m)=>DynAttrs t->SFLayoutF e m a
formCol' attrsDyn = formItem . dynamicDiv attrsDyn .layoutOrientation LayoutVertical


attrs0::R.Reflex t=>DynAttrs t
attrs0 = R.constDyn mempty

titleAttr::T.Text->M.Map T.Text T.Text
titleAttr x = M.fromList [("title",x),("placeholder",x)]

cssClassAttr::CssClasses->M.Map T.Text T.Text
cssClassAttr x = "class" RD.=: toCssString x

sfAttrs::(RD.MonadHold t m, R.Reflex t, SimpleFormLayoutFunctions e m)
         =>DynValidation t a->Maybe FieldName->Maybe T.Text->ReaderT e m (R.Dynamic t (M.Map T.Text T.Text))
sfAttrs mDyn mFN mTypeS = sfAttrs' mDyn mFN mTypeS (CssClasses [])

sfAttrs'::(RD.MonadHold t m, R.Reflex t, SimpleFormLayoutFunctions e m)
         =>DynValidation t a->Maybe FieldName->Maybe T.Text->CssClasses->ReaderT e m (R.Dynamic t (M.Map T.Text T.Text))

sfAttrs' mDyn mFN mTypeS fixedCss = do
  validClasses <- validItemStyle
  invalidClasses <- invalidItemStyle
  observerClasses <- observerOnlyStyle
  fType <- formType <$> getFormConfig 
  let title = componentTitle mFN mTypeS
      validAttrs = titleAttr title <> cssClassAttr (validClasses <> fixedCss)
      invalidAttrs = titleAttr title <> cssClassAttr (invalidClasses <> fixedCss)
      observerAttr = titleAttr title <> cssClassAttr (observerClasses <> fixedCss)
  lift  $ case fType of
         ObserveOnly -> return $ R.constDyn observerAttr
         Interactive -> return . RD.ffor (unDynValidation mDyn) $ \x -> case x of
                                                             (AccSuccess _)-> validAttrs
                                                             (AccFailure _)->invalidAttrs


componentTitle::Maybe FieldName->Maybe T.Text->T.Text
componentTitle mFN mType =
  let fnS = maybe "" T.pack  mFN
      tnS = fromMaybe "" mType
  in if isJust mFN && isJust mType then fnS <> "::" <> tnS else fnS <> tnS


instance (R.Reflex t,R.MonadHold t m,SimpleFormBuilderFunctions e t m) => B.Buildable (SimpleFormR e t m) where
  -- the rest of the instances are handled by defaults since SimpleFormR is Applicative
  bFail = failureF . T.pack
  bSum mwWidgets = SimpleFormR $ do
    let constrList = map ((fst . B.metadata) &&& B.value) mwWidgets
        defCon = case filter B.hasDefault mwWidgets of
          [] -> Nothing
          (x:_) -> Just . fst $ B.metadata x
    unSF $ sumF constrList defCon


deriveSFRowBuilder::Name -> Q [Dec]
deriveSFRowBuilder typeName =
  [d|instance SimpleFormC e t m=>Builder (SimpleFormR e t m) $(conT typeName) where
       buildA md Nothing  = liftF (itemL . layoutHoriz) ($(B.handleNothingL typeName) md)
       buildA md (Just x) = liftF (itemL . layoutHoriz) ($(B.handleJustL typeName) md x)|]


deriveSFColBuilder::Name -> Q [Dec]
deriveSFColBuilder typeName =
  [d|instance SimpleFormC e t m=>Builder (SimpleFormR e t m) $(conT typeName) where
       buildA md Nothing  = liftF (itemL . layoutVert) ($(B.handleNothingL typeName) md)
       buildA md (Just x) = liftF (itemL . layoutVert) ($(B.handleJustL typeName) md x)|]

