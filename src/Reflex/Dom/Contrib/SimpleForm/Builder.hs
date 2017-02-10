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
         module Reflex.Dom.Contrib.SimpleForm.DynValidation
       , module Reflex.Dom.Contrib.SimpleForm.Configuration
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
       , formItemStyle
       , validInputStyle
       , invalidInputStyle
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
                                                  IsCssClass (..),
                                                  LayoutDirection (..),
                                                  LayoutOrientation (..),
                                                  emptyCss)

import Reflex.Dom.Contrib.SimpleForm.DynValidation
import Reflex.Dom.Contrib.SimpleForm.Configuration

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

import           Language.Haskell.TH


type SFRW t m a = ReaderT SimpleFormConfiguration m (DynValidation t a)

-- This is necessary because this functor and applicative are different from that of SFRW
newtype SimpleFormR t m a = SimpleFormR { unSF::SFRW t m a }

instance (R.Reflex t, R.MonadHold t m)=>Functor (SimpleFormR t m) where
  fmap f sfra = SimpleFormR $ fmap (fmap f) (unSF sfra)

instance (R.Reflex t, R.MonadHold t m)=>Applicative (SimpleFormR t m) where
  pure = SimpleFormR . return . pure
  sfrF <*> sfrA = SimpleFormR $ do
    dmF <- unSF sfrF
    dmA <- unSF sfrA
    return $ dmF <*> dmA

runSimpleFormR::Monad m=>SimpleFormConfiguration->SimpleFormR t m a->m (DynValidation t a)
runSimpleFormR cfg sfra = runReaderT (unSF sfra) cfg

type SimpleFormC t m = (RD.DomBuilder t m, R.MonadHold t m, SimpleFormBuilderFunctions t m)

switchingSFR::SimpleFormC t m=>(a->SimpleFormR t m b)->a->R.Event t a->SimpleFormR t m b
switchingSFR widgetGetter widgetHolder0 newWidgetHolderEv = SimpleFormR $ do
  cfg <- ask
  let f = runSimpleFormR cfg . widgetGetter
  lift $ joinDynOfDynValidation <$> RD.widgetHold (f widgetHolder0) (fmap f newWidgetHolderEv)

asSimpleForm::RD.DomBuilder t m=>CssClass->m a->m a
asSimpleForm formClass = RD.elClass "form" (toCssString formClass)

asSimpleObserver::RD.DomBuilder t m=>CssClass->m a->m a
asSimpleObserver observerClass = RD.divClass (toCssString observerClass)


makeSimpleForm::(SimpleFormC t m, B.Builder (SimpleFormR t m) a)=>SimpleFormConfiguration->CssClass->Maybe a->m (DynValidation t a)
makeSimpleForm cfg formClass ma = asSimpleForm formClass $ runSimpleFormR cfg $ B.buildA Nothing ma

makeSimpleForm'::(SimpleFormC t m, RD.MonadHold t m
                 , B.Builder (SimpleFormR t m) a)=>
                 SimpleFormConfiguration->
                 CssClass-> -- css class for form
                 Maybe a-> -- initial values
                 m (RD.Event t ())-> -- submit control
                 m (RD.Event t a)
makeSimpleForm' cfg formClass ma submitWidget = do
  dva <- unDynValidation <$> makeSimpleForm cfg formClass ma
  submitEv <- submitWidget
  return $ RD.attachPromptlyDynWithMaybe const (avToMaybe <$> dva) submitEv -- fires when control does but only if form entries are valid


observeDynamic::(SimpleFormC t m, RD.PostBuild t m
                ,B.Builder (SimpleFormR t m) a)=>SimpleFormConfiguration->CssClass->R.Dynamic t a->m (DynValidation t a)
observeDynamic cfg observerClass aDyn = observeDynValidation cfg observerClass $ DynValidation $ fmap AccSuccess aDyn

observeDynValidation::(SimpleFormC t m,RD.PostBuild t m
                      ,B.Builder (SimpleFormR t m) a)=>SimpleFormConfiguration->CssClass->DynValidation t a->m (DynValidation t a)
observeDynValidation cfg observerClass aDynM =
  asSimpleObserver observerClass $ runSimpleFormR cfg . SimpleFormR . setToObserve $ do
    let makeForm = accValidation (return . dynValidationErr) (unSF . buildA Nothing . Just)
        builtDyn = fmap makeForm (unDynValidation aDynM)  -- Dynamic t (ReaderT e m (DynValidation t a))
    newDynEv <- RD.dyn builtDyn -- Event t (DynValidation t a)
    lift $ joinDynOfDynValidation <$> R.holdDyn aDynM newDynEv --R.foldDyn (\_ x-> x) aDynM newDynEv -- DynValidation t a


observeWidget::(SimpleFormC t m
               ,B.Builder (SimpleFormR t m) a)=>SimpleFormConfiguration->CssClass->m a->m (DynValidation t a)
observeWidget cfg observerClass wa =
  asSimpleObserver observerClass $ runSimpleFormR cfg . SimpleFormR . setToObserve $ do
  a <- lift wa
  unSF . buildA Nothing . Just $ a


observeFlow::(SimpleFormC t m, MonadFix m
             , B.Builder (SimpleFormR t m) a
             , B.Builder (SimpleFormR t m) b)=>SimpleFormConfiguration->CssClass->CssClass->(a->m b)->a->m (DynValidation t b)
observeFlow cfg formClass observerClass flow initialA = runSimpleFormR cfg . SimpleFormR  $ do
  let initialWidget = flow initialA
      obF = observeWidget cfg observerClass
  dva <- liftLF (asSimpleForm formClass) (unSF $ buildA Nothing (Just initialA)) -- DynValidation t a
  dwb <- lift $ R.foldDynMaybe (\ma _ -> flow <$> ma) initialWidget (avToMaybe <$> R.updated (unDynValidation dva)) -- Dynamic t (m b)
  lift $ joinDynOfDynValidation <$> RD.widgetHold (obF initialWidget) (obF <$> R.updated dwb)


type DynAttrs t = R.Dynamic t (M.Map T.Text T.Text)

liftF::SFLayoutF->SimpleFormR t m a->SimpleFormR t m a
liftF f = SimpleFormR . f . unSF

liftTransform::BaseLayoutF->SimpleFormR t m a->SimpleFormR t m a
liftTransform f = liftF (liftLF f)

liftRAction::Monad m=>ReaderT SimpleFormConfiguration m b->SimpleFormR t m a->SimpleFormR t m a
liftRAction ac sf = SimpleFormR $ ac >> unSF sf

liftAction::Monad m=>m b->SimpleFormR t m a->SimpleFormR t m a
liftAction ac = liftRAction (lift ac)

data CollapsibleInitialState = CollapsibleStartsOpen | CollapsibleStartsClosed deriving (Show,Eq,Ord,Enum,Bounded)

-- | class to hold form configuration.  For different configurations, declare an env type and then
-- | instantiate the class for that type.
-- TODO: Should this all just be a data type (record-of-functions)?
class SimpleFormBuilderFunctions t m | m -> t where
  failureF::T.Text->SimpleFormR t m a
  sumF::[(B.ConName,SimpleFormR t m a)]->Maybe B.ConName->SimpleFormR t m a
--  dynamicDiv::DynAttrs t->SFLayoutF


{-
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
-}


formItemStyle::ReaderT SimpleFormConfiguration m CssClasses
formItemStyle = view (cssConfig . cssAllItems) 

validInputStyle::ReaderT SimpleFormConfiguration  m CssClasses
validInputStyle = view (cssConfig . cssValidInput)

invalidInputStyle::ReaderT SimpleFormConfiguration m CssClasses
invalidInputStyle = view (cssConfig . cssInvalidInput) 

observerOnlyStyle::ReaderT SimpleFormConfiguration m CssClasses
observerOnlyStyle = view (cssConfig . cssReadOnly)

getFormType::ReaderT SimpleFormConfiguration m FormType
getFormType = view formType

setToObserve::SFLayoutF
setToObserve w = local (\fc -> fc {_formType = ObserveOnly }) w
--  fc <- getFormConfig
--  setFormConfig fc{ formType = ObserveOnly }  w

fieldSet::T.Text->SFLayoutF
fieldSet legendText ra = RD.el "fieldset" $ do
    lift $ RD.el "legend" $ RD.text legendText
    ra

itemL::SFLayoutF
itemL = do
  cfg <- ask
  let itemF = _layoutConfig . formItem $ cfg
      fillF = _layoutConfig . layoutFill$ cfg
  fillF LayoutRight  . itemF

{-
itemR::SimpleFormConfiguration->SFLayoutF
itemR cfg =
  let itemF = _layoutConfig . formItem $ cfg
      fillF = _layoutConfig . layoutFill
  fillF LayoutLeft . itemF

formRow::SimpleFormConfiguration->SFLayoutF
formRow  cfg =
  let itemF = _layoutConfig . formItem $ cfg
      orientF = _layoutConfig . layoutOrientation
  itemF . orientF LayoutHorizontal

formCol::SimpleFormConfiguration->SFLayoutF
formCol cfg =
  let itemF = _layoutConfig . formItem $ cfg
      orientF = _layoutConfig . layoutOrientation
  itemF . orientF LayoutVertical

-- I'm here

formRow'::(RD.Reflex t, SimpleFormBuilderFunctions e t m)=>DynAttrs t->SFLayoutF
formRow' attrsDyn  =
  
  formItem . dynamicDiv attrsDyn . layoutOrientation LayoutHorizontal

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
  validClasses <- validInputStyle
  invalidClasses <- invalidInputStyle
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

-}
