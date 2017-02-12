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
       , SimpleFormR(..)
       , CollapsibleInitialState(..)
       , runSimpleFormR
       , SimpleFormC
       , module ReflexExport
       , module BExport
       , module GSOP
       , liftF
       , liftTransform
       , liftRAction
       , liftAction
       , switchingSFR
       , fieldSet
       , sfRowDynAttr
       , sfColDynAttr
       , attrs0
       , titleAttr
       , cssClassAttr
       , sfAttrs
       ) where


import           Reflex.Dom.Contrib.Layout.Types             (CssClass,
                                                              CssClasses (..),
                                                              IsCssClass (..),
                                                              LayoutOrientation (..))

import           Reflex.Dom.Contrib.SimpleForm.Configuration
import           Reflex.Dom.Contrib.SimpleForm.DynValidation

import           DataBuilder                                 as BExport (Builder (..),
                                                                         FieldName,
                                                                         GBuilder (..),
                                                                         MDWrapped (..),
                                                                         buildAFromConList)
import qualified DataBuilder                                 as B
import           DataBuilder.GenericSOP                      as GSOP (Generic, HasDatatypeInfo,
                                                                      deriveGeneric)

import           Reflex                                      as ReflexExport (PushM)
import qualified Reflex                                      as R
import qualified Reflex.Dom                                  as RD

import           Control.Arrow                               ((&&&))
import           Control.Monad.Fix                           (MonadFix)
import           Control.Monad.Morph
import           Control.Monad.Reader                        (ask, runReaderT)
import qualified Data.Map                                    as M
import           Data.Maybe                                  (fromMaybe, isJust)
import           Data.Monoid                                 ((<>))
import qualified Data.Text                                   as T
import           Data.Validation                             (AccValidation (..))
import           Language.Haskell.TH




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

--askSimpleFormR::SimpleFormR t m (SimpleFormConfiguration m)

runSimpleFormR::Monad m=>SimpleFormConfiguration t m->SimpleFormR t m a->m (DynValidation t a)
runSimpleFormR cfg sfra = runReaderT (unSF sfra) cfg

type SimpleFormC t m = (RD.DomBuilder t m, R.MonadHold t m {- , SimpleFormBuilderFunctions t m -})

switchingSFR::SimpleFormC t m=>(a->SimpleFormR t m b)->a->R.Event t a->SimpleFormR t m b
switchingSFR widgetGetter widgetHolder0 newWidgetHolderEv = SimpleFormR $ do
  cfg <- ask
  let f = runSimpleFormR cfg . widgetGetter
  lift $ joinDynOfDynValidation <$> RD.widgetHold (f widgetHolder0) (fmap f newWidgetHolderEv)

asSimpleForm::RD.DomBuilder t m=>CssClasses->m a->m a
asSimpleForm formClasses = RD.elClass "form" (toCssString formClasses)

asSimpleObserver::RD.DomBuilder t m=>CssClasses->m a->m a
asSimpleObserver observerClass = RD.divClass (toCssString observerClass)


makeSimpleForm::(SimpleFormC t m, B.Builder (SimpleFormR t m) a)=>SimpleFormConfiguration t m->Maybe a->m (DynValidation t a)
makeSimpleForm cfg ma =
  let formClasses = _cssForm . _cssConfig $ cfg
  in asSimpleForm formClasses $ runSimpleFormR cfg $ B.buildA Nothing ma

makeSimpleForm'::(SimpleFormC t m, RD.MonadHold t m
                 , B.Builder (SimpleFormR t m) a)=>
                 SimpleFormConfiguration t m->
                 Maybe a-> -- initial values
                 m (RD.Event t ())-> -- submit control
                 m (RD.Event t a)
makeSimpleForm' cfg ma submitWidget = do
  dva <- unDynValidation <$> makeSimpleForm cfg ma
  submitEv <- submitWidget
  return $ RD.attachPromptlyDynWithMaybe const (avToMaybe <$> dva) submitEv -- fires when control does but only if form entries are valid


observeDynamic::(SimpleFormC t m, RD.PostBuild t m
                ,B.Builder (SimpleFormR t m) a)=>SimpleFormConfiguration t m->R.Dynamic t a->m (DynValidation t a)
observeDynamic cfg aDyn = observeDynValidation cfg $ DynValidation $ fmap AccSuccess aDyn

observeDynValidation::(SimpleFormC t m,RD.PostBuild t m
                      ,B.Builder (SimpleFormR t m) a)=>SimpleFormConfiguration t m->DynValidation t a->m (DynValidation t a)
observeDynValidation cfg aDynM =
  let observerClasses = _cssObserver . _cssConfig $ cfg
  in asSimpleObserver observerClasses $ runSimpleFormR cfg . SimpleFormR . setToObserve $ do
    let makeForm = accValidation (return . dynValidationErr) (unSF . buildA Nothing . Just)
        builtDyn = fmap makeForm (unDynValidation aDynM)  -- Dynamic t (ReaderT e m (DynValidation t a))
    newDynEv <- RD.dyn builtDyn -- Event t (DynValidation t a)
    lift $ joinDynOfDynValidation <$> R.holdDyn aDynM newDynEv --R.foldDyn (\_ x-> x) aDynM newDynEv -- DynValidation t a


observeWidget::(SimpleFormC t m
               ,B.Builder (SimpleFormR t m) a)=>SimpleFormConfiguration t m->m a->m (DynValidation t a)
observeWidget cfg wa =
  let observerClasses = _cssObserver . _cssConfig $ cfg
  in asSimpleObserver observerClasses $ runSimpleFormR cfg . SimpleFormR . setToObserve $ do
    a <- lift wa
    unSF . buildA Nothing . Just $ a


observeFlow::(SimpleFormC t m, MonadFix m
             , B.Builder (SimpleFormR t m) a
             , B.Builder (SimpleFormR t m) b)=>SimpleFormConfiguration t m->(a->m b)->a->m (DynValidation t b)
observeFlow cfg flow initialA =
  let formClasses = _cssForm . _cssConfig $ cfg
  in runSimpleFormR cfg . SimpleFormR  $ do
    let initialWidget = flow initialA
        obF = observeWidget cfg
    dva <- liftLF (asSimpleForm formClasses) (unSF $ buildA Nothing (Just initialA)) -- DynValidation t a
    dwb <- lift $ R.foldDynMaybe (\ma _ -> flow <$> ma) initialWidget (avToMaybe <$> R.updated (unDynValidation dva)) -- Dynamic t (m b)
    lift $ joinDynOfDynValidation <$> RD.widgetHold (obF initialWidget) (obF <$> R.updated dwb)

liftF::SFLayoutF t m->SimpleFormR t m a->SimpleFormR t m a
liftF f = SimpleFormR . f . unSF

liftTransform::Monad m=>(forall b.m b->m b)->SimpleFormR t m a->SimpleFormR t m a
liftTransform f = liftF (liftLF f)

liftRAction::Monad m=>SFR t m b->SimpleFormR t m a->SimpleFormR t m a
liftRAction ac sf = SimpleFormR $ ac >> unSF sf

liftAction::Monad m=>m b->SimpleFormR t m a->SimpleFormR t m a
liftAction ac = liftRAction (lift ac)

{-
class SimpleFormBuilderFunctions t m  where
  failureF::T.Text->SimpleFormR t m a
  sumF::[(B.ConName,SimpleFormR t m a)]->Maybe B.ConName->SimpleFormR t m a
  dynamicDiv::DynAttrs t->SFLayoutF m
-}

sfRowDynAttr::Monad m=>{- SimpleFormBuilderFunctions t m=> -}DynAttrs t->SFLayoutF t m
sfRowDynAttr attrsDyn = sfItem . sfDynamicDiv attrsDyn . sfOrient LayoutHorizontal

sfColDynAttr::Monad m=>{- SimpleFormBuilderFunctions t m=>-}DynAttrs t->SFLayoutF t m
sfColDynAttr attrsDyn =   sfItem . sfDynamicDiv attrsDyn . sfOrient LayoutHorizontal

fieldSet::RD.DomBuilder t m=>T.Text->SFLayoutF t m
fieldSet legendText ra = RD.el "fieldset" $ do
    lift $ RD.el "legend" $ RD.text legendText
    ra

attrs0::R.Reflex t=>DynAttrs t
attrs0 = R.constDyn mempty

titleAttr::T.Text->M.Map T.Text T.Text
titleAttr x = M.fromList [("title",x),("placeholder",x)]

cssClassAttr::CssClasses->M.Map T.Text T.Text
cssClassAttr x = "class" RD.=: toCssString x

sfAttrs::(RD.MonadHold t m, RD.DomBuilder t m)
         =>DynValidation t a->Maybe FieldName->Maybe T.Text->SFR t m (R.Dynamic t (M.Map T.Text T.Text))
sfAttrs mDyn mFN mTypeS = sfAttrs' mDyn mFN mTypeS (CssClasses [])

sfAttrs'::(RD.MonadHold t m, R.Reflex t)
         =>DynValidation t a->Maybe FieldName->Maybe T.Text->CssClasses->SFR t m (R.Dynamic t (M.Map T.Text T.Text))
sfAttrs' mDyn mFN mTypeS fixedCss = do
  validClasses <- validInputStyle
  invalidClasses <- invalidInputStyle
  observerClasses <- observerOnlyStyle
  fType <- getFormType
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


instance (R.Reflex t, R.MonadHold t m {-, SimpleFormBuilderFunctions t m -}) => B.Buildable (SimpleFormR t m) where
  bFail msg = SimpleFormR $ do
    failF <- failureF . _builderFunctions <$> ask
    failF $ T.pack msg

  bSum mwWidgets = SimpleFormR $ do
    let constrList = map ((fst . B.metadata) &&& (unSF . B.value)) mwWidgets
        defCon = case filter B.hasDefault mwWidgets of
          [] -> Nothing
          (x:_) -> Just . fst $ B.metadata x
    sF <- sumF . _builderFunctions <$> ask
    sF constrList defCon


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

