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
{-# LANGUAGE DefaultSignatures      #-}
module Reflex.Dom.Contrib.SimpleForm.Builder
       (
         module Reflex.Dom.Contrib.SimpleForm.DynValidation
       , module Reflex.Dom.Contrib.SimpleForm.Configuration
       , makeSimpleForm
       , makeSimpleForm'
       , observeDynamic
--       , observeDynValidation
       , observeWidget
       , observeFlow
       , SimpleFormR
       , FormValidator
       , validateForm
       , FormBuilder(..)
       , makeSimpleFormR
       , unSF
       , VFormBuilderC
       , SimpleFormC
       , CollapsibleInitialState(..)
       , runSimpleFormR
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
--       , deriveSFRowBuilder
--       , deriveSFColBuilder
       ) where


import           Reflex.Dom.Contrib.Layout.Types             (CssClass,
                                                              CssClasses (..),
                                                              IsCssClass (..),
                                                              LayoutOrientation (..),
                                                              emptyCss)

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
import           Control.Lens                                (view)
import           Control.Monad                               (join)
import           Control.Monad.Fix                           (MonadFix)
import           Control.Monad.Morph
import           Control.Monad.Reader                        (MonadReader (..),
                                                              runReaderT)
import           Data.Functor.Compose                        (Compose (..))
import qualified Data.Map                                    as M
import           Data.Maybe                                  (fromMaybe, isJust)
import           Data.Monoid                                 ((<>))
import qualified Data.Text                                   as T
import           Data.Validation                             (AccValidation (..))
import           Language.Haskell.TH




-- This is necessary because this functor and applicative are different from that of SFRW
type SimpleFormR t m a = Compose (SFR t m) (DynValidation t) a --SimpleFormR { unSF::SFRW t m a }

makeSimpleFormR::SFRW t m a -> SimpleFormR t m a
makeSimpleFormR = Compose

unSF::SimpleFormR t m a->SFRW t m a
unSF = getCompose

type BuilderC t m a = (B.Builder (SFR t m) (R.Dynamic t) (SFValidation) a, B.Validatable (SFValidation) a)

type FormValidator a = B.Validator (SFValidation) a

validateForm::(Functor m, R.Reflex t)=>FormValidator a->SimpleFormR t m a->SimpleFormR t m a
validateForm va = makeSimpleFormR . fmap DynValidation . B.unFGV . B.validateFGV va . B.FGV . fmap unDynValidation . unSF   


class (SimpleFormC t m, RD.PostBuild t m) => FormBuilder t m a where
  buildForm::FormValidator a->Maybe FieldName->Maybe (R.Dynamic t a)->SimpleFormR t m a
  default buildForm::(GBuilder (SFR t m) (R.Dynamic t) (SFValidation) a)
                   =>FormValidator a->Maybe FieldName->Maybe (R.Dynamic t a)->SimpleFormR t m a
  buildForm va mFN = makeSimpleFormR . fmap DynValidation . B.unFGV . gBuildValidated va mFN

type VFormBuilderC t m a = (FormBuilder t m a, B.Validatable (SFValidation) a)

buildForm'::VFormBuilderC t m a=>Maybe FieldName->Maybe (R.Dynamic t a)->SimpleFormR t m a
buildForm' = buildForm B.validator

instance (SimpleFormC t m,FormBuilder t m a)=>B.Builder (SFR t m) (R.Dynamic t) (SFValidation) a where
  buildValidated va mFN = B.FGV . fmap unDynValidation . unSF . buildForm va mFN

runSimpleFormR::Monad m=>SimpleFormConfiguration t m->SimpleFormR t m a->m (DynValidation t a)
runSimpleFormR cfg sfra = runSimpleFormR' cfg sfra return

runSimpleFormR'::Monad m=>SimpleFormConfiguration t m->SimpleFormR t m a->(DynValidation t a->m b)->m b
runSimpleFormR' cfg sfra f = runReaderT (sfWrapper $ unSF sfra >>= lift . f) cfg

type SimpleFormC t m = (RD.DomBuilder t m, R.MonadHold t m)

switchingSFR::SimpleFormC t m=>(a->SimpleFormR t m b)->a->R.Event t a->SimpleFormR t m b
switchingSFR widgetGetter widgetHolder0 newWidgetHolderEv = makeSimpleFormR $ do
  cfg <- ask
  let f = runSimpleFormR cfg . widgetGetter
  lift $ joinDynOfDynValidation <$> RD.widgetHold (f widgetHolder0) (fmap f newWidgetHolderEv)


makeSimpleForm::(SimpleFormC t m, VFormBuilderC t m a)=>SimpleFormConfiguration t m->Maybe a->m (DynValidation t a)
makeSimpleForm cfg ma = runSimpleFormR cfg $ buildForm' Nothing (R.constDyn <$> ma)


--TODO: is attachPromptlyDynWithMaybe the right thing here?
makeSimpleForm'::(SimpleFormC t m, VFormBuilderC t m a)
               =>SimpleFormConfiguration t m
               -> Maybe a -- initial values
               -> m (RD.Event t ()) -- submit control
               -> m (RD.Event t a)
makeSimpleForm' cfg ma submitWidget = do
  let f dva = do
        submitEv <- submitWidget
        return $ RD.attachPromptlyDynWithMaybe const (avToMaybe <$> (unDynValidation dva)) submitEv -- fires when control does but only if form entries are valid
  runSimpleFormR' cfg (buildForm' Nothing (R.constDyn <$> ma)) f


observeDynamic::(SimpleFormC t m, VFormBuilderC t m a)=>SimpleFormConfiguration t m->R.Dynamic t a->m (DynValidation t a)
observeDynamic cfg  = runSimpleFormR (setToObserve cfg) . buildForm' Nothing . Just 

{-
observeDynValidation::(SimpleFormC t m,RD.PostBuild t m
                      ,BuilderC t m a)=>SimpleFormConfiguration t m->DynValidation t a->m (DynValidation t a)
observeDynValidation cfg aDynM =
  runSimpleFormR (setToObserve cfg) . makeSimpleFormR $ accValidation (return . dynValidationErr) (unSF . buildForm' Nothing . Just) (unDynValidation aDynM)
-}

{-
observeDynValidation cfg aDynM =
  runSimpleFormR (setToObserve cfg) . makeSimpleFormR  $ do
    let makeForm = accValidation (return . dynValidationErr) (unSF . B.buildA Nothing . Just)
        builtDyn = fmap makeForm (unDynValidation aDynM)  -- Dynamic t (ReaderT e m (DynValidation t a))
    newDynEv <- RD.dyn builtDyn -- Event t (DynValidation t a)
    lift $ joinDynOfDynValidation <$> R.holdDyn aDynM newDynEv --R.foldDyn (\_ x-> x) aDynM newDynEv -- DynValidation t a
-}

observeWidget::(SimpleFormC t m ,VFormBuilderC t m a)=>SimpleFormConfiguration t m->m a->m (DynValidation t a)
observeWidget cfg wa =
  runSimpleFormR (setToObserve cfg) . makeSimpleFormR $ lift wa >>= unSF . buildForm' Nothing . Just . R.constDyn


observeFlow::(SimpleFormC t m
             , MonadFix m
             , VFormBuilderC t m a
             , VFormBuilderC t m b)
           =>SimpleFormConfiguration t m->(a->m b)->a->m (DynValidation t b)
observeFlow cfg flow initialA =
  runSimpleFormR cfg . makeSimpleFormR  $ do
    let initialWidget = flow initialA
        obF = observeWidget cfg
    dva <- (unSF $ buildForm' Nothing (Just $ R.constDyn initialA)) -- DynValidation t a
    dwb <- lift $ R.foldDynMaybe (\ma _ -> flow <$> ma) initialWidget (avToMaybe <$> R.updated (unDynValidation dva)) -- Dynamic t (m b)
    lift $ joinDynOfDynValidation <$> RD.widgetHold (obF initialWidget) (obF <$> R.updated dwb)

liftF::SFLayoutF t m->SimpleFormR t m a->SimpleFormR t m a
liftF f = makeSimpleFormR . f . unSF

liftTransform::Monad m=>(forall b.m b->m b)->SimpleFormR t m a->SimpleFormR t m a
liftTransform f = liftF (liftLF f)

liftRAction::Monad m=>SFR t m b->SimpleFormR t m a->SimpleFormR t m a
liftRAction ac sf = makeSimpleFormR $ ac >> unSF sf

liftAction::Monad m=>m b->SimpleFormR t m a->SimpleFormR t m a
liftAction ac = liftRAction (lift ac)

sfRowDynAttr::Monad m=>DynAttrs t->SFLayoutF t m
sfRowDynAttr attrsDyn = sfItem . sfDynamicDiv attrsDyn . sfOrient LayoutHorizontal

sfColDynAttr::Monad m=>DynAttrs t->SFLayoutF t m
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

sfAttrs::(RD.MonadHold t m, RD.DomBuilder t m,MonadFix m)
         =>DynValidation t a->Maybe FieldName->Maybe T.Text->SFR t m (R.Dynamic t (M.Map T.Text T.Text))
sfAttrs mDyn mFN mTypeS = sfAttrs' mDyn mFN mTypeS (CssClasses [])

sfAttrs'::(RD.MonadHold t m, R.Reflex t,MonadFix m)
         =>DynValidation t a->Maybe FieldName->Maybe T.Text->CssClasses->SFR t m (R.Dynamic t (M.Map T.Text T.Text))
sfAttrs' mDyn mFN mTypeS fixedCss = do
  validClasses <- validDataClasses
  invalidClasses <- invalidDataClasses
  let title = componentTitle mFN mTypeS
      validAttrs = titleAttr title <> cssClassAttr (validClasses <> fixedCss)
      invalidAttrs = titleAttr title <> cssClassAttr (invalidClasses <> fixedCss)
      f (AccSuccess _) = True
      f (AccFailure _) = False
      validDyn = f <$> (unDynValidation mDyn)
  return . RD.ffor validDyn $ \x -> if x then validAttrs else invalidAttrs

componentTitle::Maybe FieldName->Maybe T.Text->T.Text
componentTitle mFN mType =
  let fnS = maybe "" T.pack  mFN
      tnS = fromMaybe "" mType
  in if isJust mFN && isJust mType then fnS <> "::" <> tnS else fnS <> tnS


instance (SimpleFormC t m, RD.PostBuild t m)=> B.Buildable (SFR t m) (R.Dynamic t) (SFValidation) where
  bFail msg = B.FGV . fmap unDynValidation $ do
    failF <- failureF . _builderFunctions <$> ask
    failF $ T.pack msg

  bSum mwWidgets = B.FGV . fmap unDynValidation $ do
    let constrList = map ((fst . B.metadata) &&& (fmap DynValidation . B.unFGV . B.value)) mwWidgets
        defCon = case filter B.hasDefault mwWidgets of
          [] -> Nothing
          (x:_) -> Just . fst $ B.metadata x
    sF <- sumF . _builderFunctions <$> ask
    sF constrList defCon

  bDistributeList = R.distributeListOverDynPure

  bCollapse dynFGV = B.FGV $ do
    postbuild <- RD.getPostBuild
    let uncomposed = B.unFGV <$> dynFGV
        newWidgetEv = R.updated uncomposed
        startingWidgetEv = R.attachWith const (R.current uncomposed) postbuild
    join <$> RD.widgetHold (return $ R.constDyn $ AccFailure [SFNothing]) (R.leftmost [startingWidgetEv, newWidgetEv])
    

{-
  bCollapse dynFGV = FGV $ do
    let uncomposed = unFGV <$> dynFGV -- Dynamic t (SFR t m (Dynamic t (SFValidation a)))
    newInputDynEv <- RD.dyn uncomposed -- Event t (Dynamic t (SFValidation a))
    newInputBeh <- R.hold R.never (R.updated <$> dyned) -- Behavior t (Event t (SFValidation a))
    let newInputEv = R.switch newInputBeh -- Event t (SFValidation a)
    R.holdDyn (AccFailure [SFNothing]) newInputEv 
-}   
    
{-
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
