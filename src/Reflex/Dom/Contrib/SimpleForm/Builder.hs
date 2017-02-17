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
       , SimpleFormR
       , makeSimpleFormR
       , unSF
       , BuilderC
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

type BuilderC t m a = (B.Builder (SFR t m) (DynValidation t) a)
{-
instance (R.Reflex t, Functor m)=>Functor (SimpleFormR t m) where
  fmap f sfra = SimpleFormR $ fmap (fmap f) (unSF sfra)

instance (R.Reflex t, R.MonadHold t m)=>Applicative (SimpleFormR t m) where
  pure = SimpleFormR . return . pure
  sfrF <*> sfrA = SimpleFormR $ do
    dmF <- unSF sfrF
    dmA <- unSF sfrA
    return $ dmF <*> dmA

instance (R.Reflex t, R.MonadHold t m)=>Monad (SimpleFormR t m) where
  return = pure
  ma >>= f = SimpleFormR $ do -- f::a->SimpleFormR t m b, we want DynValidation t a ->
    cfg <- ask
    let sfrma = unSF ma -- ReaderT (SimpleFormConfiguration t m) (DynValidation t a)
        f'  = fmap . fmap . f -- f'::ReaderT (SimpleFormConfiguration t m) (DynValidation t a) -> ReaderT (SimpleFormConfiguration t m) (DynValidation t (SimpleFormR t m b))
    dvsfb <- unSF <$> f' sfrma -- DynValidation t (ReaderT (SimpleFormConfiguration t m) (DynValidation t b))
    x <- traverse dvsfb -- (DynValidation t (DynValidation t b))

    let dvmdvb = fmap (flip runReaderT cfg) dvsfb -- DynValidation t (m (DynValidation t b))


    (fmap $ (foldDyn dyn) . traverse . unDynValidation $ unSF ma >>= f' -- AccValidation $ (m (Event t (Dynamic t (AccValidation b))))


instance (Monad m, R.Reflex t)=>MonadReader (SimpleFormConfiguration t m) (SimpleFormR t m) where
  ask = SimpleFormR $ do
    cfg <- ask
    return (pure cfg)
  local f = liftF (local f)
-}


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


makeSimpleForm::(SimpleFormC t m, BuilderC t m a)=>SimpleFormConfiguration t m->Maybe a->m (DynValidation t a)
makeSimpleForm cfg ma = runSimpleFormR cfg $ B.buildA Nothing ma

makeSimpleForm'::(SimpleFormC t m, RD.MonadHold t m
                 , BuilderC t m a)=>
                 SimpleFormConfiguration t m->
                 Maybe a-> -- initial values
                 m (RD.Event t ())-> -- submit control
                 m (RD.Event t a)
makeSimpleForm' cfg ma submitWidget = do
  let f dva = do
        submitEv <- submitWidget
        return $ RD.attachPromptlyDynWithMaybe const (avToMaybe <$> (unDynValidation dva)) submitEv -- fires when control does but only if form entries are valid
  runSimpleFormR' cfg (B.buildA Nothing ma) f


observeDynamic::(SimpleFormC t m, RD.PostBuild t m
                ,BuilderC t m a)=>SimpleFormConfiguration t m->R.Dynamic t a->m (DynValidation t a)
observeDynamic cfg aDyn = observeDynValidation cfg $ DynValidation $ fmap AccSuccess aDyn

observeDynValidation::(SimpleFormC t m,RD.PostBuild t m
                      ,BuilderC t m a)=>SimpleFormConfiguration t m->DynValidation t a->m (DynValidation t a)
observeDynValidation cfg aDynM =
  runSimpleFormR (setToObserve cfg) . makeSimpleFormR  $ do
    let makeForm = accValidation (return . dynValidationErr) (unSF . B.buildA Nothing . Just)
        builtDyn = fmap makeForm (unDynValidation aDynM)  -- Dynamic t (ReaderT e m (DynValidation t a))
    newDynEv <- RD.dyn builtDyn -- Event t (DynValidation t a)
    lift $ joinDynOfDynValidation <$> R.holdDyn aDynM newDynEv --R.foldDyn (\_ x-> x) aDynM newDynEv -- DynValidation t a


observeWidget::(SimpleFormC t m ,BuilderC t m a)=>SimpleFormConfiguration t m->m a->m (DynValidation t a)
observeWidget cfg wa =
  runSimpleFormR (setToObserve cfg) . makeSimpleFormR $ lift wa >>= unSF . B.buildA Nothing . Just



observeFlow::(SimpleFormC t m, MonadFix m
             , BuilderC t m a
             , BuilderC t m b)=>SimpleFormConfiguration t m->(a->m b)->a->m (DynValidation t b)
observeFlow cfg flow initialA =
  runSimpleFormR cfg . makeSimpleFormR  $ do
    let initialWidget = flow initialA
        obF = observeWidget cfg
    dva <- (unSF $ B.buildA Nothing (Just initialA)) -- DynValidation t a
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


instance (R.Reflex t, R.MonadHold t m)=> B.Buildable (SFR t m) (DynValidation t) where
  bFail msg = makeSimpleFormR $ do
    failF <- failureF . _builderFunctions <$> ask
    failF $ T.pack msg

  bSum mwWidgets = makeSimpleFormR $ do
    let constrList = map ((fst . B.metadata) &&& (unSF . B.value)) mwWidgets
        defCon = case filter B.hasDefault mwWidgets of
          [] -> Nothing
          (x:_) -> Just . fst $ B.metadata x
    sF <- sumF . _builderFunctions <$> ask
    sF constrList defCon

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
