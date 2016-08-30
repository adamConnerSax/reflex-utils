{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Contrib.SimpleForm.Builder
       (
         DynMaybe
       , makeSimpleForm
       , observeDynamic
       , observeDynMaybe
       , observeWidget
       , observeFlow
       , deriveSFRowBuilder
       , deriveSFColBuilder
       , SFRW
       , SimpleFormR(..)
       , CollapsibleInitialState(..)
       , SimpleFormConfiguration(..)
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
       , layoutFieldNameHelper
       , textAtLeft
       , textOnTop
       , textOnTop'
       , legend
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

import Reflex.Dom.Contrib.Layout.Types (CssClass,CssClasses(..),IsCssClass(..))
--import Reflex.Dom.Contrib.Layout.LayoutM()

import qualified DataBuilder as B
import DataBuilder as BExport (Builder(..),GBuilder(..),FieldName,MDWrapped(..),buildAFromConList)
import DataBuilder.GenericSOP as GSOP (Generic,HasDatatypeInfo,deriveGeneric)
import DataBuilder.TH (deriveBuilder)

import qualified Reflex as R
import Reflex as ReflexExport (PushM)
import qualified Reflex.Dom as RD

import Control.Monad.Reader (ReaderT, runReaderT, ask) --, lift,local)
import Control.Monad.Morph
import Data.Maybe (isJust,fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Map as M
import Language.Haskell.TH



newtype DynMaybe t a = DynMaybe { unDynMaybe::R.Dynamic t (Maybe a) }
instance R.Reflex t=>Functor (DynMaybe t) where
  fmap f dma = DynMaybe $ fmap (fmap f) (unDynMaybe dma)  

instance R.Reflex t =>Applicative (DynMaybe t) where
  pure x = DynMaybe $ R.constDyn (Just x)
  dmf <*> dma = DynMaybe $ R.zipDynWith (<*>) (unDynMaybe dmf) (unDynMaybe dma)

instance R.Reflex t=>Monad (DynMaybe t) where
  return = pure
  dma >>= f =  DynMaybe$ do
    ma <- unDynMaybe dma
    unDynMaybe $ maybe Nothing f ma
  
type SFRW e t m a = ReaderT e m (DynMaybe t a)

-- This is necessary because this functor and applicative are different from that of SFRW
newtype SimpleFormR e t m a = SimpleFormR { unSF::SFRW e t m a }

instance (R.Reflex t, R.MonadHold t m)=>Functor (SimpleFormR e t m) where
  fmap f sfra = SimpleFormR $ (unSF sfra) >>= lift . R.mapDyn (fmap f) 

instance (R.Reflex t, R.MonadHold t m)=>Applicative (SimpleFormR e t m) where
  pure x = SimpleFormR $ return (R.constDyn (Just x))
  sfrF <*> sfrA = SimpleFormR $ do
    dmF <- unSF sfrF
    dmA <- unSF sfrA
    lift $ R.combineDyn (<*>) dmF dmA

runSimpleFormR::Monad m=>e->SimpleFormR e t m a->m (DynMaybe t a)
runSimpleFormR cfg sfra = runReaderT (unSF sfra) cfg

type SimpleFormC e t m = (RD.MonadWidget t m,SimpleFormConfiguration e t m)

switchingSFR::SimpleFormC e t m=>(a->SimpleFormR e t m b)->a->R.Event t a->SimpleFormR e t m b
switchingSFR widgetGetter widgetHolder0 newWidgetHolderEv = SimpleFormR $ do
  cfg <- ask
  let f = runSimpleFormR cfg . widgetGetter
  lift $ R.joinDyn <$> RD.widgetHold (f widgetHolder0) (fmap f newWidgetHolderEv)  

asSimpleForm::RD.MonadWidget t m=>CssClass->m a->m a
asSimpleForm formClass = RD.elClass "form" (toCssString formClass)

asSimpleObserver::RD.MonadWidget t m=>CssClass->m a->m a
asSimpleObserver observerClass = RD.divClass (toCssString observerClass)


makeSimpleForm::(SimpleFormC e t m,B.Builder (SimpleFormR e t m) a)=>e->CssClass->Maybe a->m (DynMaybe t a)
makeSimpleForm cfg formClass ma = 
  asSimpleForm formClass $ runSimpleFormR cfg $ B.buildA Nothing ma

observeDynamic::(SimpleFormC e t m,B.Builder (SimpleFormR e t m) a)=>e->CssClass->R.Dynamic t a->m (DynMaybe t a)
observeDynamic cfg observerClass aDyn = R.mapDyn Just aDyn >>= observeDynMaybe cfg observerClass

observeDynMaybe::(SimpleFormC e t m,B.Builder (SimpleFormR e t m) a)=>e->CssClass->DynMaybe t a->m (DynMaybe t a)
observeDynMaybe cfg observerClass aDynM =
  asSimpleObserver observerClass $ runSimpleFormR cfg . SimpleFormR . setToObserve $ do
    builtDyn <- R.mapDyn (maybe (return $ R.constDyn Nothing) (unSF . buildA Nothing . Just)) aDynM -- Dynamic t (ReaderT e m (DynMaybe t a))
    newDynEv <- RD.dyn builtDyn -- Event t (DynMaybe t a)
    lift $ R.joinDyn <$> R.holdDyn aDynM newDynEv --R.foldDyn (\_ x-> x) aDynM newDynEv -- DynMaybe t a


observeWidget::(SimpleFormC e t m,B.Builder (SimpleFormR e t m) a)=>e->CssClass->m a->m (DynMaybe t a)
observeWidget cfg observerClass wa =
  asSimpleObserver observerClass $ runSimpleFormR cfg . SimpleFormR . setToObserve $ do
  a <- lift wa
  unSF . buildA Nothing . Just $ a


observeFlow::(SimpleFormC e t m,B.Builder (SimpleFormR e t m) a,B.Builder (SimpleFormR e t m) b)=>e->CssClass->CssClass->(a->m b)->a->m (DynMaybe t b)
observeFlow cfg formClass observerClass f a = runSimpleFormR cfg . SimpleFormR  $ do
  let initialWidget = f a
      obF = observeWidget cfg observerClass
  dma <- liftLF (asSimpleForm formClass) (unSF $ buildA Nothing (Just a)) -- DynMaybe t a
  dwb <- lift $ R.foldDynMaybe (\ma _ -> f <$> ma) initialWidget (R.updated dma) -- Dynamic t (m b)
  lift $ R.joinDyn <$> RD.widgetHold (obF initialWidget) (obF <$> R.updated dwb)
    

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
class SimpleFormConfiguration e t m | m->t  where
  failureF::String->SimpleFormR e t m a
  sumF::[(B.ConName,SimpleFormR e t m a)]->Maybe B.ConName->SimpleFormR e t m a
  formItem::SFLayoutF e m a
  dynamicDiv::DynAttrs t->SFLayoutF e m a
  layoutVert::SFLayoutF e m a
  layoutHoriz::SFLayoutF e m a
  layoutL::SFLayoutF e m a
  layoutR::SFLayoutF e m a
  layoutHC::SFLayoutF e m a
  layoutT::SFLayoutF e m a
  layoutB::SFLayoutF e m a
  layoutVC::SFLayoutF e m a
  layoutCollapsible::T.Text->CollapsibleInitialState->SFLayoutF e m a
  validItemStyle::ReaderT e m CssClasses
  invalidItemStyle::ReaderT e m CssClasses
  observerStyle::ReaderT e m CssClasses
  observer::ReaderT e m Bool
  setToObserve::SFLayoutF e m a
  setLayoutFieldName::Maybe (T.Text -> T.Text)->SFLayoutF e m a
  getLayoutFieldName::ReaderT e m (Maybe (T.Text -> SFLayoutF e m a)) -- Nothing is ignored, otherwise do the layout

layoutFieldNameHelper::SimpleFormC e t m=>Maybe FieldName->SFLayoutF e m a
layoutFieldNameHelper mFN sfra = do
  mf <- getLayoutFieldName
  let lf = fromMaybe id $ mf <*> (T.pack <$> mFN)
  lf sfra

textAtLeft::SimpleFormC e t m=>T.Text->SFLayoutF e m a
textAtLeft label ra = formRow $ do
  formItem $ RD.el "span" $ RD.text label
  formItem ra

textOnTop::SimpleFormC e t m=>T.Text->SFLayoutF e m a
textOnTop = textOnTop' id 

textOnTop'::SimpleFormC e t m=>SFLayoutF e m ()->T.Text->SFLayoutF e m a
textOnTop' labelLayout label ra = formCol $ do
  labelLayout . formItem $ RD.el "span" $ RD.text label
  formItem ra

legend::SimpleFormC e t m=>T.Text->SFLayoutF e m a
legend legend ra = RD.el "fieldset" $ do
    lift $ RD.el "legend" $ RD.text legend
    ra

{-
labelTop::SimpleFormC e t m=>String->SFLayoutF e m a
labelTop label ra = do
  labelClasses <- labelStyle 
  layoutVert $ do
    formItem . lift $ RD.elClass "div" (toCssString labelClasses) $ RD.text label
    ra
-}

itemL::SimpleFormConfiguration e t m=>SFLayoutF e m a
itemL = layoutL . formItem

itemR::SimpleFormConfiguration e t m=>SFLayoutF e m a
itemR = layoutR . formItem 

formRow::SimpleFormConfiguration e t m=>SFLayoutF e m a
formRow  = formItem . layoutHoriz

formCol::SimpleFormConfiguration e t m=>SFLayoutF e m a
formCol = formItem . layoutVert

formRow'::SimpleFormConfiguration e t m=>DynAttrs t->SFLayoutF e m a
formRow' attrsDyn  = formItem . dynamicDiv attrsDyn . layoutHoriz

formCol'::SimpleFormConfiguration e t m=>DynAttrs t->SFLayoutF e m a
formCol' attrsDyn = formItem . dynamicDiv attrsDyn .layoutVert

{-
disabledAttr::(Monad m,SimpleFormConfiguration e t m)=>ReaderT e m (M.Map String String)
disabledAttr = do
  disabled <- inputsDisabled
  return $ if disabled then ("disabled" RD.=: "") else mempty
-}
attrs0::R.Reflex t=>DynAttrs t
attrs0 = R.constDyn mempty

titleAttr::T.Text->M.Map T.Text T.Text
titleAttr x = ("title" RD.=: x)

cssClassAttr::CssClasses->M.Map T.Text T.Text
cssClassAttr x = ("class" RD.=: toCssString x)

sfAttrs::(RD.MonadHold t m, R.Reflex t, SimpleFormConfiguration e t m)
         =>DynMaybe t a->Maybe FieldName->Maybe T.Text->ReaderT e m (R.Dynamic t (M.Map T.Text T.Text))
sfAttrs mDyn mFN mTypeS = sfAttrs' mDyn mFN mTypeS (CssClasses [])

sfAttrs'::(RD.MonadHold t m, R.Reflex t, SimpleFormConfiguration e t m)
         =>DynMaybe t a->Maybe FieldName->Maybe T.Text->CssClasses->ReaderT e m (R.Dynamic t (M.Map T.Text T.Text))
sfAttrs' mDyn mFN mTypeS fixedCss = do
  validClasses <- validItemStyle
  invalidClasses <- invalidItemStyle
  observerClasses <- observerStyle
  isObserver <- observer
  let title = componentTitle mFN mTypeS
      validAttrs = titleAttr title <> cssClassAttr (validClasses <> fixedCss)
      invalidAttrs = titleAttr title <> cssClassAttr (invalidClasses <> fixedCss)
      observerAttr = titleAttr title <> cssClassAttr (observerClasses <> fixedCss)
  lift $ if isObserver
         then return $ R.constDyn observerAttr
         else R.forDyn mDyn $ \x -> if isJust x
                                    then validAttrs
                                    else invalidAttrs 


componentTitle::Maybe FieldName->Maybe T.Text->T.Text
componentTitle mFN mType =
  let fnS = maybe "" T.pack  mFN
      tnS = maybe "" id  mType
  in if (isJust mFN && isJust mType) then (fnS <> "::" <> tnS) else (fnS <> tnS)


instance SimpleFormC e t m => B.Buildable (SimpleFormR e t m) where
  -- the rest of the instances are handled by defaults since SimpleFormR is Applicative
  bFail = failureF
  bSum mwWidgets = SimpleFormR $ do
    let constrList = map (\mdw -> (fst . B.metadata $ mdw, B.value mdw)) mwWidgets
        defCon = case filter B.hasDefault mwWidgets of
          [] -> Nothing
          (x:_) -> Just . fst $ B.metadata x
    unSF $ sumF constrList defCon 


deriveSFRowBuilder::Name -> Q [Dec]
deriveSFRowBuilder typeName = do
  [d|instance SimpleFormC e t m=>Builder (SimpleFormR e t m) $(conT typeName) where
       buildA md Nothing  = liftF (itemL . layoutHoriz) ($(B.handleNothingL typeName) md)
       buildA md (Just x) = liftF (itemL . layoutHoriz) ($(B.handleJustL typeName) md x)|]


deriveSFColBuilder::Name -> Q [Dec]
deriveSFColBuilder typeName = do
  [d|instance SimpleFormC e t m=>Builder (SimpleFormR e t m) $(conT typeName) where
       buildA md Nothing  = liftF (itemL . layoutVert) ($(B.handleNothingL typeName) md)
       buildA md (Just x) = liftF (itemL . layoutVert) ($(B.handleJustL typeName) md x)|]

