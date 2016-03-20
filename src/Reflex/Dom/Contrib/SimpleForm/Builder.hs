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
module Reflex.Dom.Contrib.SimpleForm.Builder
       (
         DynMaybe
       , makeSimpleForm
       , deriveSFRowBuilder
       , deriveSFColBuilder
       , SFRW
       , SimpleFormR(..)
       , runSimpleFormR
       , liftF
       , liftLF
       , liftTransform
       , switchingSFR
       , SimpleFormConfiguration(..)
       , itemL
       , itemR
       , formRow
       , formRow'
       , formCol
       , formCol'
       , buttonClass
       , attrs0
       , titleAttr
       , cssClassAttr
       , sfAttrs
       , sfAttrs'
       , SimpleFormC
       , module ReflexExport
       , module BExport
       , module GSOP
       ) where

import Control.Monad (liftM2)
import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.Morph
import Data.Maybe (fromJust,isJust)
import Data.Monoid ((<>))
import qualified Data.Map as M
import Language.Haskell.TH

import qualified Reflex as R
import Reflex as ReflexExport (PushM)
import qualified Reflex.Dom as RD

import Reflex.Dom.Contrib.Layout.Types (LayoutM,CssClasses(..),IsCssClass(..))
import Reflex.Dom.Contrib.Layout.Core()
--import Reflex.Orphans()

import qualified DataBuilder as B
import DataBuilder as BExport (Builder(..),GBuilder(..),FieldName)
import DataBuilder.GenericSOP as GSOP (Generic,HasDatatypeInfo,deriveGeneric)
import DataBuilder.TH (deriveBuilder)

type DynMaybe t a = R.Dynamic t (Maybe a)
type SFRW e t m a = ReaderT e m (DynMaybe t a)

data Orientation = Row | Column

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
  

makeSimpleForm::(SimpleFormC e t m,B.Builder (SimpleFormR e t m) a)=>e->Maybe a->m (DynMaybe t a)
makeSimpleForm cfg ma = runSimpleFormR cfg $ B.buildA Nothing ma

type SFLayoutF e m a = ReaderT e m a -> ReaderT e m a
type DynAttrs t = R.Dynamic t (M.Map String String)
liftLF::Monad m=>(forall b.m b->m b)->SFLayoutF e m a
liftLF = hoist

liftF::(forall b.SFLayoutF e m b)->SimpleFormR e t m a->SimpleFormR e t m a
liftF f = SimpleFormR . f . unSF

liftTransform::Monad m=>(forall b.m b->m b)->SimpleFormR e t m a->SimpleFormR e t m a
liftTransform f = liftF (liftLF f)

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
  validItemStyle::ReaderT e m CssClasses
  invalidItemStyle::ReaderT e m CssClasses
  buttonStyle::ReaderT e m CssClasses
  dropdownStyle::ReaderT e m CssClasses

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

buttonClass::RD.MonadWidget t m=>String->String->m (R.Event t ())
buttonClass label cssClass = do
  (e,_) <- RD.elAttr' "button" ("class" RD.=: cssClass) $ RD.text label
  return $ RD.domEvent RD.Click e 

attrs0::R.Reflex t=>DynAttrs t
attrs0 = R.constDyn mempty

titleAttr::String->M.Map String String
titleAttr x = ("title" RD.=: x)

cssClassAttr::CssClasses->M.Map String String
cssClassAttr x = ("class" RD.=: toCssString x)

sfAttrs::(RD.MonadHold t m, R.Reflex t, SimpleFormConfiguration e t m)
         =>DynMaybe t a->Maybe FieldName->Maybe String->ReaderT e m (R.Dynamic t (M.Map String String))
sfAttrs mDyn mFN mTypeS = sfAttrs' mDyn mFN mTypeS (CssClasses [])

sfAttrs'::(RD.MonadHold t m, R.Reflex t, SimpleFormConfiguration e t m)
         =>DynMaybe t a->Maybe FieldName->Maybe String->CssClasses->ReaderT e m (R.Dynamic t (M.Map String String))
sfAttrs' mDyn mFN mTypeS fixedCss = do
  validClasses <- validItemStyle
  invalidClasses <- invalidItemStyle
  let title = componentTitle mFN mTypeS
      validAttrs = (titleAttr title <> cssClassAttr (validClasses <> fixedCss))
      invalidAttrs = (titleAttr title <> cssClassAttr (invalidClasses <> fixedCss))
  lift $ R.mapDyn (\x -> if isJust x then validAttrs else invalidAttrs) mDyn


componentTitle::Maybe FieldName->Maybe String->String
componentTitle mFN mType =
  let fnS = maybe "" id  mFN
      tnS = maybe "" id  mType
  in if (isJust mFN && isJust mType) then fnS ++ "::" ++ tnS else fnS ++ tnS

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

