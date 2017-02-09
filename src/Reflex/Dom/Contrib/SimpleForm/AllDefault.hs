{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Contrib.SimpleForm.AllDefault
       (
         DefSFCfg(..)
       , simpleFormDefaultCss
       , simpleObserverDefaultCss
         -- these two can be re-used in other env types
       , defFailureF 
       , defSumF
       ) where

-- | A useful default config for quick form building
-- | Also serves as an example for building others
-- | and defFailureF and defSumF can be re-used for other implementations
import Reflex.Dom.Contrib.ReflexConstraints (MonadWidgetExtraC)
import Reflex.Dom.Contrib.Layout.Types (CssClasses(..),LayoutDirection(..),LayoutOrientation(..))
import Reflex.Dom.Contrib.Layout.FlexLayout (flexCol,flexRow,flexItem,
                                             flexFill,flexCenter)

import Reflex.Dom.Contrib.SimpleForm.Builder
import Reflex.Dom.Contrib.SimpleForm.Instances(sfWidget)
import Reflex.Dom.Contrib.SimpleForm.Instances.Basic(SimpleFormInstanceC)

import qualified DataBuilder as B

import qualified Reflex as R 
import qualified Reflex.Dom as RD
import Reflex.Dom.Contrib.Widgets.Common (WidgetConfig(..),Widget0(..),htmlDropdownStatic)

import Clay ((?),(@=),(#))
import qualified Clay as C

import Control.Monad.Reader (ask, asks,local)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid ((<>))

data DefSFCfg  = DefSFCfg
                 {
                   formConfig::FormConfig,
                   inputConfig::InputConfig
                 }

defFailureF::SimpleFormC e t m=>T.Text->SimpleFormR e t m a
defFailureF msg = SimpleFormR $ do
  RD.text msg
  return dynValidationNothing

data SFRPair e t m a = SFRPair { sfrpCN::B.ConName, sfrpV::SimpleFormR e t m a }

instance Eq (SFRPair e t m a) where
  (SFRPair a _) == (SFRPair b _) = a == b

defSumF::SimpleFormInstanceC e t m=>[(B.ConName,SimpleFormR e t m a)]->Maybe B.ConName->SimpleFormR e t m a
defSumF conWidgets mDefCon = SimpleFormR $ do
  let conNames = fst . unzip $ conWidgets
      getSFRP::B.ConName->[(B.ConName,SimpleFormR e t m a)]->SFRPair e t m a
      getSFRP cn = SFRPair cn . fromJust . M.lookup cn . M.fromList 
      pft (x,y) = SFRPair x y
      defPair = maybe (pft $ head conWidgets) (`getSFRP` conWidgets) mDefCon
  validClasses <- validItemStyle
  observerClasses <- observerOnlyStyle
  isObserver <- (==ObserveOnly) <$> getFormType
  let classes = if isObserver then observerClasses else validClasses 
      attrsDyn = R.constDyn (cssClassAttr classes <> titleAttr "Constructor")
      wc = WidgetConfig RD.never defPair attrsDyn
  formRow $ do
    sfrpCW <- itemL $ (sfWidget id (T.pack . sfrpCN) Nothing wc $ \wc' -> _widget0_value <$> htmlDropdownStatic conNames T.pack (`getSFRP` conWidgets) wc')
    unSF $ switchingSFR sfrpV defPair (R.updated sfrpCW)


instance (MonadWidgetExtraC t m, MonadIO (PushM t),RD.DomBuilder t m, R.MonadHold t m, MonadFix m, RD.PostBuild t m)=>SimpleFormBuilderFunctions DefSFCfg t m where
  failureF = defFailureF
  sumF = defSumF
  dynamicDiv  attrsDyn = liftLF $ RD.elDynAttr "div" attrsDyn
  
instance (MonadIO (PushM t),RD.DomBuilder t m,MonadWidgetExtraC t m
         , RD.MonadHold t m, MonadFix m, RD.PostBuild t m)=>SimpleFormLayoutFunctions DefSFCfg m where
  formItem = liftLF flexItem
  layoutOrientation LayoutHorizontal = liftLF flexRow
  layoutOrientation LayoutVertical = liftLF flexCol
  layoutFill d = liftLF (flexFill d)
  layoutCentered o = liftLF (flexCenter o)
  layoutCollapsible = collapsibleWidget
  getFormConfig = formConfig <$> ask
  setFormConfig fc = local (\cfg -> cfg{ formConfig = fc})
  getInputConfig = inputConfig <$> ask
  setInputConfig ic = local (\cfg -> cfg {inputConfig = ic})
  

collapsibleWidget::(MonadIO (PushM t),RD.DomBuilder t m)=>T.Text->CollapsibleInitialState->m a->m a
collapsibleWidget summary cis w = 
  RD.elAttr "details" (if cis == CollapsibleStartsOpen then "open" RD.=: "" else mempty) $ do
    RD.el "summary" $ RD.text summary
    w

-- The rest is css for the basic form and observer.  This can be customized by including a different style-sheet.

boxMargin m = C.sym C.margin (C.rem m)

cssOutlineTextBox m cBox cText = do
  boxMargin m
  C.border C.solid (C.px 2) cBox
  C.fontColor cText

cssSolidTextBox m cBox cText = do
  boxMargin m
  C.background cBox
  C.fontColor cText

-- some styles 
simpleFormBoxes = do
  ".sf-outline-black" ? cssOutlineTextBox 0.1 C.black C.black
  ".sf-outline-red" ? cssOutlineTextBox 0.1 C.red C.black
  ".sf-outline-blue" ? cssOutlineTextBox 0.1 C.blue C.black
  ".sf-outline-green" ? cssOutlineTextBox 0.1 C.green C.black
  ".sf-black-on-gray" ? cssSolidTextBox 0.1 C.gray C.black
  ".sf-white-on-gray" ? cssSolidTextBox 0.1 C.gray C.white

isSimpleForm::C.Selector
isSimpleForm = C.form # ".sf-form"


simpleFormElements = do
  isSimpleForm ? do
    C.background C.ghostwhite
    C.summary ? C.cursor C.pointer 
    C.button ? cssSolidTextBox 0.1 C.whitesmoke C.black
    C.input ? do
      C.verticalAlign C.middle
      C.position C.relative
    C.input  # ("type" @= "text") ? cssOutlineTextBox 0.1 C.lightslategrey C.black
    C.input  # ("type" @= "number") ? cssOutlineTextBox 0.1 C.lightslategrey C.black
    C.select ? cssOutlineTextBox 0.1 C.grey C.black
    C.input # ".sf-invalid" ? cssOutlineTextBox 0.1 C.red C.black -- invalid
    C.span ? do
      C.verticalAlign C.middle 
      
simpleFormDefaultCss = do
  simpleFormBoxes
  simpleFormElements

isSimpleObserver = C.div # ".sf-observer"

isSimpleObserverItem::C.Selector
isSimpleObserverItem = C.div # ".sf-observer-item" 


simpleObserverDefaultCss = do
  isSimpleObserver ? do
    C.background C.ghostwhite
    C.summary ? C.cursor C.pointer 
    isSimpleObserverItem ? do
      cssSolidTextBox 0.1 C.lightslategrey C.black
      C.sym C.padding (C.rem 0.1)

