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
         simpleFormDefaultCss
       , simpleObserverDefaultCss
         -- these two can be re-used in other env types
       , defFailureF 
       , defSumF
       ) where

-- | A useful default config for quick form building
-- | Also serves as an example for building others
-- | and defFailureF and defSumF can be re-used for other implementations
import Reflex.Dom.Contrib.Layout.Types (CssClasses(..),LayoutDirection(..),LayoutOrientation(..),emptyCss,oneClass)
import Reflex.Dom.Contrib.Layout.FlexLayout (flexCol,flexRow,flexItem,flexItem',
                                             flexFill,flexCenter)

import Reflex.Dom.Contrib.ReflexConstraints (MonadWidgetExtraC)
import Reflex.Dom.Contrib.SimpleForm.Builder
import Reflex.Dom.Contrib.SimpleForm.Configuration
import Reflex.Dom.Contrib.SimpleForm.Instances(sfWidget)
import Reflex.Dom.Contrib.SimpleForm.Instances.Basic(SimpleFormInstanceC)

import qualified DataBuilder as B

import qualified Reflex as R 
import qualified Reflex.Dom as RD
import Reflex.Dom.Contrib.Widgets.Common (WidgetConfig(..),Widget0(..),htmlDropdownStatic)

import Clay ((?),(@=),(#))
import qualified Clay as C

import Control.Monad.Reader (ask, asks,local,lift)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Default (Default(..))



defFailureF::SimpleFormC t m=>T.Text->SFRW t m a
defFailureF msg = do
  RD.text msg
  return dynValidationNothing

data SFRPair t m a = SFRPair { sfrpCN::B.ConName, sfrpV::SFRW t m a }

instance Eq (SFRPair t m a) where
  (SFRPair a _) == (SFRPair b _) = a == b

defSumF::(SimpleFormC t m, RD.PostBuild t m, MonadFix m, MonadWidgetExtraC t m, 
          SimpleFormInstanceC t m)=>[(B.ConName,SFRW t m a)]->Maybe B.ConName->SFRW t m a
defSumF conWidgets mDefCon = do
  let conNames = fst . unzip $ conWidgets
      getSFRP::B.ConName->[(B.ConName,SFRW t m a)]->SFRPair t m a
      getSFRP cn = SFRPair cn . fromJust . M.lookup cn . M.fromList 
      pft (x,y) = SFRPair x y
      defPair = maybe (pft $ head conWidgets) (`getSFRP` conWidgets) mDefCon
  validClasses <- validInputStyle
  observerClasses <- observerOnlyStyle
  isObserver <- (==ObserveOnly) <$> getFormType
  let classes = if isObserver then observerClasses else validClasses 
      attrsDyn = R.constDyn (cssClassAttr classes <> titleAttr "Constructor")
      wc = WidgetConfig RD.never defPair attrsDyn
  sfRow $ do
    sfrpCW <- sfItemL $ (sfWidget id (T.pack . sfrpCN) Nothing wc $ \wc' -> _widget0_value <$> htmlDropdownStatic conNames T.pack (`getSFRP` conWidgets) wc')
    unSF $ switchingSFR (SimpleFormR . sfrpV) defPair (R.updated sfrpCW)


instance (SimpleFormC t m, RD.PostBuild t m, MonadFix m, MonadWidgetExtraC t m, 
          SimpleFormInstanceC t m) => Default (BuilderFunctions t m) where
  def = BuilderFunctions defFailureF defSumF (\attrsDyn->liftLF $ RD.elDynAttr "div" attrsDyn)

{-
instance (MonadWidgetExtraC t m, MonadIO (PushM t),RD.DomBuilder t m, R.MonadHold t m, MonadFix m, RD.PostBuild t m)=>SimpleFormBuilderFunctions t m where
  failureF = defFailureF
  sumF = defSumF
  dynamicDiv  attrsDyn = liftLF $ RD.elDynAttr "div" attrsDyn
-}

instance Default (CssConfiguration) where
  def = CssConfiguration emptyCss (oneClass "sf-form") (oneClass "sf-observer") emptyCss emptyCss (oneClass "sf-valid") (oneClass "sf-invalid") (oneClass "sf-observer")

instance Default (InputElementConfig) where
  def = InputElementConfig Nothing Nothing Nothing

defFormItem::RD.DomBuilder t m=>SFLayoutF t m
defFormItem w = do
  itemStyle <- formItemStyle
  liftLF (flexItem' itemStyle) w

defLayoutOriented::RD.DomBuilder t m=>LayoutOrientation->SFLayoutF t m
defLayoutOriented LayoutHorizontal = liftLF flexRow
defLayoutOriented LayoutVertical = liftLF flexCol

defLayoutFill::RD.DomBuilder t m=>LayoutDirection->SFLayoutF t m
defLayoutFill d = liftLF (flexFill d)

defLayoutCentered::RD.DomBuilder t m=>LayoutOrientation->SFLayoutF t m
defLayoutCentered o = liftLF (flexCenter o)

defLayoutCollapsible::RD.DomBuilder t m=>T.Text->CollapsibleInitialState->SFLayoutF t m
defLayoutCollapsible t is = liftLF (collapsibleWidget t is)

instance RD.DomBuilder t m=>Default (LayoutConfiguration t m) where
  def = LayoutConfiguration defFormItem defLayoutOriented defLayoutFill defLayoutCentered defLayoutCollapsible

instance (SimpleFormC t m, RD.PostBuild t m, MonadFix m, MonadWidgetExtraC t m, 
          SimpleFormInstanceC t m) =>Default (SimpleFormConfiguration t m) where
  def = SimpleFormConfiguration Interactive def def def def

collapsibleWidget::RD.DomBuilder t m=>T.Text->CollapsibleInitialState->m a->m a
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

