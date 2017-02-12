{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Reflex.Dom.Contrib.SimpleForm.AllDefault (DefaultConfigurationC) where

-- | A useful default config for quick form building
-- | Also serves as an example for building others
-- | and defFailureF and defSumF can be re-used for other implementations
import           Reflex.Dom.Contrib.Layout.FlexLayout          (flexCenter,
                                                                flexCol,
                                                                flexFill,
                                                                flexItem,
                                                                flexItem',
                                                                flexRow)
import           Reflex.Dom.Contrib.Layout.Types               (CssClasses (..), LayoutDirection (..),
                                                                LayoutOrientation (..),
                                                                emptyCss,
                                                                oneClass)

import           Reflex.Dom.Contrib.CssUtils                   (CssLinks (..))
import           Reflex.Dom.Contrib.Layout.ClayUtils           (cssToBS)
import           Reflex.Dom.Contrib.ReflexConstraints          (MonadWidgetExtraC)
import           Reflex.Dom.Contrib.SimpleForm.Builder
import           Reflex.Dom.Contrib.SimpleForm.Configuration
import           Reflex.Dom.Contrib.SimpleForm.Instances       (sfWidget)
import           Reflex.Dom.Contrib.SimpleForm.Instances.Basic (SimpleFormInstanceC)

import qualified DataBuilder                                   as B

import qualified Reflex                                        as R
import qualified Reflex.Dom                                    as RD
import           Reflex.Dom.Contrib.Widgets.Common             (Widget0 (..), WidgetConfig (..),
                                                                htmlDropdownStatic)

import           Clay                                          hiding (head, id)
import qualified Clay                                          as C
import qualified Clay.Flexbox                                  as Flexbox
import           Control.Monad.Fix                             (MonadFix)
import           Control.Monad.IO.Class                        (MonadIO)
import           Control.Monad.Reader                          (ask, asks, lift,
                                                                local)
import           Data.ByteString                               (ByteString)
import           Data.Default                                  (Default (..))
import qualified Data.Map                                      as M
import           Data.Maybe                                    (fromJust)
import           Data.Monoid                                   ((<>))
import qualified Data.Text                                     as T

import           Prelude                                       hiding (div, rem,
                                                                span)

instance Default (SimpleFormIncludedCss) where
  def = SimpleFormIncludedCss defaultCss (CssLinks [])

defaultCss::ByteString
defaultCss = cssToBS simpleFormDefaultCss <> cssToBS simpleObserverDefaultCss


type DefaultConfigurationC t m =
  (SimpleFormC t m,
   RD.PostBuild t m,
   MonadFix m,
   MonadWidgetExtraC t m,
   SimpleFormInstanceC t m)

instance Default (CssConfiguration) where
  def = CssConfiguration emptyCss (oneClass "sf-form") (oneClass "sf-observer") (oneClass "sf-item") emptyCss (oneClass "sf-valid") (oneClass "sf-invalid")



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

instance DefaultConfigurationC t m=> Default (SimpleFormConfiguration t m) where
  def = SimpleFormConfiguration Interactive def def def def

collapsibleWidget::RD.DomBuilder t m=>T.Text->CollapsibleInitialState->m a->m a
collapsibleWidget summary cis w =
  RD.elAttr "details" (if cis == CollapsibleStartsOpen then "open" RD.=: "" else mempty) $ do
    RD.el "summary" $ RD.text summary
    w

instance DefaultConfigurationC t m => Default (BuilderFunctions t m) where
  def = BuilderFunctions defFailureF defSumF defDynamicDiv

defDynamicDiv::(RD.DomBuilder t m, RD.PostBuild t m)=>DynAttrs t -> SFLayoutF t m
defDynamicDiv dynAttrs = liftLF $ RD.elDynAttr "div" dynAttrs

defFailureF::SimpleFormC t m=>T.Text->SFRW t m a
defFailureF msg = do
  RD.text msg
  return dynValidationNothing

data SFRPair t m a = SFRPair { sfrpCN::B.ConName, sfrpV::SFRW t m a }

instance Eq (SFRPair t m a) where
  (SFRPair a _) == (SFRPair b _) = a == b

defSumF::DefaultConfigurationC t m=>[(B.ConName,SFRW t m a)]->Maybe B.ConName->SFRW t m a
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

-- The rest is css for the basic form and observer.  This can be customized by including a different style-sheet.

boxMargin m = sym margin (rem m)

cssOutlineTextBox m cBox cText = do
  boxMargin m
  border solid (px 2) cBox
  fontColor cText

cssSolidTextBox m cBox cText = do
  boxMargin m
  background cBox
  fontColor cText

-- some styles
simpleFormBoxes = do
  ".sf-outline-black" ? cssOutlineTextBox 0.1 black black
  ".sf-outline-red" ? cssOutlineTextBox 0.1 red black
  ".sf-outline-blue" ? cssOutlineTextBox 0.1 blue black
  ".sf-outline-green" ? cssOutlineTextBox 0.1 green black
  ".sf-black-on-gray" ? cssSolidTextBox 0.1 gray black
  ".sf-white-on-gray" ? cssSolidTextBox 0.1 gray white

isSimpleForm::Selector
isSimpleForm = form # ".sf-form"

isSimpleFormItem::Selector
isSimpleFormItem = div # ".sf-item"

simpleFormElements = do
  isSimpleForm ? do
    fontSize (rem 1.1)
    border solid (px 1) black
    sym borderRadius (rem 0.2)
    sym padding (rem 0.2)
    summary ? cursor pointer
    button ? do
      sym borderRadius (rem 0.2)
      cssSolidTextBox 0.1 whitesmoke black
    input ? do
      fontSize (rem 1.1)
      sym borderRadius (rem 0.2)
      verticalAlign middle
      position relative
    input  # ("type" @= "text") ? cssOutlineTextBox 0.1 lightslategrey black
    input  # ("type" @= "number") ? cssOutlineTextBox 0.1 lightslategrey black
    select ? do
      fontSize (rem 1.1)
      cssOutlineTextBox 0.1 grey black
    input # ".sf-invalid" ? cssOutlineTextBox 0.1 red black -- invalid
    span ? do
      verticalAlign middle
  isSimpleFormItem ? do
    marginTop (px 10)
    width (pct 60)
    display flex
    justifyContent spaceBetween
    flexWrap Flexbox.wrap

simpleFormDefaultCss = do
  simpleFormBoxes
  simpleFormElements

isSimpleObserver = C.div # ".sf-observer"

isSimpleObserverItem::Selector
isSimpleObserverItem = C.div # ".sf-observer-item"


simpleObserverDefaultCss = do
  isSimpleObserver ? do
    background ghostwhite
    summary ? cursor pointer
    isSimpleObserverItem ? do
      cssSolidTextBox 0.1 lightslategrey black
      sym padding (rem 0.1)

