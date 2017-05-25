{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Reflex.Dom.Contrib.FormBuilder.AllDefault (DefaultConfigurationC) where

-- | A useful default config for quick form building
-- | Also serves as an example for building others
-- | and defFailureF and defSumF can be re-used for other implementations
import           Reflex.Dom.Contrib.Layout.FlexLayout           (flexCenter,
                                                                 flexCol,
                                                                 flexFill,
                                                                 flexItem,
                                                                 flexItem',
                                                                 flexRow)
import           Reflex.Dom.Contrib.Layout.Types                (CssClasses (..),
                                                                 LayoutDirection (..),
                                                                 LayoutOrientation (..),
                                                                 emptyCss,
                                                                 oneClass,
                                                                 toCssString)

import           Reflex.Dom.Contrib.CssUtils                    (CssLinks (..))
import           Reflex.Dom.Contrib.FormBuilder.Builder
import           Reflex.Dom.Contrib.FormBuilder.Configuration
import           Reflex.Dom.Contrib.FormBuilder.Instances       (formWidget)
import           Reflex.Dom.Contrib.FormBuilder.Instances.Basic (FormInstanceC)
import           Reflex.Dom.Contrib.Layout.ClayUtils            (cssToBS)
import           Reflex.Dom.Contrib.ReflexConstraints           (MonadWidgetExtraC)

import qualified DataBuilder                                    as B

import qualified Reflex                                         as R
import qualified Reflex.Dom                                     as RD
import           Reflex.Dom.Contrib.Widgets.Common              (Widget0 (..), WidgetConfig (..),
                                                                 htmlDropdownStatic)
import qualified Reflex.Dom.Widget.Basic                        as RD

import           Clay                                           hiding (head,
                                                                 id)
import qualified Clay                                           as C
import qualified Clay.Flexbox                                   as Flexbox
import           Control.Monad                                  (join)
import           Control.Monad.Fix                              (MonadFix)
import           Control.Monad.IO.Class                         (MonadIO)
import           Control.Monad.Reader                           (ask, asks,
                                                                 lift, local)
import           Data.ByteString                                (ByteString)
import           Data.Default                                   (Default (..))
import           Data.Functor.Compose                           (Compose (Compose, getCompose))
import qualified Data.Map                                       as M
import           Data.Maybe                                     (fromMaybe)
import           Data.Monoid                                    ((<>))
import qualified Data.Text                                      as T

import           Prelude                                        hiding (div,
                                                                 rem, span)

instance Default (FormIncludedCss) where
  def = FormIncludedCss defaultCss (CssLinks [])

defaultCss :: ByteString
defaultCss = cssToBS formDefaultCss <> cssToBS observerDefaultCss

type DefaultConfigurationC t m = FormInstanceC t m

byFormType :: FormType -> a -> a -> a
byFormType ft ifInteractive ifReadOnly = case ft of
  Interactive -> ifInteractive
  ObserveOnly -> ifReadOnly

instance Default (CssConfiguration) where
  def = CssConfiguration
    (\ft->byFormType ft (oneClass "sf-container") (oneClass "sf-observer"))
    (const $ oneClass "sf-item")
    (const emptyCss)
    (const $ oneClass "sf-valid")
    (const $ oneClass "sf-invalid")


instance Default (InputElementConfig t) where
  def = InputElementConfig Nothing Nothing Nothing

defLayoutWrapper :: RD.DomBuilder t m => FormType -> FLayoutF t m
defLayoutWrapper _ w = do
  classes <- wrapperClasses
  RD.divClass (toCssString classes) w

defLayoutItem :: RD.DomBuilder t m => FormType -> FLayoutF t m
defLayoutItem _ w = do
  classes <- itemClasses
  liftLF (flexItem' classes) w

defLayoutOriented :: RD.DomBuilder t m => FormType -> LayoutOrientation -> FLayoutF t m
defLayoutOriented _ LayoutHorizontal = liftLF flexRow
defLayoutOriented _ LayoutVertical   = liftLF flexCol

defLayoutFill :: RD.DomBuilder t m => FormType -> LayoutDirection -> FLayoutF t m
defLayoutFill _ d = liftLF (flexFill d)

defLayoutCentered :: RD.DomBuilder t m => FormType -> LayoutOrientation -> FLayoutF t m
defLayoutCentered _ o = liftLF (flexCenter o)

defLayoutCollapsible :: RD.DomBuilder t m => FormType -> T.Text -> CollapsibleInitialState -> FLayoutF t m
defLayoutCollapsible _ t is = liftLF (collapsibleWidget t is)

instance RD.DomBuilder t m => Default (LayoutConfiguration t m) where
  def = LayoutConfiguration defLayoutWrapper defLayoutItem defLayoutOriented defLayoutFill defLayoutCentered defLayoutCollapsible

instance DefaultConfigurationC t m => Default (FormConfiguration t m) where
  def = FormConfiguration Interactive def def def def

collapsibleWidget :: RD.DomBuilder t m => T.Text -> CollapsibleInitialState -> m a -> m a
collapsibleWidget summary cis w =
  RD.elAttr "details" (if cis == CollapsibleStartsOpen then "open" RD.=: "" else mempty) $ do
    RD.el "summary" $ RD.text summary
    w

instance DefaultConfigurationC t m => Default (BuilderFunctions t m) where
  def = BuilderFunctions defFailureF defSumF defDynamicDiv

defDynamicDiv :: (RD.DomBuilder t m, RD.PostBuild t m) => DynAttrs t -> FLayoutF t m
defDynamicDiv dynAttrs = liftLF $ RD.elDynAttr "div" dynAttrs

defFailureF :: RD.DomBuilder t m => T.Text -> FRW t m a
defFailureF msg = do
  RD.text msg
  return dynValidationNothing

whichFired :: R.Reflex t => [R.Event t a] -> R.Event t Int
whichFired = R.leftmost . zipWith (<$) [0..]

safeIndex :: Int -> [a] -> Maybe a
safeIndex n l = let ln = take (n+1) l in if length ln == (n+1) then Just (last ln) else Nothing

safeHead :: [a] -> Maybe a
safeHead = safeIndex 0

-- FIXME: write a version that uses all the widgets at once, hiding the unused ones.  THat will be more efficient in the case when switching is expected
defSumF :: DefaultConfigurationC t m => [(B.ConName,R.Event t (),FRW t m a)]->FRW t m a
defSumF conWidgets = fRow $ do
  let (names, events, widgets) = unzip3 conWidgets
      indexedNames = zip [0..] (T.pack <$> names)
      inputIndexEv = whichFired events
  validClasses <- validDataClasses
  formType <- getFormType
  let selectionControl = case formType of
        Interactive ->
          let ddAttrs = (titleAttr "Constructor") -- <> observeOnlyAttr)
              ddConfig = RD.DropdownConfig inputIndexEv (R.constDyn ddAttrs)
          in RD._dropdown_change <$> RD.dropdown 0 (RD.constDyn $ M.fromList indexedNames) ddConfig
        ObserveOnly -> do
          let newConNameEv = R.fmapMaybe (\n -> T.pack <$> safeIndex n names) inputIndexEv
          curConName <- R.holdDyn "" newConNameEv
          RD.divClass "sf-observed-constructor" (RD.dynText curConName) >> return R.never

  chosenIndexEv <- fItem $ selectionControl
  let newIndexEv = R.leftmost [inputIndexEv,chosenIndexEv]
  curIndex <- R.holdDyn 0 newIndexEv
  let switchWidgetEv = R.updated . R.uniqDyn $ curIndex
      errorW msg = do
        RD.el "span" $ RD.text msg
        return $ dynValidationErr [FInvalid msg]
      newWidgetEv = fromMaybe (errorW "index error in defSumF!") . (\n -> safeIndex n widgets) <$> switchWidgetEv
  fItem $ joinDynOfDynValidation <$> RD.widgetHold (fromMaybe (errorW "empty widget list in defSumF!") $ safeHead widgets) newWidgetEv

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
formBoxes = do
  ".sf-outline-black" ? cssOutlineTextBox 0.1 black black
  ".sf-outline-red" ? cssOutlineTextBox 0.1 red black
  ".sf-outline-blue" ? cssOutlineTextBox 0.1 blue black
  ".sf-outline-green" ? cssOutlineTextBox 0.1 green black
  ".sf-black-on-gray" ? cssSolidTextBox 0.1 gray black
  ".sf-white-on-gray" ? cssSolidTextBox 0.1 gray white

isFormContainer :: Selector
isFormContainer = ".sf-container"

isFormItem :: Selector
isFormItem = div # ".sf-item"

isValidData :: Selector
isValidData = div # ".sf-valid"

isInvalidData :: Selector
isInvalidData = div # ".sf-invalid"

isObservedConstructor :: Selector
isObservedConstructor = div # ".sf-observed-constructor"


formElements = do
  isFormContainer ? do
    fontSize (rem 1)
    border solid (px 1) black
    sym borderRadius (rem 0.2)
    sym padding (rem 0.2)
    summary ? cursor pointer
    button ? do
      sym borderRadius (rem 0.2)
      cssSolidTextBox 0.1 whitesmoke black
      textAlign center
    input ? do
      fontSize (rem 1)
      sym borderRadius (rem 0.2)
      verticalAlign middle
      position relative
    input  # ("type" @= "text") ? cssOutlineTextBox 0.1 lightslategrey black
    input  # ("type" @= "number") ? cssOutlineTextBox 0.1 lightslategrey black
    select ? do
      fontSize (rem 1)
      cssOutlineTextBox 0.1 grey black
    input # ".sf-invalid" ? cssOutlineTextBox 0.1 red black -- invalid
    span ? do
      verticalAlign middle
  isFormItem ? do
    label ? do
      display flex
      flexDirection Flexbox.row
      flexWrap Flexbox.wrap
      alignItems center
      span ? do
        Flexbox.flex 1 0 auto --sfLabelWidth
        minWidth (rem 5)
      span |+ star ? do
        Flexbox.flex 2 0 auto --sfInputWidth

formDefaultCss = do
  formBoxes
  formElements

isObserver :: Selector
isObserver = C.div # ".sf-observer"

isObserverItem :: Selector
isObserverItem = C.div # ".sf-observer-item"


observerDefaultCss = do
  isObserver ? do
    background ghostwhite
    summary ? cursor pointer
    isValidData ? do
      cssOutlineTextBox 0.1 black black
      sym padding (rem 0.1)
    isObservedConstructor ? do
      cssOutlineTextBox 0.1 red red
