{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
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

import Control.Monad.IO.Class (MonadIO)
import Control.Lens ((^.))
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift,local)
import Data.Maybe (fromJust)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Monoid ((<>))
import qualified Clay as C
import Clay ((?),(@=),(#))

import qualified Reflex as R 
import qualified Reflex.Dom as RD
import Reflex.Dom.Contrib.Widgets.Common (WidgetConfig(..),Widget0(..),htmlDropdownStatic)
import Reflex.Dom.Contrib.Layout.Types (CssClass(..),CssClasses(..))
import Reflex.Dom.Contrib.Layout.FlexLayout (flexLayoutColSimple,flexLayoutRowSimple,flexLayoutItemSimple,flexFillR,flexFillL,flexVCenter)

import Reflex.Dom.Contrib.SimpleForm.Builder
import Reflex.Dom.Contrib.SimpleForm.Instances(sfWidget)

import qualified DataBuilder as B

data DefSFCfg  = DefSFCfg
                 {
                   cfgValidStyle::CssClasses
                 , cfgInvalidStyle::CssClasses
                 , cfgDisable::Bool
                 }

disableDefCfg::DefSFCfg->DefSFCfg
disableDefCfg x = x { cfgDisable = True }

defFailureF::SimpleFormC e t m=>String->SimpleFormR e t m a
defFailureF msg = SimpleFormR $ do
  RD.text msg
  return $ R.constDyn Nothing

data SFRPair e t m a = SFRPair { sfrpCN::B.ConName, sfrpV::(SimpleFormR e t m a) }

instance Eq (SFRPair e t m a) where
  (SFRPair a _) == (SFRPair b _) = a == b

defSumF::SimpleFormC e t m=>[(B.ConName,SimpleFormR e t m a)]->Maybe B.ConName->SimpleFormR e t m a
defSumF conWidgets mDefCon = SimpleFormR $ do
  let conNames = fst . unzip $ conWidgets
      getSFRP::B.ConName->[(B.ConName,SimpleFormR e t m a)]->SFRPair e t m a
      getSFRP cn = SFRPair cn . fromJust . M.lookup cn . M.fromList 
      pft (x,y) = SFRPair x y
      defPair = maybe (pft $ head conWidgets) (\cn->getSFRP cn conWidgets) mDefCon
  validClasses <- validItemStyle
  disabled <- inputsDisabled
  let disabledAttr = if disabled then ("disabled" RD.=: "") else mempty
      attrsDyn = R.constDyn (cssClassAttr (validClasses) <> titleAttr ("Constructor") <> disabledAttr)
      wc = WidgetConfig RD.never defPair attrsDyn
  formRow $ do
    sfrpCW <- itemL $ sfWidget id sfrpCN  wc $ \c -> _widget0_value <$> htmlDropdownStatic conNames id (flip getSFRP conWidgets) wc
    unSF $ switchingSFR sfrpV defPair (R.updated sfrpCW)


instance (MonadIO(PushM t),RD.MonadWidget t m)=>SimpleFormConfiguration DefSFCfg t m where
  failureF = defFailureF
  sumF = defSumF
  formItem = liftLF $ flexLayoutItemSimple 
  dynamicDiv attrsDyn = liftLF $ RD.elDynAttr "div" attrsDyn
  layoutVert = liftLF $ flexLayoutColSimple 
  layoutHoriz = liftLF $ flexLayoutRowSimple
  layoutL = liftLF $ flexFillR 
  layoutR  = liftLF $ flexFillL 
  validItemStyle = cfgValidStyle <$> ask 
  invalidItemStyle = cfgInvalidStyle <$> ask
  inputsDisabled = cfgDisable <$> ask
  disableInputs = local disableDefCfg

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
isSimpleForm = C.form # ".simpleForm"

simpleFormElements = do
  isSimpleForm ? do
    C.background C.ghostwhite
    C.button ? cssSolidTextBox 0.1 C.whitesmoke C.black
    C.input  # ("type" @= "text") ? cssOutlineTextBox 0.1 C.lightslategrey C.black
    C.input  # ("type" @= "number") ? cssOutlineTextBox 0.1 C.lightslategrey C.black
    C.select ? cssOutlineTextBox 0.1 C.grey C.black
    C.input # ".sf-invalid" ? cssOutlineTextBox 0.1 C.red C.black -- invalid
      
simpleFormDefaultCss = do
  simpleFormBoxes
  simpleFormElements

isSimpleObserver = C.div # ".simpleObserver"

simpleObserverDefaultCss = do
  isSimpleObserver ? do
    simpleFormBoxes

