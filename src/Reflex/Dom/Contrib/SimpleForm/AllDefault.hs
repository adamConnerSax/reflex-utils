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
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks, lift,local)
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
import Reflex.Dom.Contrib.Layout.FlexLayout (flexLayoutColSimple,flexLayoutRowSimple,flexLayoutItemSimple,
                                             flexFillR,flexFillL,flexHCenter,
                                             flexFillU,flexFillD,flexVCenter)

import Reflex.Dom.Contrib.SimpleForm.Builder
import Reflex.Dom.Contrib.SimpleForm.Instances(sfWidget)

import qualified DataBuilder as B

data DefSFCfg  = DefSFCfg
                 {
                   cfgValidStyle::CssClasses
                 , cfgInvalidStyle::CssClasses                                    
                 , cfgObserverStyle::CssClasses
                 , cfgLabelF::Maybe (String -> String)
                 , cfgObserver::Bool
                 }

setCfgToObserver::DefSFCfg->DefSFCfg
setCfgToObserver x = x { cfgObserver = True }

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
  observerClasses <- observerStyle
  isObserver <- observer
  let classes = if isObserver then observerClasses else validClasses 
      attrsDyn = R.constDyn (cssClassAttr (classes) <> titleAttr ("Constructor"))
      wc = WidgetConfig RD.never defPair attrsDyn
  formRow $ do
    sfrpCW <- itemL $ sfWidget id sfrpCN Nothing wc $ \c -> _widget0_value <$> htmlDropdownStatic conNames id (flip getSFRP conWidgets) wc
    unSF $ switchingSFR sfrpV defPair (R.updated sfrpCW)


instance (MonadIO (PushM t),RD.MonadWidget t m)=>SimpleFormConfiguration DefSFCfg t m where
  failureF = defFailureF
  sumF = defSumF
  dynamicDiv  attrsDyn = liftLF $ RD.elDynAttr "div" attrsDyn
  formItem    = liftLF $ flexLayoutItemSimple 
  layoutVert  = liftLF $ flexLayoutColSimple 
  layoutHoriz = liftLF $ flexLayoutRowSimple
  layoutL     = liftLF $ flexFillR 
  layoutR     = liftLF $ flexFillL
  layoutHC    = liftLF $ flexHCenter
  layoutT     = liftLF $ flexFillD 
  layoutB     = liftLF $ flexFillU
  layoutVC    = liftLF $ flexVCenter
  layoutCollapsible = collapsibleWidget 
  validItemStyle   = cfgValidStyle <$> ask 
  invalidItemStyle = cfgInvalidStyle <$> ask
  observerStyle    = cfgObserverStyle <$> ask
  observer         = cfgObserver <$> ask
  setToObserve = local (\cfg -> cfg {cfgObserver = True })
  setLayoutFieldName f = local (\cfg -> cfg { cfgLabelF = f })
  getLayoutFieldName = do
    mf <- asks cfgLabelF
    return $ mf >>= (\f -> Just $ textAtLeft . f)


collapsibleWidget::(MonadIO (PushM t),RD.MonadWidget t m)=>String->Bool->m a->m a
collapsibleWidget summary isOpen w = do
  RD.elAttr "details" (if isOpen then ("open" RD.=: "") else mempty) $ do
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

