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

import qualified Reflex as R 
import qualified Reflex.Dom as RD
import Reflex.Dom.Contrib.Widgets.Common (WidgetConfig(..),Widget0(..),htmlDropdownStatic)
import Reflex.Dom.Contrib.Layout.Types (CssClass(..),CssClasses(..))
import Reflex.Dom.Contrib.Layout.FlexLayout (flexLayoutColSimple,flexLayoutRowSimple,flexLayoutItemSimple,flexFillR,flexFillL,flexVCenter)

import Reflex.Dom.Contrib.SimpleForm.Builder
import Reflex.Dom.Contrib.SimpleForm.Instances()

import qualified DataBuilder as B

data DefSFCfg  = DefSFCfg
                 {
                   cfgValidStyle::CssClasses
                 , cfgInvalidStyle::CssClasses
                 , cfgButtonStyle::CssClasses
                 , cfgDropdownStyle::CssClasses
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
--  env <- ask
  let conNames = fst . unzip $ conWidgets
      getSFRP::B.ConName->[(B.ConName,SimpleFormR e t m a)]->SFRPair e t m a
      getSFRP cn = SFRPair cn . fromJust . M.lookup cn . M.fromList 
      pft (x,y) = SFRPair x y
      defPair = maybe (pft $ head conWidgets) (\cn->getSFRP cn conWidgets) mDefCon
  validClasses <- validItemStyle
  dropdownClasses <- dropdownStyle
  disabled <- inputsDisabled
  let disabledAttr = if disabled then ("disabled" RD.=: "") else mempty
      attrsDyn = R.constDyn (cssClassAttr (validClasses <> dropdownClasses) <> titleAttr ("Constructor") <> disabledAttr)
  formRow $ do
    sfrpCW <- itemL $ _widget0_value <$> htmlDropdownStatic conNames id (flip getSFRP conWidgets) (WidgetConfig RD.never defPair attrsDyn)
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
  buttonStyle = cfgButtonStyle <$> ask
  dropdownStyle = cfgDropdownStyle <$> ask
  inputsDisabled = cfgDisable <$> ask
  disableInputs = local disableDefCfg
