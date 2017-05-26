{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.Dom.Contrib.Widgets.ModalEditor
  (

  ) where

import Reflex.Dom.Contrib.DynamicUtils (dynAsEv)

import Reflex (Dynamic, Event, Reflex, never, attachWithMaybe, leftmost)
import Reflex.Dynamic (updated,constDyn,current,tagPromptlyDyn)
import Reflex.Dom (widgetHold,dropdown,DropdownConfig(..))
import qualified Reflex.Dom as RD
import qualified Reflex as R
import qualified Reflex.Dom.Contrib.Widgets.Modal as RDC

import qualified Reflex.Dom.Contrib.Layout.FlexLayout as L

import Control.Lens (makeLenses)
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Data.Default
import Safe (headMay)
import qualified Data.Map as M
import qualified Data.Text as T


data ModalEditor a = ModalEditor { _modalEditor_value :: Dynamic t (Maybe a)
                                 , _modalEditor_change :: Event t a -- only fire when there is a valid value
                                 }

-- update function can place a default in on Nothing
data OnExternalChange a = Close | Update (Maybe a -> Maybe a) 
data ButtonConfig = ButtonConfig { _button_label :: T.Text
                                 , _button_attributes :: M.Map T.Text T.Text
                                 }

makeLenses ''ButtonConfig

-- widget could go in here but has no sensible default except "return"
data ModalEditorConfig t a = { _modalEditor_attributes :: Dynamic t (M.Map T.Text T.Text)
                             , _modalEditor_onChange :: OnExternalChange a
                             , _modalEditor_openButton :: Maybe a -> ButtonConfig
                             , _modalEditor_XButton :: Maybe (Maybe a -> ButtonConfig)
                             , _modalEditor_OkButton :: Maybe a -> ButtonConfig
                             , _modalEditor_CancelButton :: Maybe a -> ButtonConfig
                             }

makeLenses ''ModalEditorConfig                           

dynamicButton :: RD.DomBuilder t m=> Dynamic t ButtonConfig -> m (RD.Event t ())
dynamicButton cfg attrsDyn =
  let attrs = R.zipDynWith union (constDyn ("type" RD.=: "button")) (view button_attributes <$> cfg)
  in (RD.domEvent RD.Click . fst) <$> RD.elDynAttr' "button" attrs (RD.dynText $ view button_label <$> cfg)


instance Reflex t => Default (ModalEditorConfig a) where
  def = ModalEditorConfig
        (constDyn M.empty) 
        (Left ()) -- close on external change to input dynamic
        (<$ ButtonConfig "Edit" M.empty) -- default to Edit button which is disbled when input is Nothing
        Nothing -- default to no "x" button in the header
        (<$ ButtonConfig "OK" M.empty) -- simple OK button in footer
        (<$ ButtonConfig "Cancel" M.empty) -- simple Cancel button in footer

-- | Modal Editor for a Dynamic a
modalEditor :: forall t m a. (RD.DomBuilder t m, RD.PostBuild t m)
  => (Dynamic t (Maybe a) -> m (Dynamic t (Maybe a))) -- a widget for editing an a.  Blank on Nothing, returns Nothing on invalid value
  -> Dynamic t a
  -> ModalEditorConfig t a
  -> m Dynamic t (Maybe a)
modalEditor editW aMDyn config = L.flexCol $ mdo
  let (widgetInputDyn, closeOnChangeEv) = case config ^. modalEditor_onChange of
        Close -> (aMDyn, () <$ R.updated aMDyn)
        Update f -> (f <$> aMDyn, R.never) 
      openButton = dynamicButton (config ^. modalEditor_openButton <$> aMDyn) 
      header = maybe return (\f -> dynamicButton $ f <$> newAMDyn) $ config ^. modalEditor_XButton 
      footer = L.flexRow $ do
        okEv <- dynamicButton (config ^. modalEditor_OKButton <$> newAMDyn)
        cancelEv' <- dynamicButton (config ^. modalEditor_CancelButton <$> newAMDyn)
        return (cancelEv', okEv)
      maEv = mdo
        let body = L.flexRow $ editW wigetInputDyn
            attrsDyn = R.zipDynWith union visAttrs (config ^. modalEditor_attributes)
        visAttrs <- R.holdDyn visibleCss (hiddenCss <$ R.leftmost [cancelEv, closeOnChangeEv])
        (newAMEv', cancelEv) <- R.elDynAttr "div" attrsDyn $ RDC.mkModalBody header footer body
        return newAMEv'
  postbuild <- RD.getPostBuild
  newAMEv <- R.switch . R.current <$> RD.widgetHold (return R.never) (maEv <$ openButton)
  newAMDyn <- R.holdDyn Nothing $ R.leftmost [tag (R.current aMDyn) postbuild, newMAEv]
  return $ ModalEditor newAMDyn newAMEv 
