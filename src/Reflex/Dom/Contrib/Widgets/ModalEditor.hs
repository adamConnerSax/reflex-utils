{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.Dom.Contrib.Widgets.ModalEditor
  (
    ModalEditor (..)
  , modalEditor_value
  , modalEditor_change
  , OnExternalChange (..)
  , ButtonConfig (..)
  , button_label
  , button_attributes
  , button_iconClass
  , ModalEditorConfig (..)
  , modalEditor_attributes
  , modalEditor_onChange
  , modalEditor_closeOnOk
  , modalEditor_openButton
  , modalEditor_XButton
  , modalEditor_OkButton
  , modalEditor_CancelButton
  , modalEditor
  ) where

import           Reflex.Dom.Contrib.DynamicUtils      (dynAsEv)

import           Reflex                               (Dynamic, Event, Reflex,
                                                       attachWithMaybe,
                                                       leftmost, never)
import qualified Reflex                               as R
import           Reflex.Dom                           (DropdownConfig (..),
                                                       dropdown, widgetHold)
import qualified Reflex.Dom                           as RD
import qualified Reflex.Dom.Contrib.Widgets.Modal     as RDC
import           Reflex.Dynamic                       (constDyn, current,
                                                       tagPromptlyDyn, updated)

import           Reflex.Dom.Contrib.DynamicUtils      (dynAsEv)
import qualified Reflex.Dom.Contrib.Layout.FlexLayout as L
import           Reflex.Dom.Contrib.ReflexConstraints (MonadWidgetExtraC)

import           Control.Lens                         (makeLenses, view, (%~),
                                                       (&), (^.))
import           Control.Monad                        (join)
import           Control.Monad.Fix                    (MonadFix)
import           Data.Default
import qualified Data.Map                             as M
import qualified Data.Text                            as T
import           Safe                                 (headMay)


data ModalEditor t a = ModalEditor { _modalEditor_value  :: Dynamic t (Maybe a)
                                   , _modalEditor_change :: Event t a -- only fire when there is a valid value
                                   }

makeLenses ''ModalEditor

-- update function can place a default in on Nothing
data OnExternalChange a = Close | Update (Maybe a -> Maybe a)

data ButtonConfig = ButtonConfig { _button_label      :: T.Text
                                 , _button_attributes :: M.Map T.Text T.Text
                                 , _button_iconClass  :: Maybe T.Text
                                 }

makeLenses ''ButtonConfig

-- widget could go in here but has no sensible default except "return"
data ModalEditorConfig t a = ModalEditorConfig { _modalEditor_attributes   :: Dynamic t (M.Map T.Text T.Text)
                                               , _modalEditor_onChange     :: OnExternalChange a
                                               , _modalEditor_closeOnOk    :: Bool
                                               , _modalEditor_openButton   :: Maybe a -> ButtonConfig
                                               , _modalEditor_XButton      :: Maybe (Maybe a -> ButtonConfig)
                                               , _modalEditor_OkButton     :: Maybe a -> ButtonConfig
                                               , _modalEditor_CancelButton :: Maybe a -> ButtonConfig
                                               }

makeLenses ''ModalEditorConfig

dynamicButton :: (RD.DomBuilder t m, RD.PostBuild t m) => Dynamic t ButtonConfig -> m (RD.Event t ())
dynamicButton cfg = do
  let attrs = R.zipDynWith M.union (constDyn ("type" RD.=: "button")) (view button_attributes <$> cfg)
  fmap (RD.domEvent RD.Click . fst) $ RD.elDynAttr' "button" attrs $ do
    let iconAttrs = maybe M.empty (M.singleton "class")
    _ <- RD.elDynAttr "i" (iconAttrs . view button_iconClass <$> cfg) RD.blank
    RD.dynText $ view button_label <$> cfg


instance Reflex t => Default (ModalEditorConfig t a) where
  def = ModalEditorConfig
        (constDyn M.empty)
        Close -- close on change to input dynamic
        True -- close on OK
        (const $ ButtonConfig "Edit" M.empty Nothing) -- default to Edit button which is disabled when input is Nothing
        Nothing -- default to no "x" button in the header
        (const $ ButtonConfig "OK" M.empty Nothing) -- simple OK button in footer
        (const $ ButtonConfig "Cancel" M.empty Nothing) -- simple Cancel button in footer

-- | Modal Editor for a Dynamic a
modalEditor :: forall t m a. ( RD.DomBuilder t m
                             , MonadWidgetExtraC t m
                             , RD.PostBuild t m
                             , MonadFix m
                             , RD.MonadHold t m
                             )
  => (Dynamic t (Maybe a) -> m (Dynamic t (Maybe a))) -- a widget for editing an a.  Blank on Nothing, returns Nothing on invalid value
  -> Dynamic t (Maybe a)
  -> ModalEditorConfig t a
  -> m (ModalEditor t a)
modalEditor editW aMDyn config = mdo
  let (widgetInputDyn, closeOnChangeEv) = case config ^. modalEditor_onChange of
        Close    -> (aMDyn, () <$ R.updated aMDyn)
        Update f -> (f <$> aMDyn, R.never)
      e2m = either (const Nothing) Just
      m2e = maybe (Left ()) Right
      header = maybe (return R.never) (\f -> L.flexRow $ dynamicButton $ f <$> newAMDyn) $ (config ^. modalEditor_XButton)
      footer eaDyn = L.flexRow $ do
        okEv <- L.flexItem $ dynamicButton ((config ^. modalEditor_OkButton) . e2m <$> eaDyn)
        cancelEv' <- L.flexItem $ dynamicButton ((config ^. modalEditor_CancelButton) . e2m <$> eaDyn)
        return (cancelEv', okEv)
      showAttrs hideEv showEv = R.holdDyn visibleCSS $ R.leftmost [hiddenCSS <$ hideEv, visibleCSS <$ showEv]
      maAndCloseEv = mdo -- returns m (Event t (Maybe a), Event t ())
        let body = fmap m2e <$> (L.flexRow $ editW widgetInputDyn)
            modalAttrsDyn = R.zipDynWith M.union modalVisAttrs (config ^. modalEditor_attributes)
            closeOnOkEv okEv = if (config ^. modalEditor_closeOnOk) then () <$ okEv else R.never
            closeEv = R.leftmost [ cancelEv
                                 , closeOnChangeEv
                                 , closeOnOkEv newAMEv'
                                 ]
        modalVisAttrs <- showAttrs closeEv R.never -- we don't need to reshow because we build the widget anew each time the button is pressed.
        (newAMEv', cancelEv) <- L.flexCol $ RD.elDynAttr "div" modalAttrsDyn $ RDC.mkModalBody header footer body
        return $ (e2m <$> newAMEv', closeEv)
  let openButtonConfigOrig = (config ^. modalEditor_openButton) <$> aMDyn
      openButtonConfig = R.zipDynWith (\bc va -> bc & button_attributes %~ M.union va) openButtonConfigOrig
--  aMEv <- dynAsEv aMDyn
  openButtonVisAttrs <- showAttrs openButtonEv modalCloseEv
  openButtonEv <- dynamicButton $ openButtonConfig openButtonVisAttrs
  evOfEvs <- R.current <$> RD.widgetHold (return (R.never, R.never)) (maAndCloseEv <$ openButtonEv)
  let newAEv = R.fmapMaybe id $ R.switch (fst <$> evOfEvs)
      modalCloseEv = R.switch (snd <$> evOfEvs)
--  newAMDyn <- R.holdDyn Nothing $ R.leftmost [aMEv, Just <$> newAEv]
  newAMDyn <- R.buildDynamic (R.sample (R.current aMDyn)) $ R.leftmost [R.updated aMDyn, Just <$> newAEv]
  return $ ModalEditor newAMDyn newAEv

-- NB:  This is only in (Maybe a) rather than a because of the holdDyn, which needs a starting point.

hiddenCSS :: M.Map T.Text T.Text
hiddenCSS  = "style" RD.=: "display: none !important"

visibleCSS :: M.Map T.Text T.Text
visibleCSS = "style" RD.=: "display: inline"
