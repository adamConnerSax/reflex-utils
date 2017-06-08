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
    ModalEditor
  , modalEditor_value
  , modalEditor_mValue
  , modalEditor_change
  , modalEditor_WidgetResult
  , OnExternalChange (..)
  , UpdateOutput (..)
  , ButtonConfig (..)
  , button_label
  , button_attributes
  , button_iconClass
  , removeIfError
  , disableAndDisplayIfError
  , ModalEditorConfig (..)
  , modalEditor_attributes
  , modalEditor_onChange
  , modalEditor_updateOutput
  , modalEditor_closeOnOk
  , modalEditor_openButton
  , modalEditor_XButton
  , modalEditor_OkButton
  , modalEditor_CancelButton
  , modalEditor
  , modalEditorEither
  ) where

import           Reflex.Dom.Contrib.DynamicUtils         (dynAsEv)

import           Reflex                                  (Dynamic, Event,
                                                          Reflex,
                                                          attachWithMaybe,
                                                          leftmost, never)
import qualified Reflex                                  as R
import           Reflex.Dom                              (DropdownConfig (..),
                                                          dropdown, widgetHold)
import qualified Reflex.Dom                              as RD
--import qualified Reflex.Dom.Contrib.Widgets.Modal        as RDC
import           Reflex.Dynamic                          (constDyn, current,
                                                          tagPromptlyDyn,
                                                          updated)

import           Reflex.Dom.Contrib.DynamicUtils         (dynAsEv)
import           Reflex.Dom.Contrib.EventUtils           (leftWhenNotRight)
import qualified Reflex.Dom.Contrib.Layout.FlexLayout    as L
import qualified Reflex.Dom.Contrib.Layout.Types         as L
import           Reflex.Dom.Contrib.ReflexConstraints    (MonadWidgetExtraC)
import           Reflex.Dom.Contrib.Widgets.WidgetResult (WidgetResult,
                                                          WrappedWidgetResult,
                                                          currentWidgetResult,
                                                          unsafeBuildWrappedWidgetResult,
                                                          updatedWidgetResult,
                                                          widgetResultToDynamic)

import           Control.Lens                            (makeLenses, view,
                                                          (%~), (&), (^.))
import           Control.Monad                           (join)
import           Control.Monad.Fix                       (MonadFix)
import           Data.Default
import           Data.Either                             (isRight)
import           Data.Functor.Compose                    (Compose (Compose),
                                                          getCompose)
import qualified Data.Map                                as M
import qualified Data.Text                               as T
import           Safe                                    (headMay)

e2m :: Either e a -> Maybe a
e2m = either (const Nothing) Just

m2e :: Maybe a -> Either () a
m2e = maybe (Left ()) Right


data ModalEditor t e a = ModalEditor { _modalEditor_value  :: Dynamic t (Either e a)
                                     , _modalEditor_change :: Event t a -- only fire when there is a valid value
                                     }

--makeLenses ''ModalEditor
modalEditor_value :: ModalEditor t e a -> Dynamic t (Either e a)
modalEditor_value = _modalEditor_value

modalEditor_change :: ModalEditor t e a -> Event t a
modalEditor_change = _modalEditor_change

modalEditor_mValue :: Reflex t => ModalEditor t e a -> Dynamic t (Maybe a)
modalEditor_mValue = fmap e2m . modalEditor_value

modalEditor_WidgetResult :: Reflex t => ModalEditor t e a -> WrappedWidgetResult t (Either e) a
modalEditor_WidgetResult me = unsafeBuildWrappedWidgetResult (modalEditor_value me) (Right <$> modalEditor_change me)

-- update function can place a default in on Nothing
-- NB: This doesn't work yet because of a conflict with updating on close and reopen.  So control will always close on input change.
data OnExternalChange a = Close | UpdateIfRight | UpdateOrDefault a
data UpdateOutput = Always | OnOk

data ButtonConfig = ButtonConfig { _button_label      :: T.Text
                                 , _button_attributes :: M.Map T.Text T.Text
                                 , _button_iconClass  :: Maybe T.Text
                                 }

makeLenses ''ButtonConfig

-- widget could go in here but has no sensible default except "return"
data ModalEditorConfig t e a = ModalEditorConfig { _modalEditor_attributes   :: Dynamic t (M.Map T.Text T.Text)
                                                 , _modalEditor_onChange     :: OnExternalChange a
                                                 , _modalEditor_updateOutput :: UpdateOutput
                                                 , _modalEditor_closeOnOk    :: Bool
                                                 , _modalEditor_openButton   :: Either e a -> ButtonConfig
                                                 , _modalEditor_XButton      :: Maybe (Either e a -> ButtonConfig)
                                                 , _modalEditor_OkButton     :: Either e a -> ButtonConfig
                                                 , _modalEditor_CancelButton :: Either e a -> ButtonConfig
                                                 }

makeLenses ''ModalEditorConfig

removeIfError :: Either e a -> ButtonConfig -> ButtonConfig
removeIfError (Left _) (ButtonConfig l attrs ic) = ButtonConfig l (M.union hiddenCSS attrs) ic
removeIfError _ x = x

disableAndDisplayIfError :: (e -> T.Text) -> Either e a -> ButtonConfig -> ButtonConfig
disableAndDisplayIfError f (Left e) (ButtonConfig l attrs ic) = ButtonConfig (f e) (M.union disableCSS attrs) Nothing
disableAndDisplayIfError _ _ x = x

dynamicButton :: (RD.DomBuilder t m, RD.PostBuild t m) => Dynamic t ButtonConfig -> m (RD.Event t ())
dynamicButton cfg = do
  let attrs = R.zipDynWith M.union (constDyn ("type" RD.=: "button")) (view button_attributes <$> cfg)
  fmap (RD.domEvent RD.Click . fst) $ RD.elDynAttr' "button" attrs $ do
    let iconAttrs = maybe M.empty (M.singleton "class")
    _ <- RD.elDynAttr "i" (iconAttrs . view button_iconClass <$> cfg) RD.blank
    RD.dynText $ view button_label <$> cfg


instance Reflex t => Default (ModalEditorConfig t e a) where
  def = ModalEditorConfig
        (constDyn M.empty)
        Close -- close on change to input dynamic
        OnOk -- pass all updates through
        True -- close on OK
        (const $ ButtonConfig "Edit" M.empty Nothing) -- default to Edit button which is disabled when input is Nothing
        Nothing -- default to no "x" button in the header
        (const $ ButtonConfig "OK" M.empty Nothing) -- simple OK button in footer
        (const $ ButtonConfig "Cancel" M.empty Nothing) -- simple Cancel button in footer

data InnerModal t a = InnerModal { updateOutput :: Event t a, modalClosed :: Event t (), updateInput :: Event t a }

-- | Modal Editor for a Dynamic a
-- | Widget as input takes an Event t () which will be fired when the modal is opened. This allows the widget to tag things.
modalEditorEither :: forall t m e a. ( RD.DomBuilder t m
                                     , MonadWidgetExtraC t m
                                     , RD.PostBuild t m
                                     , MonadFix m
                                     , RD.MonadHold t m
                                     )
  => (Dynamic t (Maybe a) -> m (WidgetResult t (Either e a))) -- a widget for editing an a. returns Left on invalid value.
  -> Dynamic t (Either e a)
  -> ModalEditorConfig t e a
  -> m (ModalEditor t e a)
modalEditorEither editW aEDyn config = do
  let newInputEv = R.updated aEDyn
      (closeOnChangeEv, switchToDefaultEv) = case config ^. modalEditor_onChange of
        Close             -> (newInputEv, R.never)
        UpdateIfRight     -> (R.fmapMaybe (either (const Nothing) (Just . Right)) newInputEv, R.never)
        UpdateOrDefault a -> (R.never, R.fmapMaybe (either (const $ Just $ Right a) (const Nothing)) newInputEv)
      header eaDyn = maybe (return R.never) (\f -> L.flexFill L.LayoutLeft $ dynamicButton $ f <$> eaDyn) $ (config ^. modalEditor_XButton)
      footer eaDyn = L.flexRow $ do
        okEv <- L.flexFill L.LayoutRight $ dynamicButton ((config ^. modalEditor_OkButton) <$> eaDyn)
        cancelEv' <- L.flexFill L.LayoutLeft $ dynamicButton ((config ^. modalEditor_CancelButton) <$> eaDyn)
        return (cancelEv', okEv)
      showAttrs hideEv showEv = R.holdDyn visibleCSS $ R.leftmost [hiddenCSS <$ hideEv, visibleCSS <$ showEv]
      eaAndCloseW eaDyn inputWhenOpened = do
        let body = L.flexItem $ editW $ e2m <$> eaDyn
            closeOnOkEv okEv = if config ^. modalEditor_closeOnOk then okEv else R.never
        rec let modalAttrsDyn = R.zipDynWith M.union modalVisAttrs (config ^. modalEditor_attributes)
                closingValueEv = R.leftmost [ inputWhenOpened <$ cancelEv -- when cancelled, we revert to the value we had when opened
                                            , closeOnChangeEv -- when the input changes, we switch to that value
                                            , closeOnOkEv okAEEv -- when okay is pressed and that closes, we switch to the new value
                                            ]
            modalVisAttrs <- showAttrs closingValueEv R.never -- we don't need to reshow because we build the widget anew each time the button is pressed.
            (newAEEv', okAEEv, cancelEv) <- RD.elDynAttr "div" modalAttrsDyn $ L.flexCol $ mkModalBodyUpdateAlways (header eaDyn) footer body
        let updateAEEv = case config ^. modalEditor_updateOutput of
                           Always -> newAEEv'
                           OnOk   -> okAEEv
            updateBodyAEEv = R.leftmost [okAEEv, closingValueEv] -- in case we don't close on okay
            retAEEv = R.leftmost [updateAEEv, inputWhenOpened <$ cancelEv]
        return $ InnerModal retAEEv (() <$ closingValueEv) updateBodyAEEv
  rec let openButtonConfigOrig = (config ^. modalEditor_openButton) <$> aeForBody
          openButtonConfig = R.zipDynWith (\bc va -> bc & button_attributes %~ M.union va) openButtonConfigOrig
      openButtonVisAttrs <- showAttrs openButtonEv modalCloseEv
      openButtonEv <- dynamicButton $ openButtonConfig openButtonVisAttrs
      let openEv = R.tag (current aeForBody) openButtonEv
      evOfInnerModal <- R.current <$> RD.widgetHold (return $ InnerModal R.never R.never R.never) (eaAndCloseW aeForBody <$> openEv)
      let updateOutputEv = R.switch (updateOutput <$> evOfInnerModal)
          modalCloseEv = R.switch (modalClosed <$> evOfInnerModal)
          updateModalInputEv = R.switch (updateInput <$> evOfInnerModal)
      aeForBody <- R.buildDynamic (R.sample $ R.current aEDyn) $ R.leftmost [ switchToDefaultEv
                                                                            , newInputEv -- this order matters since modalClose will carry inputWhenOpened
                                                                            , updateModalInputEv
                                                                            ]
  newAEDyn <- R.buildDynamic (R.sample $ R.current aEDyn) $ R.leftmost [switchToDefaultEv, newInputEv, updateOutputEv] -- auth value
  return $ ModalEditor newAEDyn (R.fmapMaybe e2m updateOutputEv)


modalEditor :: forall t m a. ( RD.DomBuilder t m
                             , MonadWidgetExtraC t m
                             , RD.PostBuild t m
                             , MonadFix m
                             , RD.MonadHold t m
                             )
  => (Dynamic t (Maybe a) -> m (WidgetResult t (Maybe a))) -- a widget for editing an a. returns Left on invalid value
  -> Dynamic t (Maybe a)
  -> ModalEditorConfig t () a
  -> m (ModalEditor t () a)
modalEditor editW aMDyn config = modalEditorEither (fmap (fmap m2e) . editW) (m2e <$> aMDyn) config



-- | Template for a modal with a header, body, and footer where the header has
-- a close icon and the footer has a cancel and save button.
-- this version, copied From reflex-dom-contrib and modified.
-- passes through all updates on its output so it can be wrapped to require "ok" or not.
-- It has 3 event outputs:
-- 1. all updates that come from the body and
-- 2. a tagged current value of the body on a press of "OK" and
-- 3. an event to signal that a cancel or dismiss button has been pressed.
mkModalBodyUpdateAlways
    :: ( RD.DomBuilder t m
       , MonadWidgetExtraC t m
       , RD.PostBuild t m
       , MonadFix m
       , RD.MonadHold t m
       )
    => m (R.Event t ())
    -- ^ A header widget returning an event that closes the modal.
    -> (R.Dynamic t (Either e a) -> m (R.Event t (), R.Event t ()))
    -- ^ Footer widget that takes the current state of the body and returns
    -- a pair of a cancel event and an ok event.
    -> m (WidgetResult t (Either e a))
    -> m (R.Event t (Either e a), R.Event t (Either e a), R.Event t ())
mkModalBodyUpdateAlways header footer body = do
  RD.divClass "modal-dialog" $ RD.divClass "modal-content" $ do
    dismiss <- header
    bodyRes <- RD.divClass "modal-body" body
    (cancel, ok) <- footer $ widgetResultToDynamic bodyRes
    let resE1 = R.tag (currentWidgetResult bodyRes) ok
        closem1 = R.leftmost [dismiss, cancel]
    return (updatedWidgetResult bodyRes, R.ffilter isRight resE1, closem1)

hiddenCSS :: M.Map T.Text T.Text
hiddenCSS  = "style" RD.=: "display: none !important"

visibleCSS :: M.Map T.Text T.Text
visibleCSS = "style" RD.=: "display: inline"

disableCSS :: M.Map T.Text T.Text
disableCSS = "disabled" RD.=: ""

