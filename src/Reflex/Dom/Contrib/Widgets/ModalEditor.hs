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
  , ButtonConfig (..)
  , button_label
  , button_attributes
  , button_iconClass
  , disableIfNothing
  , ModalEditorConfig (..)
  , modalEditor_attributes
  , modalEditor_onChange
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
import qualified Reflex.Dom.Contrib.Layout.FlexLayout    as L
import           Reflex.Dom.Contrib.ReflexConstraints    (MonadWidgetExtraC)
import           Reflex.Dom.Contrib.Widgets.WidgetResult (WidgetResult,
                                                          WrappedWidgetResult,
                                                          unsafeBuildWrappedWidgetResult)

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

disableIfNothing :: Maybe a -> ButtonConfig -> ButtonConfig
disableIfNothing Nothing (ButtonConfig l attrs ic) = ButtonConfig l (M.union hiddenCSS attrs) ic
disableIfNothing _ x = x

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
-- | Widget as input takes an Event t () which will be fired when the modal is opened. This allows the widget to tag things.
modalEditorEither :: forall t m e a. ( RD.DomBuilder t m
                                     , MonadWidgetExtraC t m
                                     , RD.PostBuild t m
                                     , MonadFix m
                                     , RD.MonadHold t m
                                     )
  => (Dynamic t (Maybe a) -> m (Dynamic t (Either e a))) -- a widget for editing an a. returns Left on invalid value.
  -> Dynamic t (Either e a)
  -> ModalEditorConfig t a
  -> m (ModalEditor t e a)
modalEditorEither editW aEDyn config = mdo
  let newInputEv = R.updated aEDyn
      (widgetInputEv, closeOnChangeEv) = case config ^. modalEditor_onChange of
        _ {- Close -}   -> (e2m <$> newInputEv, () <$ newInputEv)
--        UpdateIfRight -> (e2m <$> newInputEv, R.fmapMaybe (either (const $ Just ()) (const Nothing)) newInputEv)
--        UpdateOrDefault a -> (either (const $ Just a) Just <$> newInputEv, R.never)
      header = maybe (return R.never) (\f -> L.flexRow $ dynamicButton $ f . e2m <$> newAEDyn) $ (config ^. modalEditor_XButton)
      footer eaDyn = L.flexRow $ do
        okEv <- L.flexItem $ dynamicButton ((config ^. modalEditor_OkButton) . e2m <$> eaDyn)
        cancelEv' <- L.flexItem $ dynamicButton ((config ^. modalEditor_CancelButton) . e2m <$> eaDyn)
        return (cancelEv', okEv)
      showAttrs hideEv showEv = R.holdDyn visibleCSS $ R.leftmost [hiddenCSS <$ hideEv, visibleCSS <$ showEv]
      eaAndCloseW input = mdo -- returns m (Event t (Either e a), Event t ())
        let body = L.flexRow $ editW $ (R.constDyn $ e2m input) -- we freeze the input on open
            modalAttrsDyn = R.zipDynWith M.union modalVisAttrs (config ^. modalEditor_attributes)
            closeOnOkEv okEv = if (config ^. modalEditor_closeOnOk) then () <$ okEv else R.never
            closeEv = R.leftmost [ cancelEv
                                 , closeOnChangeEv
                                 , closeOnOkEv newAEEv'
                                 ]
        modalVisAttrs <- showAttrs closeEv R.never -- we don't need to reshow because we build the widget anew each time the button is pressed.
        (newAEEv', cancelEv) <- L.flexCol $ RD.elDynAttr "div" modalAttrsDyn $ mkModalBody header footer body
        return $ (newAEEv', closeEv)
  let openButtonConfigOrig = (config ^. modalEditor_openButton) . e2m <$> newAEDyn
      openButtonConfig = R.zipDynWith (\bc va -> bc & button_attributes %~ M.union va) openButtonConfigOrig
  openButtonVisAttrs <- showAttrs openButtonEv modalCloseEv
  openButtonEv <- dynamicButton $ openButtonConfig openButtonVisAttrs
  let openEv = R.tag (R.current newAEDyn) openButtonEv
  evOfEvs <- R.current <$> RD.widgetHold (return (R.never, R.never)) (eaAndCloseW <$> openEv)
  let newEaEv = R.switch (fst <$> evOfEvs)
      modalCloseEv = R.switch (snd <$> evOfEvs)
  newAEDyn <- R.buildDynamic (R.sample $ R.current aEDyn) $ R.leftmost [newInputEv, newEaEv]
  return $ ModalEditor newAEDyn (R.fmapMaybe e2m newEaEv)

-- NB:  This is only in (Either e a) rather than a because of the holdDyn, which needs a starting point.

modalEditor :: forall t m a. ( RD.DomBuilder t m
                             , MonadWidgetExtraC t m
                             , RD.PostBuild t m
                             , MonadFix m
                             , RD.MonadHold t m
                             )
  => (Dynamic t (Maybe a) -> m (Dynamic t (Maybe a))) -- a widget for editing an a. returns Left on invalid value
  -> Dynamic t (Maybe a)
  -> ModalEditorConfig t a
  -> m (ModalEditor t () a)
modalEditor editW aMDyn config = modalEditorEither (fmap (fmap m2e) . editW) (m2e <$> aMDyn) config



-- | Template for a modal with a header, body, and footer where the header has
-- a close icon and the footer has a cancel and save button.
-- copied From reflex-dom-contrib and modified to work with WidgetResults
mkModalBody
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
    -> m (R.Dynamic t (Either e a))
    -> m (R.Event t (Either e a), R.Event t ())
mkModalBody header footer body = do
  RD.divClass "modal-dialog" $ RD.divClass "modal-content" $ do
    dismiss <- header
    bodyRes <- RD.divClass "modal-body" body
    (cancel, ok) <- footer bodyRes
    let resE1 = R.tag (R.current bodyRes) ok -- changed
        closem1 = R.leftmost
                  [dismiss, cancel, () <$ R.ffilter isRight resE1]
    return (resE1, closem1)

hiddenCSS :: M.Map T.Text T.Text
hiddenCSS  = "style" RD.=: "display: none !important"

visibleCSS :: M.Map T.Text T.Text
visibleCSS = "style" RD.=: "display: inline"


