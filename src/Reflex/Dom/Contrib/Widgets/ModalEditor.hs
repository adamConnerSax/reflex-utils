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
{-# LANGUAGE TupleSections         #-}
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
                                                          MonadHold, Reflex,
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

import           Reflex.Dom.Contrib.DynamicUtils         (dynAsEv, dynPlusEvent,
                                                          dynStartingFrom)
import           Reflex.Dom.Contrib.EventUtils           (fanBool,
                                                          leftWhenNotRight)
import qualified Reflex.Dom.Contrib.Layout.FlexLayout    as L
import qualified Reflex.Dom.Contrib.Layout.Types         as L
import           Reflex.Dom.Contrib.ReflexConstraints    (MonadWidgetExtraC)
import           Reflex.Dom.Contrib.Widgets.WidgetResult (WidgetResult,
                                                          WrappedWidgetResult,
                                                          buildWidgetResult,
                                                          currentWidgetResult,
                                                          dynamicToWidgetResult,
                                                          unsafeBuildWrappedWidgetResult,
                                                          updatedWidgetResult,
                                                          widgetResultToDynamic)

import           Control.Lens                            (makeLenses,
                                                          makePrisms, preview,
                                                          view, (%~), (&), (^.),
                                                          _Left, _Right)
import           Control.Monad                           (join)
import           Control.Monad.Fix                       (MonadFix)
import           Data.Bool                               (bool)
import           Data.Default
import           Data.Either                             (isLeft, isRight)
import           Data.Functor.Compose                    (Compose (Compose),
                                                          getCompose)
import qualified Data.Map                                as M
import           Data.Monoid                             ((<>))
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
data UpdateOutput = Always | OnOk deriving (Eq)

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

data EditorUpdate e a = OpenPressed | Edited (Either e a) | OkPressed | ClosePressed

makePrisms ''EditorUpdate

editorUpdateToText :: EditorUpdate e a -> T.Text
editorUpdateToText OpenPressed = "OpenPressed"
editorUpdateToText OkPressed = "OkPressed"
editorUpdateToText ClosePressed = "ClosePressed"
editorUpdateToText (Edited ea) = case ea of
  Left x  -> "Edited (Left)"
  Right x -> "Edited (Right)"

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
        (const $ ButtonConfig "Close" M.empty Nothing) -- simple Cancel button in footer


data InnerModalEvs t a = InnerModal { updateOutput :: Event t a, modalClosed :: Event t (), updateInput :: Event t a }

data ViewState = Button | Editor deriving (Eq)
data WidgetState t e a = WidgetState { viewState       :: Dynamic t ViewState
                                     , editWidgetInput :: Dynamic t (Either e a)
--                                     , editorValue :: Dynamic t (Either e a)
                                     , widgetOutput    :: WidgetResult t (Either e a) }

modalEditorFrame :: (Reflex t, MonadHold t m, MonadFix m) => ModalEditorConfig t e a -> Event t (EditorUpdate e a) -> Dynamic t (Either e a) -> m (WidgetState t e a)
modalEditorFrame cfg euEv eitherDyn = do
  let newInputEv = R.updated eitherDyn -- this only happens on input change so we don't put it in EditorUpdate
      newInputToEditorEv = case cfg ^. modalEditor_onChange of
        Close -> newInputEv -- or R.ffilter isRight newInputEv filter for only valid values?
        UpdateIfRight -> Right <$> R.fmapMaybe (preview _Right) newInputEv
        UpdateOrDefault a -> R.fmapMaybe (either (const $ Just $ Right a) (Just . Right)) newInputEv
      openEv = R.fmapMaybe (preview _OpenPressed) euEv
      editedEv = R.fmapMaybe (preview _Edited) euEv
      okPressedEv = R.fmapMaybe (preview _OkPressed) euEv
      closeOnOkEv = if cfg ^. modalEditor_closeOnOk then okPressedEv else R.never
      closeOnChangeEv = case cfg ^. modalEditor_onChange of
        Close             -> () <$ newInputEv
        UpdateIfRight     -> () <$ R.fmapMaybe (preview _Left) newInputEv
        UpdateOrDefault _ -> R.never
      closeEv = R.leftmost [R.fmapMaybe (preview _ClosePressed) euEv, closeOnOkEv, closeOnChangeEv]


  viewDyn <- R.holdDyn Button $ R.leftmost [Button <$ closeEv, Editor <$ openEv]


  rec editorVal <- dynPlusEvent editorInput editedEv
      editorInput <- dynStartingFrom eitherDyn $ R.leftmost [ newInputToEditorEv
                                                            , (R.tag (R.current editorVal) okPressedEv)
                                                            ]
  let updateOutputEv = R.leftmost [ R.tag (R.current editorVal) okPressedEv -- order matters.  okPressedEv can also fire closeEv
                                  , if cfg ^. modalEditor_updateOutput == Always then editedEv else R.never
                                  , R.tag (R.current editorInput) closeEv
                                  ]
  outputWR <- buildWidgetResult eitherDyn updateOutputEv
  return $ WidgetState viewDyn editorInput outputWR


widgetSwitcher :: (Reflex t, RD.DomBuilder t m, R.MonadHold t m)
  => (a -> b -> m c) -- function to produce a widget from the a given in the update event
  -> m c -- starting widget
  -> Event t (a, b) -- use a to choose widget function and give it b as an argument to produce
  -> m (R.Dynamic t c) -- the resulting widget with dynamic output to account for switching
widgetSwitcher selFunction startW updateEv =
  let toWidget (a, b) = selFunction a b
      widgetEv = toWidget <$> updateEv
  in RD.widgetHold startW widgetEv

widgetSwitcherEv :: (Reflex t, RD.DomBuilder t m, R.MonadHold t m)
  => (a -> b -> m (Event t c)) -- function to produce a widget from the a given in the update event
  -> m (Event t c) -- starting widget
  -> Event t (a, b) -- use a to choose widget function and give it b as an argument to produce
  -> m (Event t c) -- the resulting widget with dynamic output to account for switching
widgetSwitcherEv selFunction startW updateEv = R.switch . R.current <$> widgetSwitcher selFunction startW updateEv

-- | Modal Editor for a Dynamic a
modalEditorEither :: forall t m e a. (  Reflex t
                                      , RD.DomBuilder t m
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
  let --selF :: ViewState -> Dynamic t (Either e a) -> m (Event t (EditorUpdate e a))
      selF vs eaDyn = case vs of
        Button -> modalEditorOpenButton config eaDyn
        Editor -> configuredModalWidget config editW eaDyn

  rec (WidgetState viewDyn editorWidgetInputDyn outputWR) <- modalEditorFrame config editorUpdateEv aEDyn
      editorUpdateEv <- R.traceEventWith (T.unpack . editorUpdateToText) <$> widgetSwitcherEv selF (modalEditorOpenButton config aEDyn) ((,editorWidgetInputDyn) <$> R.updated viewDyn)

  return $ ModalEditor (widgetResultToDynamic outputWR) (R.fmapMaybe e2m $ updatedWidgetResult outputWR)

modalEditorOpenButton :: (Reflex t, RD.DomBuilder t m, RD.PostBuild t m) => ModalEditorConfig t e a -> Dynamic t (Either e a) -> m (Event t (EditorUpdate e a))
modalEditorOpenButton config eaDyn =
  let openButtonConfigOrig = (config ^. modalEditor_openButton) <$> eaDyn
      openButtonConfig = R.zipDynWith (\bc va -> bc & button_attributes %~ M.union va) openButtonConfigOrig
  in fmap (const OpenPressed) <$> (dynamicButton $ openButtonConfig (constDyn M.empty))


configuredModalWidget :: forall t m e a. ( Reflex t
                                         , RD.DomBuilder t m
                                         , MonadWidgetExtraC t m
                                         , RD.PostBuild t m
                                         , MonadFix m
                                         , RD.MonadHold t m
                                         )
  => ModalEditorConfig t e a
  -> (Dynamic t (Maybe a) -> m (WidgetResult t (Either e a)))
  -> Dynamic t (Either e a)
  -> m (Event t (EditorUpdate e a))
configuredModalWidget config editW eaDyn =
  let header d = maybe (return R.never) (\f -> L.flexFill L.LayoutLeft $ dynamicButton $ f <$> d) $ (config ^. modalEditor_XButton)
      footer d = L.flexRow $ do
        okEv <- L.flexFill L.LayoutRight $ dynamicButton ((config ^. modalEditor_OkButton) <$> d)
        cancelEv' <- L.flexFill L.LayoutLeft $ dynamicButton ((config ^. modalEditor_CancelButton) <$> d)
        return (cancelEv', okEv)
      body d = L.flexItem $ editW $ e2m <$> d
      modalAttrsDyn = config ^. modalEditor_attributes
  in RD.elDynAttr "div" modalAttrsDyn $ L.flexCol $ mkModalBodyUpdateAlwaysEU (header eaDyn) footer (body eaDyn)

mkModalBodyUpdateAlwaysEU :: ( RD.DomBuilder t m
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
  -> m (R.Event t (EditorUpdate e a))
mkModalBodyUpdateAlwaysEU header footer body = do
  RD.divClass "modal-dialog" $ RD.divClass "modal-content" $ do
    dismiss <- header
    bodyRes <- RD.divClass "modal-body" body
    (cancel, ok) <- footer $ widgetResultToDynamic bodyRes
    let resE1 = R.tag (currentWidgetResult bodyRes) ok
        closem1 = R.leftmost [dismiss, cancel]
    return $ R.leftmost [ ClosePressed <$ closem1
                        , OkPressed <$ R.fmapMaybe (preview _Right) resE1 -- only fire OK when value is valid.  We could handle this above?
                        , Edited <$> updatedWidgetResult bodyRes
                        ]
--    return (updatedWidgetResult bodyRes, R.ffilter isRight resE1, closem1)


-- | Modal Editor for a Dynamic a
-- | Widget as input takes an Event t () which will be fired when the modal is opened. This allows the widget to tag things.
modalEditorEither' :: forall t m e a. ( RD.DomBuilder t m
                                     , MonadWidgetExtraC t m
                                     , RD.PostBuild t m
                                     , MonadFix m
                                     , RD.MonadHold t m
                                     )
  => (Dynamic t (Maybe a) -> m (WidgetResult t (Either e a))) -- a widget for editing an a. returns Left on invalid value.
  -> Dynamic t (Either e a)
  -> ModalEditorConfig t e a
  -> m (ModalEditor t e a)
modalEditorEither' editW aEDyn config = do
  let newInputEv = R.updated aEDyn
      (newInputValEv, closeOnChangeEv) = case config ^. modalEditor_onChange of
        Close             -> (R.fmapMaybe (either (const Nothing) (Just . Right)) newInputEv, () <$ newInputEv)
        UpdateIfRight     -> ( R.fmapMaybe (either (const Nothing) (Just . Right)) newInputEv
                             , R.fmapMaybe (either (const $ Just ()) (const Nothing)) newInputEv)
        UpdateOrDefault a -> (either (const $ Right a) Right <$> newInputEv, R.never)
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
                closingValueEv = R.leftmost [ R.tag (R.current valueAtCancel) cancelEv -- when cancelled, we revert to the value we had when opened
                                            , newInputValEv -- when the input changes, we switch to that value
                                            , closeOnOkEv okAEEv -- when okay is pressed and that closes, we switch to the new value
                                            ]
                closingEv = R.leftmost [cancelEv, () <$ closeOnOkEv okAEEv, closeOnChangeEv]
            modalVisAttrs <- showAttrs closingEv R.never -- we don't need to reshow because we build the widget anew each time the button is pressed.
            (newAEEv', okAEEv, cancelEv) <- RD.elDynAttr "div" modalAttrsDyn $ L.flexCol $ mkModalBodyUpdateAlways (header eaDyn) footer body
            valueAtCancel <- R.holdDyn inputWhenOpened okAEEv
        let updateAEEv = case config ^. modalEditor_updateOutput of
                           Always -> newAEEv'
                           OnOk   -> okAEEv
            updateBodyAEEv = R.leftmost [okAEEv, closingValueEv] -- in case we don't close on okay
            retAEEv = R.leftmost [updateAEEv, inputWhenOpened <$ cancelEv]
        return $ InnerModal retAEEv closingEv updateBodyAEEv
  rec let openButtonConfigOrig = (config ^. modalEditor_openButton) <$> aeForBody
          openButtonConfig = R.zipDynWith (\bc va -> bc & button_attributes %~ M.union va) openButtonConfigOrig
      openButtonVisAttrs <- showAttrs openButtonEv modalCloseEv
      openButtonEv <- dynamicButton $ openButtonConfig openButtonVisAttrs
      let openEv = R.tag (current aeForBody) openButtonEv
      evOfInnerModal <- R.current <$> RD.widgetHold (return $ InnerModal R.never R.never R.never) (eaAndCloseW aeForBody <$> openEv)
      let updateOutputEv = R.switch (updateOutput <$> evOfInnerModal)
          modalCloseEv = R.switch (modalClosed <$> evOfInnerModal)
          updateModalInputEv = R.switch (updateInput <$> evOfInnerModal)
      aeForBody <- R.buildDynamic (R.sample $ R.current aEDyn) $ R.leftmost [ newInputValEv -- this order matters since modalClose will carry inputWhenOpened
                                                                            , updateModalInputEv
                                                                            ]
  newAEDyn <- R.buildDynamic (R.sample $ R.current aEDyn) $ R.leftmost [newInputEv, updateOutputEv] -- auth value
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
-- This version, copied From reflex-dom-contrib and modified,
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

