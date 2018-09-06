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

import           Reflex                               (Dynamic, Event,
                                                       MonadHold, Reflex)
import qualified Reflex                               as R
import qualified Reflex.Dom                           as RD

import           Reflex.Dynamic                       (constDyn)

--import           Reflex.Dom.Contrib.DynamicUtils         (dynPlusEvent,
--                                                          dynStartingFrom)
import qualified Reflex.Dom.Contrib.Layout.FlexLayout as L
import qualified Reflex.Dom.Contrib.Layout.Types      as L
import           Reflex.Dom.Contrib.ReflexConstraints (MonadWidgetExtraC)
{-import           Reflex.Dom.Contrib.Widgets.WidgetResult (WidgetResult,
                                                          WrappedWidgetResult,
                                                          currentWidgetResult,
                                                          unsafeBuildWrappedWidgetResult,
                                                          updatedWidgetResult,
                                                          widgetResultToDynamic)
-}

import           Control.Lens                         (makeLenses, preview,
                                                       view, (%~), (&), (^.),
                                                       _Left, _Right)
import           Control.Monad.Fix                    (MonadFix)
import           Data.Default
import           Data.Either                          (isRight)
import qualified Data.Map                             as M
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T


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

--modalEditor_WidgetResult :: Reflex t => ModalEditor t e a -> WrappedWidgetResult t (Either e) a
--modalEditor_WidgetResult me = unsafeBuildWrappedWidgetResult (modalEditor_value me) (Right <$> modalEditor_change me)

e2m :: Either e a -> Maybe a
e2m = either (const Nothing) Just

m2e :: Maybe a -> Either () a
m2e = maybe (Left ()) Right

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

data EditorEvs t e a = EditorEvs { _openPressed  :: Event t ()
                                 , _okPressed    :: Event t (Either e a)
                                 , _closePressed :: Event t (Either e a)
                                 , _edited       :: Event t (Either e a)
                                 }

makeLenses ''EditorEvs

openOnlyEvs :: Reflex t => Event t () -> EditorEvs t e a
openOnlyEvs ev = EditorEvs ev R.never R.never R.never

removeIfError :: Either e a -> ButtonConfig -> ButtonConfig
removeIfError (Left _) (ButtonConfig l attrs ic) = ButtonConfig l (M.union hiddenCSS attrs) ic
removeIfError _ x = x

disableAndDisplayIfError :: (e -> T.Text) -> Either e a -> ButtonConfig -> ButtonConfig
disableAndDisplayIfError f (Left e) (ButtonConfig _ attrs _) = ButtonConfig (f e) (M.union disableCSS attrs) Nothing
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




data ViewState = Button | Editor deriving (Eq)
data WidgetState t e a = WidgetState { viewState       :: Dynamic t ViewState
                                     , editWidgetInput :: Dynamic t (Either e a)
                                     , editorValue     :: Dynamic t (Either e a)
                                     , editorChange    :: Event t a
                                     }

modalEditorFrame :: (Reflex t, MonadHold t m, MonadFix m)
  => ModalEditorConfig t e a
  -> EditorEvs t e a
  -> Dynamic t (Either e a)
  -> m (WidgetState t e a)
modalEditorFrame cfg edEvs valueDyn = do
  let inputValueEv = R.updated valueDyn -- this only happens on input change so we don't put it in EditorUpdate
      validInputValueEv = R.fmapMaybe (preview _Right) inputValueEv
      editedValueEv = view edited edEvs
      openButtonEv = view openPressed edEvs
      okButtonValueEv = view okPressed edEvs
      closeButtonValueEv = view closePressed edEvs

      closeOnOkValueEv = if cfg ^. modalEditor_closeOnOk then okButtonValueEv else R.never

      closeOnChangeValueEv = case cfg ^. modalEditor_onChange of
        Close             -> inputValueEv
        UpdateIfRight     -> Left <$> R.fmapMaybe (preview _Left) inputValueEv -- i.e., close if Left
        UpdateOrDefault _ -> R.never

      closeValueEv = R.leftmost [closeButtonValueEv, closeOnOkValueEv, closeOnChangeValueEv]

      inputToEditorValueEv = case cfg ^. modalEditor_onChange of
        Close             -> inputValueEv
        UpdateIfRight     -> Right <$> validInputValueEv
        UpdateOrDefault a -> either (const $ Right a) Right <$> inputValueEv

  viewDyn <- R.holdDyn Button $ R.leftmost [Button <$ closeValueEv, Editor <$ openButtonEv]
  editorInput <- dynStartingFrom valueDyn $ R.leftmost [inputToEditorValueEv, closeValueEv]
  let updateOutputEv = R.leftmost [ closeValueEv -- order matters.  closeOnOkValueEv should fire rather than okButtonValueEv
                                  , if cfg ^. modalEditor_updateOutput == Always then editedValueEv else okButtonValueEv
                                  ]
  dynOut <- dynPlusEvent valueDyn updateOutputEv
  return $ WidgetState viewDyn editorInput dynOut (R.fmapMaybe e2m updateOutputEv)

-- | Modal Editor for a Dynamic a
modalEditorEither :: forall t m e a. (  Reflex t
                                      , RD.DomBuilder t m
                                      , MonadWidgetExtraC t m
                                      , RD.PostBuild t m
                                      , MonadFix m
                                      , RD.MonadHold t m
                                     )
  => (Dynamic t (Maybe a) -> m (Dynamic t (Either e a))) -- a widget for editing an a. returns Left on invalid value.
  -> Dynamic t (Either e a)
  -> ModalEditorConfig t e a
  -> m (ModalEditor t e a)
modalEditorEither editW aEDyn config = do
  let selF eaDyn vs = case vs of
        Button -> modalEditorOpenButton config eaDyn
        Editor -> configuredModalWidget config editW eaDyn

  rec (WidgetState viewDyn editorWidgetInputDyn modalValue modalChange) <- modalEditorFrame config editorEvs aEDyn
      res <- R.current <$> RD.widgetHold (modalEditorOpenButton config aEDyn) (selF editorWidgetInputDyn <$> R.updated viewDyn)
      let editorEvs = EditorEvs
                      (R.switch $ view openPressed <$> res)
                      (R.switch $ view okPressed <$> res)
                      (R.switch $ view closePressed <$> res)
                      (R.switch $ view edited <$> res)

  return $ ModalEditor modalValue modalChange

modalEditorOpenButton :: (Reflex t, RD.DomBuilder t m, RD.PostBuild t m)
  => ModalEditorConfig t e a
  -> Dynamic t (Either e a)
  -> m (EditorEvs t e a)
modalEditorOpenButton config eaDyn =
  let openButtonConfigOrig = (config ^. modalEditor_openButton) <$> eaDyn
      openButtonConfig = R.zipDynWith (\bc va -> bc & button_attributes %~ M.union va) openButtonConfigOrig
  in openOnlyEvs <$> (dynamicButton $ openButtonConfig (constDyn M.empty))

configuredModalWidget :: forall t m e a. ( Reflex t
                                         , RD.DomBuilder t m
                                         , MonadWidgetExtraC t m
                                         , RD.PostBuild t m
                                         , MonadFix m
                                         , RD.MonadHold t m
                                         )
  => ModalEditorConfig t e a
  -> (Dynamic t (Maybe a) -> m (Dynamic t (Either e a)))
  -> Dynamic t (Either e a)
  -> m (EditorEvs t e a)
configuredModalWidget config editW eaDyn = do
  let header d = maybe (return R.never) (\f -> L.flexFill L.LayoutLeft $ dynamicButton $ f <$> d) $ (config ^. modalEditor_XButton)
      footer d = L.flexRow $ do
        okEv <- L.flexFill L.LayoutRight $ dynamicButton ((config ^. modalEditor_OkButton) <$> d)
        cancelEv' <- L.flexFill L.LayoutLeft $ dynamicButton ((config ^. modalEditor_CancelButton) <$> d)
        return (cancelEv', okEv)
      modalAttrsDyn = R.zipDynWith (M.unionWith (\c1 c2 -> c1 <> " " <> c2)) (config ^. modalEditor_attributes) (R.constDyn ("class" RD.=: "modal-dialog"))
  RD.elDynAttr "div" modalAttrsDyn $ RD.divClass "modal-content" $ do
    rec dismiss <- header bodyDyn
        bodyDyn <- L.flexItem $ RD.divClass "modal-body" $ editW $ e2m <$> eaDyn
    (cancel, ok) <- footer bodyDyn
    let okValueEv = Right <$> R.attachWithMaybe (\e _ -> preview _Right e)  (R.current bodyDyn) ok
    valueIfClosed <- dynPlusEvent eaDyn okValueEv
    let closeValueEv = R.tag (R.current valueIfClosed) $ R.leftmost [dismiss, cancel]
    return $ EditorEvs R.never okValueEv closeValueEv (R.updated bodyDyn)


modalEditor :: forall t m a. ( RD.DomBuilder t m
                             , MonadWidgetExtraC t m
                             , RD.PostBuild t m
                             , MonadFix m
                             , RD.MonadHold t m
                             )
  => (Dynamic t (Maybe a) -> m (Dynamic t (Maybe a))) -- a widget for editing an a. returns Left on invalid value
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
    -> m (Dynamic t (Either e a))
    -> m (R.Event t (Either e a), R.Event t (Either e a), R.Event t ())
mkModalBodyUpdateAlways header footer body = do
  RD.divClass "modal-dialog" $ RD.divClass "modal-content" $ do
    dismiss <- header
    bodyRes <- RD.divClass "modal-body" body
    (cancel, ok) <- footer bodyRes
    let resE1 = R.tag (R.current bodyRes) ok
        closem1 = R.leftmost [dismiss, cancel]
    return (R.updated bodyRes, R.ffilter isRight resE1, closem1)


hiddenCSS :: M.Map T.Text T.Text
hiddenCSS  = "style" RD.=: "display: none !important"

visibleCSS :: M.Map T.Text T.Text
visibleCSS = "style" RD.=: "display: inline"

disableCSS :: M.Map T.Text T.Text
disableCSS = "disabled" RD.=: ""


dynStartingFrom :: (R.Reflex t, R.MonadHold t m) => R.Dynamic t a -> R.Event t a -> m (R.Dynamic t a)
dynStartingFrom d = R.buildDynamic (R.sample $ R.current d)


-- NB: This means that if d is updated and e fires in the same frame, the update to d will be the result here.
dynPlusEvent :: (R.Reflex t, R.MonadHold t m) => R.Dynamic t a -> R.Event t a -> m (R.Dynamic t a)
dynPlusEvent d e = R.buildDynamic (R.sample . R.current $ d) $ R.leftmost [R.updated d, e]


{-
data InnerModalEvs t a = InnerModal { updateOutput :: Event t a, modalClosed :: Event t (), updateInput :: Event t a }

-- | Modal Editor for a Dynamic a
-- | Widget as input takes an Event t () which will be fired when the modal is opened. This allows the widget to tag things.
modalEditorEither' :: forall t m e a. ( RD.DomBuilder t m
                                      , MonadWidgetExtraC t m
                                      , RD.PostBuild t m
                                      , MonadFix m
                                      , RD.MonadHold t m
                                      )
  => (Dynamic t (Maybe a) -> m (Dynamic t (Either e a))) -- a widget for editing an a. returns Left on invalid value.
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
-}

