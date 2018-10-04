{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
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
--  , OnExternalChange (..)
  , UpdateOutput (..)
  , ButtonConfig (..)
  , button_label
  , button_attributes
  , button_iconClass
--  , removeIfTrue
  , disableAndDisplayIfError
  , ModalEditorConfig (..)
  , modalEditor_attributes
  , modalEditor_closeOnNewInput
  , modalEditor_updateOutput
  , modalEditor_closeOnOk
  , modalEditor_openButton
  , modalEditor_XButton
  , modalEditor_OkButton
  , modalEditor_CancelButton
  , modalEditorMaybe
  , modalEditorEither
  ) where

import           Reflex                                  (Dynamic, Event,
                                                          MonadHold, Reflex)
import qualified Reflex                                  as R
import qualified Reflex.Dom                              as RD

import           Reflex.Dynamic                          (constDyn)

import qualified Reflex.Dom.Contrib.Layout.FlexLayout    as L
import qualified Reflex.Dom.Contrib.Layout.Types         as L
import           Reflex.Dom.Contrib.ReflexConstraints    (MonadWidgetExtraC)
import           Reflex.Dom.Contrib.Widgets.WidgetResult (WidgetResult,
                                                          currentWidgetResult,
                                                          unsafeBuildWidgetResult,
                                                          unsafeBuildWrappedWidgetResult,
                                                          updatedWidgetResult,
                                                          widgetResultToDynamic)


import           Control.Arrow                           ((***))
import           Control.Lens                            (makeLenses, preview,
                                                          view, (%~), (&), (^.),
                                                          _Left, _Right)
import           Control.Monad.Fix                       (MonadFix)
import           Data.Default
import           Data.Either                             (isRight)
import           Data.Functor.Compose                    (Compose (Compose))
import           Data.Kind                               (Type)
import qualified Data.List                               as L
import qualified Data.Map                                as M
import           Data.Maybe                              (mapMaybe)
import           Data.Monoid                             ((<>))
import qualified Data.Text                               as T
import           Safe                                    (headMay)

data ModalEditor t e a = ModalEditor { _modalEditor_value  :: Dynamic t (Either e a)
                                     , _modalEditor_change :: Event t a -- only fire when there is a valid value
                                     }

--makeLenses ''ModalEditor
modalEditor_value :: ModalEditor t e a -> Dynamic t (Either e a)
modalEditor_value = _modalEditor_value

modalEditor_change :: ModalEditor t e a -> Event t a
modalEditor_change = _modalEditor_change

modalEditor_mValue :: Reflex t => ModalEditor t g a -> Dynamic t (Maybe a)
modalEditor_mValue = fmap e2m . modalEditor_value

modalEditor_WidgetResult :: Reflex t => ModalEditor t e a -> Compose (WidgetResult t) (Either e) a
modalEditor_WidgetResult me = Compose $ unsafeBuildWidgetResult (modalEditor_value me) (Right <$> modalEditor_change me)


e2m :: Either e a -> Maybe a
e2m = either (const Nothing) Just

m2e :: Maybe a -> Either () a
m2e = maybe (Left ()) Right


data UpdateOutput = Always | OnOk deriving (Eq)

data ButtonConfig = ButtonConfig { _button_label      :: T.Text
                                 , _button_attributes :: M.Map T.Text T.Text
                                 , _button_iconClass  :: Maybe T.Text
                                 }

makeLenses ''ButtonConfig

-- widget could go in here but has no sensible default except "return"
data ModalEditorConfig t e a = ModalEditorConfig { _modalEditor_attributes      :: Dynamic t (M.Map T.Text T.Text)
                                                 , _modalEditor_closeOnNewInput :: Bool
                                                 , _modalEditor_updateOutput    :: UpdateOutput
                                                 , _modalEditor_closeOnOk       :: Bool
                                                 , _modalEditor_openButton      :: Either e a -> ButtonConfig
                                                 , _modalEditor_XButton         :: Maybe (Either e a -> ButtonConfig)
                                                 , _modalEditor_OkButton        :: Either e a -> ButtonConfig
                                                 , _modalEditor_CancelButton    :: Either e a -> ButtonConfig
                                                 }

makeLenses ''ModalEditorConfig

removeIfError :: Either e a -> ButtonConfig -> ButtonConfig
removeIfError (Left _) (ButtonConfig l attrs ic) = ButtonConfig l (M.union hiddenCSS attrs) ic
removeIfError _ x = x

disableAndDisplayIfError :: (e -> T.Text) -> Either e a -> ButtonConfig -> ButtonConfig
disableAndDisplayIfError toText (Left x) (ButtonConfig _ attrs _) = ButtonConfig (toText x) (M.union disableCSS attrs) Nothing
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
        True -- close on change to input dynamic
        OnOk -- Only update output dynamic or fire change event when "ok" is pressed.
        True -- close on OK
        (const $ ButtonConfig "Edit" M.empty Nothing) -- default to Edit button which is disabled when input is Nothing
        Nothing -- default to no "x" button in the header
        (const $ ButtonConfig "OK" M.empty Nothing) -- simple OK button in footer
        (const $ ButtonConfig "Close" M.empty Nothing) -- simple Cancel button in footer

data ViewState = Button | Editor deriving (Eq)

data ModalState :: Type -> Type -> Type where
  ModalOpen :: Either e a -> Either e a -> ModalState e a -- ifDismissed output
  ModalClosed :: Either e a -> ModalState e a -- output

modalOutput :: ModalState e a  -> Either e a
modalOutput (ModalOpen _ x) = x
modalOutput (ModalClosed x) = x

modalViewState :: ModalState e a  -> ViewState
modalViewState (ModalOpen _ _) = Editor
modalViewState (ModalClosed _) = Button

data ModalEvent :: Type -> Type -> Type where
  UpdateCurrent :: Either e a -> ModalEvent e a
  UpdateRevert  :: Either e a -> ModalEvent e a
  Open          :: ModalEvent e a
  Close         :: ModalEvent e a

updateModalState :: ModalState e a -> ModalEvent e a  -> ModalState e a
updateModalState (ModalClosed _)        (UpdateCurrent eaC)         = ModalClosed eaC
updateModalState (ModalOpen eaR _)      (UpdateCurrent eaC)         = ModalOpen eaR eaC
updateModalState (ModalOpen _ eaC)      (UpdateRevert eaR)          = ModalOpen eaR eaC
updateModalState (ModalClosed eaC)      Open                        = ModalOpen eaC eaC
updateModalState (ModalOpen eaR _)      Close                       = ModalClosed eaR
updateModalState _ _ = error "updateModalState called with bad arguments." -- we could disallow these with with types but it gets messy.

modalEditorOpenButton :: (Reflex t, RD.DomBuilder t m, RD.PostBuild t m)
  => ModalEditorConfig t e a
  -> Dynamic t (Either e a)
  -> m (R.Event t [ModalEvent e a])
modalEditorOpenButton config eaDyn = do
  let openButtonConfigOrig = (config ^. modalEditor_openButton) <$> eaDyn
      openButtonConfig = R.zipDynWith (\bc va -> bc & button_attributes %~ M.union va) openButtonConfigOrig
      updateInputEv = UpdateCurrent <$> R.updated eaDyn
  openEv <- fmap (const Open) <$> (dynamicButton $ openButtonConfig (constDyn M.empty))
  return $ (pure <$> R.leftmost [updateInputEv, openEv])

openModalWidget :: ( Reflex t
                   , RD.DomBuilder t m
                   , MonadWidgetExtraC t m
                   , RD.PostBuild t m
                   , MonadFix m
                   , RD.MonadHold t m
                   )
  => ModalEditorConfig t e a
  -> (Dynamic t (Maybe a) -> m (Dynamic t (Either e a)))
  -> Dynamic t (Either e a)
  -> m (R.Event t [ModalEvent e a], R.Event t a)
openModalWidget config editW eaDyn = do
  let header d = maybe (return R.never) (\f -> L.flexFill L.LayoutLeft $ dynamicButton $ f <$> d) $ (config ^. modalEditor_XButton)
      footer d = L.flexRow $ do
        okEv <- L.flexFill L.LayoutRight $ dynamicButton ((config ^. modalEditor_OkButton) <$> d)
        cancelEv' <- L.flexFill L.LayoutLeft $ dynamicButton ((config ^. modalEditor_CancelButton) <$> d)
        return (cancelEv', okEv)
      modalAttrsDyn = R.zipDynWith (M.unionWith (\c1 c2 -> c1 <> " " <> c2)) (config ^. modalEditor_attributes) (R.constDyn ("class" RD.=: "modal-dialog"))
      closeWith x = [Close, UpdateCurrent x]
      updateBothWith x = [UpdateRevert x, UpdateCurrent x]
  RD.elDynAttr "div" modalAttrsDyn $ RD.divClass "modal-content" $ do
    rec dismiss <- header bodyDyn
        bodyDyn <- L.flexItem $ RD.divClass "modal-body" $ editW $ e2m <$> eaDyn
    (cancel, ok) <- footer bodyDyn
    let okValueEv = Right <$> R.attachWithMaybe (\e _ -> preview _Right e)  (R.current bodyDyn) ok
        okEvs = (if (config ^. modalEditor_closeOnOk) then closeWith else updateBothWith) <$> okValueEv
        editEvs = if (config ^. modalEditor_updateOutput) == Always then pure . UpdateCurrent  <$> R.updated bodyDyn else R.never
        dismissEvs = [Close] <$ R.leftmost [cancel,dismiss]
        newInputEvs = (if (config ^. modalEditor_closeOnNewInput) then closeWith else updateBothWith) <$> R.updated eaDyn
        changeEv = R.fmapMaybe e2m $ if config ^. modalEditor_updateOutput == Always then R.updated bodyDyn else okValueEv
    return $ (R.leftmost [newInputEvs, okEvs, dismissEvs, editEvs], changeEv)

-- | Modal Editor for a Dynamic a
modalEditorEither :: ( Reflex t
                     , RD.DomBuilder t m
                     , MonadWidgetExtraC t m
                     , RD.PostBuild t m
                     , MonadFix m
                     , RD.MonadHold t m
                     )
  => (Dynamic t (Maybe a) -> m (Dynamic t (Either e a))) -- a widget for editing an a. returns Left on invalid value.
  -> R.Dynamic t (Either e a)
  -> ModalEditorConfig t e a
  -> m (ModalEditor t e a)
modalEditorEither editW eaDyn config = do
  let selF xDyn vs = case vs of
        Button -> (,) <$> modalEditorOpenButton config xDyn <*> pure R.never
        Editor -> openModalWidget config editW xDyn
      updateState = L.foldl' updateModalState
      evDynToEv = R.switch . R.current
  rec let newStateEv = R.attachWith updateState (R.current stateDyn) modalEditorUpdateEvs
      stateDyn <- R.buildDynamic (R.sample . R.current $ fmap ModalClosed eaDyn) newStateEv
      (modalEditorUpdateEvs, modalChangeEv) <- (evDynToEv *** evDynToEv) . R.splitDynPure <$> RD.widgetHold (selF eaDyn Button) (selF eaDyn <$> viewEv)
      let viewEv = modalViewState <$> R.updated stateDyn
  return $ ModalEditor (modalOutput <$> stateDyn) modalChangeEv

modalEditorMaybe :: ( RD.DomBuilder t m
                    , MonadWidgetExtraC t m
                    , RD.PostBuild t m
                    , MonadFix m
                    , RD.MonadHold t m
                    )
  => (Dynamic t (Maybe a) -> m (Dynamic t (Maybe a))) -- a widget for editing an a. returns Left on invalid value
  -> R.Dynamic t (Maybe a)
  -> ModalEditorConfig t () a
  -> m (ModalEditor t () a)
modalEditorMaybe editW maDyn config = modalEditorEither (fmap (fmap m2e) . editW) (m2e <$> maDyn) config

hiddenCSS :: M.Map T.Text T.Text
hiddenCSS  = "style" RD.=: "display: none !important"

visibleCSS :: M.Map T.Text T.Text
visibleCSS = "style" RD.=: "display: inline"

disableCSS :: M.Map T.Text T.Text
disableCSS = "disabled" RD.=: ""

