{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Reflex.Dom.Contrib.FormBuilder.Instances.ModalForm
       (
         ModalForm (..)
       , HasModalFormConfig (..)
       , modalizeWidget
       , modalizeEditor
       , modalEditField
       ) where



--import qualified DataBuilder                           as B

import           Reflex.Dom.Contrib.FormBuilder.Builder
import           Reflex.Dom.Contrib.FormBuilder.Instances.Basic (FormInstanceC)
import           Reflex.Dom.Contrib.ReflexConstraints           (MonadWidgetExtraC)
import           Reflex.Dom.Contrib.Widgets.ModalEditor         (ModalEditorConfig,
                                                                 modalEditorEither,
                                                                 modalEditor_WidgetResult)
import           Reflex.Dom.Contrib.Widgets.WidgetResult        (transformWrappedWidgetResult,
                                                                 widgetResultToDynamic)

import qualified Reflex.Dom                                     as RD

import           Control.Monad.Fix                              (MonadFix)
import           Data.Functor.Compose                           (Compose (Compose),
                                                                 getCompose)

newtype ModalForm a = ModalForm { unModalForm :: a } deriving (Functor)

class HasModalFormConfig t a where
  modalConfig :: ModalEditorConfig t FormErrors a

instance ( HasModalFormConfig t a
         , FormInstanceC t m
         , FormBuilder t m a
         ) => FormBuilder t m (ModalForm a) where
  buildForm vMFA mFN fvMF  =
    let va = fmap unModalForm . vMFA . ModalForm
        modalWidget = fmap (fmap avToEither . getCompose) . unF . buildForm va mFN . dynMaybeToFormValue . Compose -- FIXME
        aEDyn = widgetResultToDynamic $ fValToEitherFE <$> (getCompose $ unModalForm <$> fvMF)
    in makeForm $ fmap ModalForm . Compose . fmap eitherToAV . getCompose . modalEditor_WidgetResult <$> modalEditorEither modalWidget aEDyn modalConfig


-- this just rearranges argument order and does the Dynamic t (Maybe a) <-> DynMaybe t a
-- also commits to e ~ FormErrors
modalizeWidget ::  ( RD.DomBuilder t m
                   , MonadWidgetExtraC t m
                   , RD.PostBuild t m
                   , MonadFix m
                   , RD.MonadHold t m
                   ) => ModalEditorConfig t FormErrors a -> (FormValue t a -> m (FormValue t a)) -> FormValue t a -> m (FormValue t a)
modalizeWidget cfg w fva =
  let matchedWidget = fmap (fmap avToEither . getCompose) . w . dynMaybeToFormValue . Compose
      matchedInput = widgetResultToDynamic $ fValToEitherFE <$> getCompose fva
      matchOutput = transformWrappedWidgetResult eitherToAV . modalEditor_WidgetResult
  in matchOutput <$> modalEditorEither matchedWidget matchedInput cfg

modalizeEditor ::  ( RD.DomBuilder t m
                   , MonadWidgetExtraC t m
                   , RD.PostBuild t m
                   , MonadFix m
                   , RD.MonadHold t m
                 ) => ModalEditorConfig t FormErrors a -> FormEditor t m a a -> FormEditor t m a a
modalizeEditor cfg e = Editor $ makeForm . modalizeWidget cfg (unF . runEditor e)

modalEditField :: (FormInstanceC t m, VFormBuilderC t m a) => ModalEditorConfig t FormErrors a -> FormEditor t m a a
modalEditField cfg = modalizeEditor cfg (editField Nothing)

--maybeToEitherFE :: Maybe a -> Either FormErrors a
--maybeToEitherFE = maybe (Left [FNothing]) Right

fValToEitherFE :: FValidation a -> Either FormErrors a
fValToEitherFE = accValidation Left Right

{-
modalizeFormField :: (FormInstanceC t m, VFormBuilderC t m a) => ModalEditorConfig t a -> DynMaybe t a -> Form t m a
modalizeFormField meCfg dma =  makeForm $ do
  formCfg <- getFC
  let meWidget ma = fmap avToMaybe . unDynValidation <$> (unF $ buildVForm Nothing (Compose ma))
  aMDyn <- view modalEditor_value <$> modalEditor meWidget (getCompose dma) meCfg
  return $ DynValidation $ fmap maybeToAV aMDyn
-}
