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
import           Reflex.Dom.Contrib.FormBuilder.Configuration   (getFC)
import           Reflex.Dom.Contrib.FormBuilder.Instances.Basic (FormInstanceC)
import           Reflex.Dom.Contrib.ReflexConstraints           (MonadWidgetExtraC)
import           Reflex.Dom.Contrib.Widgets.ModalEditor         (ModalEditorConfig,
                                                                 modalEditor,
                                                                 modalEditorEither,
                                                                 modalEditor_WidgetResult)
import           Reflex.Dom.Contrib.Widgets.WidgetResult        (transformWrappedWidgetResult,
                                                                 widgetResultToDynamic)

import qualified Reflex                                         as R
import qualified Reflex.Dom                                     as RD

import           Control.Lens                                   (view)
import           Control.Monad                                  (join)
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
  buildForm vMFA mFN dmMF  =
    let va = fmap unModalForm . vMFA . ModalForm
        modalWidget = fmap (fmap avToEither . getCompose) . unF . buildForm va mFN . Compose  -- FIXME
        aEDyn = maybeToEitherFE <$> (getCompose $ unModalForm <$> dmMF)
    in makeForm $ fmap ModalForm . Compose . fmap eitherToAV . getCompose . modalEditor_WidgetResult <$> modalEditorEither modalWidget aEDyn modalConfig


-- this just rearranges argument order and does the Dynamic t (Maybe a) <-> DynMaybe t a
-- also commits to e ~ FormErrors
modalizeWidget ::  ( RD.DomBuilder t m
                   , MonadWidgetExtraC t m
                   , RD.PostBuild t m
                   , MonadFix m
                   , RD.MonadHold t m
                   ) => ModalEditorConfig t FormErrors a -> (DynMaybe t a -> m (FormResult t a)) -> DynMaybe t a -> m (FormResult t a)
modalizeWidget cfg w dma =
  let matchedWidget = fmap (fmap avToEither . getCompose) . w . Compose
      matchedInput = maybeToEitherFE <$> getCompose dma
      matchOutput = transformWrappedWidgetResult eitherToAV . modalEditor_WidgetResult
  in matchOutput <$> modalEditorEither matchedWidget matchedInput cfg

modalizeEditor ::  ( RD.DomBuilder t m
                   , MonadWidgetExtraC t m
                   , RD.PostBuild t m
                   , MonadFix m
                   , RD.MonadHold t m
                 ) => ModalEditorConfig t FormErrors a -> DynEditor t m a a -> DynEditor t m a a
modalizeEditor cfg e = DynEditor $ makeForm . modalizeWidget cfg (unF . runDynEditor e)

modalEditField :: (FormInstanceC t m, VFormBuilderC t m a) => ModalEditorConfig t FormErrors a -> DynEditor t m a a --DynMaybe t a -> Form t m a
modalEditField cfg = modalizeEditor cfg (editField Nothing)

maybeToEitherFE :: Maybe a -> Either FormErrors a
maybeToEitherFE = maybe (Left [FNothing]) Right

{-
modalizeFormField :: (FormInstanceC t m, VFormBuilderC t m a) => ModalEditorConfig t a -> DynMaybe t a -> Form t m a
modalizeFormField meCfg dma =  makeForm $ do
  formCfg <- getFC
  let meWidget ma = fmap avToMaybe . unDynValidation <$> (unF $ buildVForm Nothing (Compose ma))
  aMDyn <- view modalEditor_value <$> modalEditor meWidget (getCompose dma) meCfg
  return $ DynValidation $ fmap maybeToAV aMDyn
-}
