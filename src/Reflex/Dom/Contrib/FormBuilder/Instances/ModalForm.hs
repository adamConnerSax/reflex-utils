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
       , modalizeForm
       , modalizeFormField
       ) where



--import qualified DataBuilder                           as B

import           Reflex.Dom.Contrib.FormBuilder.Builder
import           Reflex.Dom.Contrib.FormBuilder.Configuration   (getFC)
import           Reflex.Dom.Contrib.FormBuilder.Instances.Basic (FormInstanceC)
import           Reflex.Dom.Contrib.ReflexConstraints           (MonadWidgetExtraC)
import           Reflex.Dom.Contrib.Widgets.ModalEditor         (ModalEditorConfig,
                                                                 modalEditor,
                                                                 modalEditor_value)

import qualified Reflex                                         as R
import qualified Reflex.Dom                                     as RD

import           Control.Lens                                   (view)
import           Control.Monad                                  (join)
import           Control.Monad.Fix                              (MonadFix)
import           Data.Functor.Compose                           (Compose (Compose),
                                                                 getCompose)

newtype ModalForm a = ModalForm { unModalForm :: a  } deriving (Functor)

class HasModalFormConfig t a where
  modalConfig :: ModalEditorConfig t a

instance ( HasModalFormConfig t a
         , FormInstanceC t m
         , FormBuilder t m a
         ) => FormBuilder t m (ModalForm a) where
  buildForm vMFA mFN dmMF  =
    let va = fmap unModalForm . vMFA . ModalForm
        modalWidget = fmap (getCompose . dynValidationToDynMaybe) . unF . buildForm va mFN . Compose
        aMDyn = getCompose $ unModalForm <$> dmMF
    in makeForm $ fmap ModalForm . dynMaybeToDynValidation . Compose . (view modalEditor_value) <$> modalEditor modalWidget aMDyn modalConfig

-- this just rearranges argument order and does the Dynamic t (Maybe a) <-> DynMaybe t a
modalizeWidget ::  ( RD.DomBuilder t m
                   , MonadWidgetExtraC t m
                   , RD.PostBuild t m
                   , MonadFix m
                   , RD.MonadHold t m
                   ) => ModalEditorConfig t a -> (DynMaybe t a -> m (DynMaybe t a)) -> DynMaybe t a -> m (DynMaybe t a)
modalizeWidget cfg w dma = Compose . view modalEditor_value <$> modalEditor (fmap getCompose . w . Compose) (getCompose dma) cfg

modalizeForm ::  ( RD.DomBuilder t m
                 , MonadWidgetExtraC t m
                 , RD.PostBuild t m
                 , MonadFix m
                 , RD.MonadHold t m
                 ) => ModalEditorConfig t a -> (DynMaybe t a -> Form t m a) -> DynMaybe t a -> Form t m a
modalizeForm cfg fw = makeForm . fmap dynMaybeToDynValidation . modalizeWidget cfg (fmap dynValidationToDynMaybe . unF . fw)


modalizeFormField :: (FormInstanceC t m, VFormBuilderC t m a) => ModalEditorConfig t a -> DynMaybe t a -> Form t m a
modalizeFormField cfg = modalizeForm cfg (buildVForm Nothing)

{-
modalizeFormField :: (FormInstanceC t m, VFormBuilderC t m a) => ModalEditorConfig t a -> DynMaybe t a -> Form t m a
modalizeFormField meCfg dma =  makeForm $ do
  formCfg <- getFC
  let meWidget ma = fmap avToMaybe . unDynValidation <$> (unF $ buildVForm Nothing (Compose ma))
  aMDyn <- view modalEditor_value <$> modalEditor meWidget (getCompose dma) meCfg
  return $ DynValidation $ fmap maybeToAV aMDyn
-}
