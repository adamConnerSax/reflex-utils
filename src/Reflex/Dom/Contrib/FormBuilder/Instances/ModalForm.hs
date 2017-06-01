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

newtype ModalForm e a = ModalForm { unModalForm :: a  } deriving (Functor)

class HasModalFormConfig t e a where
  modalConfig :: ModalEditorConfig t e a

instance ( HasModalFormConfig t e a
         , FormInstanceC t m
         , FormBuilder t m a
         ) => FormBuilder t m (ModalForm e a) where
  buildForm vMFA mFN dmMF  =
    let va = fmap unModalForm . vMFA . ModalForm
        modalWidget = fmap (fmap avToEither . unDynValidation) . unF . buildForm va mFN . Compose
        aMDyn = getCompose $ unModalForm <$> dmMF
    in makeForm $ fmap ModalForm . DynValidation . fmap eitherToAV . (view modalEditor_value) <$> modalEditorEither modalWidget aMDyn modalConfig

-- this just rearranges argument order and does the Dynamic t (Maybe a) <-> DynMaybe t a
modalizeWidget ::  ( RD.DomBuilder t m
                   , MonadWidgetExtraC t m
                   , RD.PostBuild t m
                   , MonadFix m
                   , RD.MonadHold t m
                   ) => ModalEditorConfig t a -> (DynMaybe t a -> m (DynValidation t a)) -> DynMaybe t a -> m (DynValidation t a)
modalizeWidget cfg w dma = DynValidation . fmap eitherToAv . view modalEditor_value <$> modalEditorEither (fmap (fmap avToEither . getCompose) . w . Compose) (getCompose dma) cfg

modalizeForm ::  ( RD.DomBuilder t m
                 , MonadWidgetExtraC t m
                 , RD.PostBuild t m
                 , MonadFix m
                 , RD.MonadHold t m
                 ) => ModalEditorConfig t a -> (DynMaybe t a -> Form t m a) -> DynMaybe t a -> Form t m a
modalizeForm cfg fw = makeForm . modalizeWidget cfg (unF . fw)


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
