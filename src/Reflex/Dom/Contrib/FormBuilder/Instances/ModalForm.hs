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
       ) where



--import qualified DataBuilder                           as B

import           Reflex.Dom.Contrib.FormBuilder.Builder
import           Reflex.Dom.Contrib.FormBuilder.Instances.Basic (FormInstanceC)
import           Reflex.Dom.Contrib.Widgets.ModalEditor         (ModalEditorConfig,
                                                                 modalEditor,
                                                                 modalEditor_value)

import qualified Reflex                                         as R
import qualified Reflex.Dom                                     as RD

import           Control.Lens                                   (view)
import           Data.Functor.Compose                           (Compose (Compose),
                                                                 getCompose)

newtype ModalForm a = ModalForm { unModalForm :: a } deriving (Functor)

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
