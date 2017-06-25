{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Reflex.Dom.Contrib.FormBuilder.FormEditor
  (
    Form
  , FormEditor
  , Editor (Editor)
  -- reexports
  , runEditor
  , transformEditor
  , (|<|)
  , (|>|)
  ) where


import           Reflex.Dom.Contrib.Editor                    (Combinable (..), Distributable (..),
                                                               Editor (Editor),
                                                               runEditor,
                                                               transformEditor,
                                                               (|<|), (|>|))

import           Reflex.Dom.Contrib.FormBuilder.Configuration (FR, FormValue, dynMaybeToFormValue)
import           Reflex.Dom.Contrib.FormBuilder.DynValidation (AccValidation (AccFailure, AccSuccess),
                                                               DynMaybe,
                                                               FormError (FNothing),
                                                               FormErrors,
                                                               accValidation,
                                                               avToMaybe,
                                                               dynValidationErr,
                                                               maybeToFV,
                                                               mergeAccValidation)
import           Reflex.Dom.Contrib.Widgets.WidgetResult      (constWidgetResult,
                                                               dynamicToWidgetResult,
                                                               dynamicWidgetResultToWidgetResult,
                                                               widgetResultToDynamic)
import           Reflex.Dynamic.FactorDyn                     (factorDyn')


import           Control.Lens                                 (Lens)
import           Control.Monad                                (join)
import           Control.Monad.Fix                            (MonadFix)
import           Control.Monad.Trans                          (lift)
import           Control.Monad.Trans.Reader                   (ask, runReaderT)
import           Data.Bifunctor                               (bimap)
import           Data.Functor.Compose                         (Compose (Compose),
                                                               getCompose)
import           Data.Profunctor                              (Choice (..), Profunctor (dimap),
                                                               Strong (..))
import qualified Generics.SOP                                 as SOP
import           GHC.Generics                                 (Generic)


import           Reflex                                       (Dynamic, Event,
                                                               MonadHold,
                                                               Reflex,
                                                               buildDynamic,
                                                               constDyn,
                                                               holdDyn)
import           Reflex.Dom                                   (DomBuilder,
                                                               PostBuild, dyn)


-- This is necessary because this functor and applicative are different from that of SFRW
-- NB: Form is *not* a Monad. So we do all the monadic widget building in (FRW t m a) and then wrap with makeForm
type Form t m = Compose (FR t m) (FormValue t)

type FormEditor t m a b = Editor (FormValue t) (Form t m) a b

-- Ditto for DynMaybe t m -> Form t m
-- NB: This one uses the instance for (FormValue t m) (Form t m) by upgrading the DynMaybe to a FormValue.
instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m) => Combinable (DynMaybe t) (Form t m) where
  combine :: DynMaybe t (Form t m a) -> Form t m a
  combine = combine . dynMaybeToFormValue

-- Ditto for FormValue t m -> Form t m
instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m) => Combinable (FormValue t) (Form t m) where
  combine :: FormValue t (Form t m a) -> Form t m a
  combine x = Compose $ do
    let fvfr2fr = Compose . fmap mergeAccValidation . sequenceA . fmap getCompose
    let x1 = widgetResultToDynamic . fmap (fmap fvfr2fr . sequenceA) . getCompose . fmap getCompose $ x -- Dynamic t ((FR t m) (FormValue t a))
    x2 <- dyn x1 -- Event t (FormValue t a)
    x4 <- holdDyn (constWidgetResult $ AccFailure [FNothing]) (getCompose <$> x2) -- Dynamic t (WidgetResult t (FValidation a))
    return $ Compose $ dynamicWidgetResultToWidgetResult x4


data AccValEither e a b = F e | SL a | SR b deriving (Generic)
instance SOP.Generic (AccValEither e a b)

toAccValEither :: AccValidation e (Either a b) -> AccValEither e a b
toAccValEither = accValidation F (either SL SR)

fromAccValEither :: AccValEither e a b -> AccValidation e (Either a b)
fromAccValEither (F x) = AccFailure x
fromAccValEither (SL x) = AccSuccess $ Left x
fromAccValEither (SR x) = AccSuccess $ Right x

factorAccValEither :: (Reflex t, MonadHold t m, MonadFix m) => Dynamic t (AccValEither e a b) -> m (Dynamic t (AccValEither (Dynamic t e) (Dynamic t a) (Dynamic t b)))
factorAccValEither = factorDyn'

instance (Reflex t, MonadHold t m, MonadFix m) => Distributable (FormValue t) m where
  distribute :: forall t a b m. (Reflex t, MonadHold t m, MonadFix m) => FormValue t (Either a b) -> m (FormValue t (Either (FormValue t a) (FormValue t b)))
  distribute x = do
    let x1 :: Dynamic t (AccValEither FormErrors a b)
        x1 = widgetResultToDynamic . fmap toAccValEither $ getCompose x
    x2 :: Dynamic t (AccValEither (Dynamic t FormErrors) (Dynamic t a) (Dynamic t b)) <- factorAccValEither x1
    let x3 :: Dynamic t (AccValidation (Dynamic t FormErrors) (Either (Dynamic t a) (Dynamic t b)))
        x3 = fromAccValEither <$> x2
        f :: AccValidation (Dynamic t e) (Either c d) -> Dynamic t (AccValidation e (Either c d))
        f x = case x of
          AccFailure de -> fmap AccFailure de
          AccSuccess y -> constDyn $ AccSuccess y
        x4 :: FormValue t (Either (Dynamic t a) (Dynamic t b))
        x4 = Compose $ dynamicToWidgetResult $ join $ fmap f x3
    return $ fmap (bimap (dynMaybeToFormValue . Compose . fmap pure) (dynMaybeToFormValue . Compose . fmap pure)) x4





