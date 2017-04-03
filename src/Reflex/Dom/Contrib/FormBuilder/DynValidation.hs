{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Reflex.Dom.Contrib.FormBuilder.DynValidation where

import           Control.Monad     (join)
import           Data.Text         (Text)
import           Data.Validation   (AccValidation (..))
import           DataBuilder.Types (MonadLike (..), MaybeLike(..))
import           Reflex            (Dynamic, Event,Reflex, constDyn, zipDynWith)
import           Reflex.Dom        (PostBuild)
import           GHC.Generics      (Generic)
import           Data.Functor.Compose (Compose(Compose,getCompose))

import           Reflex.Dom.Contrib.DynamicUtils (dynamicMaybeAsEv)


type DynMaybe t = Compose (Dynamic t) Maybe

constDynMaybe::Reflex t=>Maybe a->DynMaybe t a
constDynMaybe = Compose . constDyn 

dynMaybeAsEv::PostBuild t m=>DynMaybe t a -> m (Event t a)
dynMaybeAsEv = dynamicMaybeAsEv . getCompose

data FormError  = FNothing | FNoParse Text | FInvalid Text deriving (Show,Eq,Generic)

type FormErrors = [FormError]

type FValidation = AccValidation FormErrors

accValidation::(err->b)->(a->b)->AccValidation err a->b
accValidation f _ (AccFailure e) = f e
accValidation _ f (AccSuccess a) = f a

avToMaybe::AccValidation e a->Maybe a
avToMaybe (AccFailure _) = Nothing
avToMaybe (AccSuccess a) = Just a

maybeToAV::Maybe a->AccValidation FormErrors a
maybeToAV Nothing = AccFailure [FNothing]
maybeToAV (Just a) = AccSuccess a

mergeAccValidation::AccValidation e (AccValidation e a) -> AccValidation e a
mergeAccValidation x = case x of
  AccSuccess y -> y
  AccFailure errs -> AccFailure errs

newtype DynValidation t a = DynValidation { unDynValidation::Dynamic t (FValidation a) }

-- NB: this is isomorphic to Compose (Dynamic t) (AccValidation SimpleFormErrors a)
{-
type  DynValidation t a = Compose (Dynamic t) (AccValidation a)
unDynValidation::DynValidation t a->Dynamic t (AccValidation a)
unDynValidation = getCompose

makeDynValidation::Dynamic t (AccValidation a) -> DynValidation t a
makeDynValidation = Compose
-}

dynValidationNothing::Reflex t=>DynValidation t a
dynValidationNothing = DynValidation $ constDyn (AccFailure [FNothing])

constDynValidation::Reflex t=>a->DynValidation t a
constDynValidation a  = DynValidation $ constDyn (AccSuccess a)

dynValidationErr::Reflex t=>FormErrors->DynValidation t a
dynValidationErr = DynValidation . constDyn . AccFailure

joinDynOfDynValidation::Reflex t =>Dynamic t (DynValidation t a) -> DynValidation t a
joinDynOfDynValidation = DynValidation . join . fmap unDynValidation

instance Reflex t=>Functor (DynValidation t) where
  fmap f dva = DynValidation $ fmap (fmap f) (unDynValidation dva)

instance Reflex t=>Applicative (DynValidation t) where
  pure = DynValidation . constDyn . AccSuccess
  dvf <*> dva = DynValidation $ zipDynWith (<*>) (unDynValidation dvf) (unDynValidation dva)

instance MonadLike FValidation where
  pureLike = pure
  joinLike = mergeAccValidation

instance MaybeLike FValidation where
  absorbMaybe = mergeAccValidation . fmap maybeToAV
  toMaybe = avToMaybe
