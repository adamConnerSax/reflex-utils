{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Reflex.Dom.Contrib.SimpleForm.DynValidation where

import           Control.Monad     (join)
--import           Data.Functor.Compose (Compose (..))
import           Data.Text         (Text)
import           Data.Validation   (AccValidation (..))
import           DataBuilder.Types (MonadLike (..))
import           Reflex            (Dynamic, Reflex, constDyn, zipDynWith, never)
import           GHC.Generics      (Generic)

data SimpleFormError  = SFNothing | SFNoParse Text | SFInvalid Text deriving (Show,Eq,Generic)

type SimpleFormErrors = [SimpleFormError]

type SFValidation = AccValidation SimpleFormErrors

accValidation::(err->b)->(a->b)->AccValidation err a->b
accValidation f _ (AccFailure e) = f e
accValidation _ f (AccSuccess a) = f a

avToMaybe::AccValidation e a->Maybe a
avToMaybe (AccFailure _) = Nothing
avToMaybe (AccSuccess a) = Just a

maybeToAV::Maybe a->AccValidation SimpleFormErrors a
maybeToAV Nothing = AccFailure [SFNothing]
maybeToAV (Just a) = AccSuccess a

mergeAccValidation::AccValidation e (AccValidation e a) -> AccValidation e a
mergeAccValidation x = case x of
  AccSuccess y -> y
  AccFailure errs -> AccFailure errs

newtype DynValidation t a = DynValidation { unDynValidation::Dynamic t (SFValidation a) }
-- NB: this is isomorphic to Compose (Dynamic t) (AccValidation SimpleFormErrors a)
{-
type  DynValidation t a = Compose (Dynamic t) (AccValidation a)
unDynValidation::DynValidation t a->Dynamic t (AccValidation a)
unDynValidation = getCompose

makeDynValidation::Dynamic t (AccValidation a) -> DynValidation t a
makeDynValidation = Compose
-}

dynValidationNothing::Reflex t=>DynValidation t a
dynValidationNothing = DynValidation $ constDyn (AccFailure [SFNothing])

constDynValidation::Reflex t=>a->DynValidation t a
constDynValidation a  = DynValidation $ constDyn (AccSuccess a)

dynValidationErr::Reflex t=>SimpleFormErrors->DynValidation t a
dynValidationErr = DynValidation . constDyn . AccFailure

joinDynOfDynValidation::Reflex t =>Dynamic t (DynValidation t a) -> DynValidation t a
joinDynOfDynValidation = DynValidation . join . fmap unDynValidation

instance Reflex t=>Functor (DynValidation t) where
  fmap f dva = DynValidation $ fmap (fmap f) (unDynValidation dva)

instance Reflex t=>Applicative (DynValidation t) where
  pure = DynValidation . constDyn . AccSuccess
  dvf <*> dva = DynValidation $ zipDynWith (<*>) (unDynValidation dvf) (unDynValidation dva)


instance MonadLike SFValidation where
  pureLike = pure
  joinLike = mergeAccValidation

