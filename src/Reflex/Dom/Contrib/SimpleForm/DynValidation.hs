module Reflex.Dom.Contrib.SimpleForm.DynValidation where

import           Reflex                          (Reflex,Dynamic,constDyn,zipDynWith)
import           Control.Monad                   (join)
import           Data.Validation                 (AccValidation (..))
import           Data.Text                       (Text)


data SimpleFormError  = SFNothing | SFNoParse Text | SFInvalid Text deriving (Show,Eq)

type SimpleFormErrors = [SimpleFormError]
  
accValidation::(err->b)->(a->b)->AccValidation err a->b
accValidation f _ (AccFailure e) = f e
accValidation _ f (AccSuccess a) = f a

avToMaybe::AccValidation e a->Maybe a
avToMaybe (AccFailure _) = Nothing
avToMaybe (AccSuccess a) = Just a

maybeToAV::Maybe a->AccValidation SimpleFormErrors a
maybeToAV Nothing = AccFailure [SFNothing]
maybeToAV (Just a) = AccSuccess a

newtype DynValidation t a = DynValidation { unDynValidation::Dynamic t (AccValidation SimpleFormErrors a) }

dynValidationNothing::Reflex t=>DynValidation t a
dynValidationNothing = DynValidation $ constDyn (AccFailure [SFNothing])

dynValidationErr::Reflex t=>SimpleFormErrors->DynValidation t a
dynValidationErr = DynValidation . constDyn . AccFailure

joinDynOfDynValidation::Reflex t =>Dynamic t (DynValidation t a) -> DynValidation t a
joinDynOfDynValidation = DynValidation . join . fmap unDynValidation

instance Reflex t=>Functor (DynValidation t) where
  fmap f dva = DynValidation $ fmap (fmap f) (unDynValidation dva)

instance Reflex t=>Applicative (DynValidation t) where
  pure = DynValidation . constDyn . AccSuccess
  dvf <*> dva = DynValidation $ zipDynWith (<*>) (unDynValidation dvf) (unDynValidation dva)

-- no Monad instance because it would fail to satisfy <*> = `ap' due to that being impossible for AccValidation
