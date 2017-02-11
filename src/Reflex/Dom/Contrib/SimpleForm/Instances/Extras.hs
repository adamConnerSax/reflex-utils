{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Reflex.Dom.Contrib.SimpleForm.Instances.Extras
       (
         MWidget(..)
       , buildValidated
       , buildValidatedIso
       ) where

import           Control.Lens                          (view)
import           Control.Lens.Iso                      (Iso', from, iso)
import           Control.Monad.Reader                  (lift)
import           Control.Monad.Fix                     (MonadFix)
import qualified Data.Text                             as T
import           Data.Validation                       (AccValidation (..))

-- reflex imports
import qualified Reflex                                as R
import qualified Reflex.Dom                            as RD

import qualified DataBuilder                           as B

import           Reflex.Dom.Contrib.SimpleForm.Builder


{-
-- This is not an isomorphism since there may be b's which have no analog as a's.  Injective but not nec. Surjective.
class EquivRep a b where
  to::a -> b
  from::b -> a

instance (SimpleFormC t m,EquivRep a b, B.Builder (SimpleFormR t m) b)=>Builder (SimpleFormR t m) a where
  buildA mFN ma = from <$> buildA mFN (to <$> ma)
-}

instance (SimpleFormC t m, RD.PostBuild t m, MonadFix m
         , B.Builder (SimpleFormR t m) a)=>Builder (SimpleFormR t m) (R.Dynamic t a) where
  buildA mFN mda = SimpleFormR $
    case mda of
      Nothing -> return dynValidationNothing
      Just aDyn -> do
        let builder::Maybe a->SimpleFormR t m a
            builder = buildA mFN
            startDynM = DynValidation $ AccSuccess <$> aDyn
            builtDynM = (unSF . builder . Just) <$> aDyn -- Dynamic t (ReaderT e m (DynValidation t a))
        newDynEv <- RD.dyn builtDynM -- Event t (DynValidation a)
        dValidation <- joinDynOfDynValidation <$> R.foldDyn (\_ x-> x) startDynM newDynEv -- DynMaybe t a
        return $ fmap R.constDyn dValidation


newtype MWidget m a = MWidget { unMW::m a }

instance (SimpleFormC t m, B.Builder (SimpleFormR t m) a)=>Builder (SimpleFormR t m) (MWidget m a) where
  buildA mFN mwa = SimpleFormR $
    case mwa of
      Nothing -> return dynValidationNothing
      Just wa -> do
        a <- lift $ unMW wa
        let builder::Maybe a->SimpleFormR t m a
            builder = buildA mFN
        dva <- unSF $ builder (Just a)
        return $ fmap (MWidget . return) dva --R.mapDyn (maybe Nothing (Just . MWidget . return)) dma

buildValidatedIso::forall t m a b.(SimpleFormC t m,
                                  Builder (SimpleFormR t m) b)=>
                (a->Either T.Text a)->
                Iso' a b->
                Maybe FieldName->
                Maybe a->
                SimpleFormR t m a
buildValidatedIso vf isoAB mFN ma =
  let mInitial = ma >>= either (const Nothing) (Just . view isoAB) . vf
      validatedX = either (\x->AccFailure [SFInvalid x]) AccSuccess . vf
      validatedAVX = accValidation AccFailure validatedX
      validatedDVX = DynValidation . fmap validatedAVX . unDynValidation
  in SimpleFormR $ validatedDVX <$> unSF (view (from isoAB) <$> buildA mFN mInitial)

buildValidated::forall t m a b.(SimpleFormC t m,
                                  Builder (SimpleFormR t m) b)=>
                (a->Either T.Text a)->
                (a->b)->
                (b->a)->
                Maybe FieldName->
                Maybe a->
                SimpleFormR t m a
buildValidated vf a2b b2a = buildValidatedIso vf (iso a2b b2a)
