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
--         buildValidatedDynamic
--       , MWidget(..)
--       , buildValidatedIso'
--       , buildValidatedIso
       ) where
{-
import           Control.Lens                          (view)
import           Control.Lens.Iso                      (Iso', from, iso)
import Control.Monad (join)
import           Control.Monad.Fix                     (MonadFix)
import           Control.Monad.Reader                  (lift)
import qualified Data.Text                             as T
import           Data.Validation                       (AccValidation (..))

-- reflex imports
import qualified Reflex                                as R
import qualified Reflex.Dom                            as RD

import qualified DataBuilder                           as B

import           Reflex.Dom.Contrib.SimpleForm.Builder
import           Reflex.Dom.Contrib.SimpleForm.Instances.Basic (VFormBuilderC)


-- This is not an isomorphism since there may be b's which have no analog as a's.  Injective but not nec. Surjective.
class EquivRep a b where
  to::a -> b
  from::b -> a

instance (SimpleFormC t m,EquivRep a b, B.Builder (SimpleFormR t m) b)=>Builder (SimpleFormR t m) a where
  buildA mFN ma = from <$> buildA mFN (to <$> ma)
-}

{-
buildValidatedDynamic'::forall t m a.(SimpleFormC t m, RD.PostBuild t m, MonadFix m, VBuilderC t m a)=>FormValidator (DynValidation t) a -> Maybe FieldName -> Maybe (R.Dynamic t a) -> SimpleFormR t m a
buildValidatedDynamic' va mFN mda = B.validateFV va . makeSimpleFormR $ do
  let inputDyn = maybe (R.constDyn Nothing) (fmap Just) mda -- Dynamic t (Maybe a)
      builderDyn = unSF . B.buildA mFN <$> inputDyn -- Dynamic t (SFRW t m a)
  builderDVEv <- fmap unDynValidation <$> RD.dyn builderDyn --Event t (Dynamic t (AccValidation t a))
  avDyn <- join <$> R.holdDyn (maybeToAV <$> inputDyn) builderDVEv -- Dynamic t (Dynamic t (AccValidation t a))
  return $ DynValidation avDyn


buildValidatedDynamic::forall t m a.(SimpleFormC t m, RD.PostBuild t m, MonadFix m, VBuilderC t m a)=>B.Validator (DynValidation t) a -> Maybe FieldName -> Maybe (R.Dynamic t a) -> SimpleFormR t m a
buildValidatedDynamic va mFN mda = B.validateFV va . makeSimpleFormR $ do
  let inputDyn = maybe (R.constDyn Nothing) (fmap Just) mda -- Dynamic t (Maybe a)
      builderDyn = unSF . B.buildA mFN <$> inputDyn -- Dynamic t (SFRW t m a)

  builderEvEv <- fmap (R.updated . unDynValidation) <$> RD.dyn builderDyn --Event t (Event t (AccValidation a))
  builderEvBeh <- R.hold R.never builderEvEv -- Behavior t (Event t (AccValidation a))
  let builderEv = R.traceEventWith (const "builderEv") $ R.switch builderEvBeh -- Event t (AccValidation a)
--  postbuild <- RD.getPostBuild
--  let initialEv = R.attachWith const (R.current (maybeToAV <$> inputDyn)) postbuild
  avDyn <- R.holdDyn (AccFailure [SFNothing]) $ R.leftmost [builderEv] --,initialEv]
  return $ DynValidation avDyn
  
instance (SimpleFormC t m, RD.PostBuild t m, MonadFix m, VBuilderC t m a)=>Builder (SFR t m) (DynValidation t) (R.Dynamic t a) where
  buildValidated va mFN mda = B.validateFV va $ R.constDyn <$> buildValidatedDynamic B.validate mFN mda

newtype MWidget m a = MWidget { unMW::m a }

instance (SimpleFormC t m, VBuilderC t m a)=>Builder (SFR t m) (DynValidation t) (MWidget m a) where
  buildValidated va mFN mwa = B.validateFV va . makeSimpleFormR $
    case mwa of
      Nothing -> return dynValidationNothing
      Just wa -> do
        a <- lift $ unMW wa
        let builder::Maybe a->SimpleFormR t m a
            builder = B.buildA mFN
        dva <- unSF $ builder (Just a)
        return $ fmap (MWidget . return) dva --R.mapDyn (maybe Nothing (Just . MWidget . return)) dma


makeValF::R.Reflex t=>(a->Either T.Text a)->B.Validator (DynValidation t) a
makeValF f a = DynValidation $ case f a of
  Left e -> R.constDyn (AccFailure [SFInvalid e])
  Right a -> R.constDyn (AccSuccess a)

isoMapValF::Functor f=>Iso' a b->B.Validator f a -> B.Validator f b
isoMapValF iso vfa =  (\b -> view iso <$> vfa (view (from iso) b))

buildValidatedIso::forall t m a b.(SimpleFormC t m,Builder (SFR t m) (DynValidation t) b)=>
                   (a->Either T.Text a)->
                   Iso' a b->
                   Maybe FieldName->
                   Maybe a->
                   SimpleFormR t m a
buildValidatedIso vf isoAB mFN ma =
  let valFa = makeValF vf
      valFb = isoMapValF isoAB valFa
      mInitial = ma >>= either (const Nothing) (Just . view isoAB) . vf
{-
      validatedX = either (\x->AccFailure [SFInvalid x]) AccSuccess . vf
      validatedAVX = accValidation AccFailure validatedX
      validatedDVX = DynValidation . fmap validatedAVX . unDynValidation
-}
  in makeSimpleFormR $ unSF (view (from isoAB) <$> buildValidated valFb mFN mInitial)

buildValidatedIso'::forall t m a b.(SimpleFormC t m,Builder (SFR t m) (DynValidation t) b)=>
                (a->Either T.Text a)->
                (a->b)->
                (b->a)->
                Maybe FieldName->
                Maybe a->
                SimpleFormR t m a
buildValidatedIso' vf a2b b2a = buildValidatedIso vf (iso a2b b2a)
-}
