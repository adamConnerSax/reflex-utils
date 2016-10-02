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
       ) where

import           Control.Applicative                   (liftA2)
import           Control.Monad.Morph                   (hoist)
import           Control.Monad.Reader                  (ReaderT, ask, lift,
                                                        runReaderT)
import qualified Data.Map                              as M
import           Data.Maybe                            (isJust)
import           Data.Monoid                           ((<>))
import           Data.Validation                       (AccValidation (..))
-- for using the generic builder
import qualified GHC.Generics                          as GHCG

-- reflex imports
import qualified Reflex                                as R
import qualified Reflex.Dom                            as RD
import           Reflex.Dom.Contrib.Widgets.Common
import           Reflex.Dynamic.TH                     (mkDyn)

-- From this lib
import           Reflex.Dom.Contrib.Layout.Types       (CssClasses,
                                                        IsCssClass (..))

import qualified DataBuilder                           as B

import           Reflex.Dom.Contrib.SimpleForm.Builder


{-
-- This is not an isomorphism since there may be b's which have no analog as a's.  Injective but not nec. Surjective.
class EquivRep a b where
  to::a -> b
  from::b -> a

instance (SimpleFormC e t m,EquivRep a b, B.Builder (SimpleFormR e t m) b)=>Builder (SimpleFormR e t m) a where
  buildA mFN ma = from <$> buildA mFN (to <$> ma)
-}

instance (SimpleFormC e t m, B.Builder (SimpleFormR e t m) a)=>Builder (SimpleFormR e t m) (R.Dynamic t a) where
  buildA mFN mda = SimpleFormR $
    case mda of
      Nothing -> return dynValidationNothing
      Just aDyn -> do
        let builder::Maybe a->SimpleFormR e t m a
            builder = buildA mFN
            startDynM = DynValidation $ AccSuccess <$> aDyn
            builtDynM = (unSF . builder . Just) <$> aDyn -- Dynamic t (ReaderT e m (DynValidation t a))
        newDynEv <- RD.dyn builtDynM -- Event t (DynValidation a)
        dValidation <- joinDynOfDynValidation <$> R.foldDyn (\_ x-> x) startDynM newDynEv -- DynMaybe t a
        return $ fmap R.constDyn dValidation


newtype MWidget m a = MWidget { unMW::m a }

instance (SimpleFormC e t m, B.Builder (SimpleFormR e t m) a)=>Builder (SimpleFormR e t m) (MWidget m a) where
  buildA mFN mwa = SimpleFormR $
    case mwa of
      Nothing -> return dynValidationNothing
      Just wa -> do
        a <- lift $ unMW wa
        let builder::Maybe a->SimpleFormR e t m a
            builder = buildA mFN
        dva <- unSF $ builder (Just a)
        return $ fmap (MWidget . return) dva --R.mapDyn (maybe Nothing (Just . MWidget . return)) dma


