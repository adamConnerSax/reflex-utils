{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Reflex.Dom.Contrib.SimpleForm.Instances.Extras
       (
         MWidget(..)
       ) where

import Control.Applicative (liftA2)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.Morph (hoist)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Maybe (isJust)

-- for using the generic builder
import qualified GHC.Generics as GHCG

-- reflex imports
import qualified Reflex as R 
import qualified Reflex.Dom as RD
import Reflex.Dynamic.TH (mkDyn)
import Reflex.Dom.Contrib.Widgets.Common --(HtmlWidget,combineWidgets)

-- From this lib
import Reflex.Dom.Contrib.Layout.Types (CssClasses,IsCssClass(..))

import qualified DataBuilder as B

import Reflex.Dom.Contrib.SimpleForm.Builder


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
      Nothing -> return $ R.constDyn Nothing 
      Just aDyn -> do
        let builder::Maybe a->SimpleFormR e t m a
            builder = buildA mFN
        startDyn <- R.mapDyn Just aDyn -- DynMaybe t a
        builtDyn <- R.mapDyn (unSF . builder) startDyn -- Dynamic t (SimpleFormR e t m (DynMaybe t a))
        newDynEv <- RD.dyn builtDyn -- Event t (DynMaybe t a)
        dMaybe <- R.joinDyn <$> R.foldDyn (\_ x-> x) startDyn newDynEv -- DynMaybe t a
        lift $ R.mapDyn (maybe Nothing (Just . R.constDyn)) dMaybe


newtype MWidget m a = MWidget { unMW::m a }

instance (SimpleFormC e t m, B.Builder (SimpleFormR e t m) a)=>Builder (SimpleFormR e t m) (MWidget m a) where
  buildA mFN mwa = SimpleFormR $ 
    case mwa of
      Nothing -> return $ R.constDyn Nothing 
      Just wa -> do
        a <- lift $ unMW wa
        let builder::Maybe a->SimpleFormR e t m a
            builder = buildA mFN
        dma <- unSF $ builder (Just a)
        lift $ R.mapDyn (maybe Nothing (Just . MWidget . return)) dma 


