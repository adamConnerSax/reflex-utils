{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Dom.Contrib.SimpleForm.Instances.Extras () where

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

-- This is not an isomorphism since there may be b's which have no analog as a's.  Injective but not nec. Surjective.
class EquivRep a b where
  to::a -> b
  from::b -> a

instance (SimpleFormC e t m,EquivRep a b, B.Builder (SimpleFormR e t m) b)=>Builder (SimpleFormR e t m) a where
  buildA md ma = from <$> buildA (to <$> ma)

instance (SimpleFormC e t m, GHCG.Generic a)=>

