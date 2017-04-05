{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Reflex.Dom.Contrib.FormBuilder.ListHoldFunctions
  (
  ) where

import qualified Reflex as R
import Reflex.Patch (PatchDMap(..),ComposeMaybe(..))
import qualified Reflex.Dom as RD

import qualified Data.Dependent.Map as DM

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Identity (Identity)
import Data.Functor.Misc (mapWithFunctorToDMap, dmapToMap, Const2)

import Data.Proxy (Proxy(..))


listHoldWithKey :: forall t m k v a. (Ord k, RD.DomBuilder t m, R.MonadHold t m)
  => Map k v -> R.Event t (Map k (Maybe v)) -> (k -> v -> m a) -> m (R.Dynamic t (Map k a))
listHoldWithKey m0 m' f = do
  let dm0 = mapWithFunctorToDMap $ Map.mapWithKey f m0
      dm' = fmap (PatchDMap . mapWithFunctorToDMap . Map.mapWithKey (\k v -> ComposeMaybe $ fmap (f k) v)) m'
  (a0, a') <- R.sequenceDMapWithAdjust dm0 dm'
  fmap dmapToMap . R.incrementalToDynamic <$> R.holdIncremental a0 a' --TODO: Move the dmapToMap to the righthand side so it doesn't get fully redone every time

-- Can we get rid of the need for proxies here?
class DMapRepresentable f k v a where
  type DMapKey f k a :: * -> *
  toDMapWithFunctor::Functor g=>(k->v->g a)->f v->DM.DMap (DMapKey f k a) g
  fromDMap::Proxy k->Proxy v->DM.DMap (DMapKey f k a) Identity -> f a
  
instance DMapRepresentable (Map k) k v a where
  type DMapKey (Map k) k a = Const2 k a
  toDMapWithFunctor h  = mapWithFunctorToDMap . Map.mapWithKey h 
  fromDMap _ _ = dmapToMap

-- we're specializing to Maybe here in order to use ComposeMaybe which seems to matter, though I don't get why.
-- Related to roles and a coerce.
instance DMapRepresentable f k v a => DMapRepresentable f k (Maybe v) a where
  DMapKey f k a = DMapKey f k a
  toDMapWithFunctor q = toDMapWithFunctor  
    
listHoldWithKeyGeneral::forall t m f k v a. (RD.DomBuilder t m, R.MonadHold t m, DMapRepresentable f k v a, DM.GCompare (DMapKey f k a))
  =>f v -> R.Event t (f (Maybe v)) -> (k->v-> m a) -> m (R.Dynamic t (f a))
listHoldWithKeyGeneral c0 c' f = do
  let dc0 = toDMapWithFunctor f c0
      dc' = fmap (PatchDMap . toDMapWithFunctor (\k v -> ComposeMaybe $ fmap (f k) v)) c'
      fromDM = fromDMap (Proxy :: Proxy k) (Proxy :: Proxy v)
  (a0,a') <- R.sequenceDMapWithAdjust dc0 dc'
  fmap fromDM . R.incrementalToDynamic <$> R.holdIncremental a0 a' --TODO: Move the dmapToMap to the righthand side so it doesn't get fully redone every time



  
