{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Reflex.Dom.Contrib.SumType
  (
    DynMaybe
  , dynamicToEventList
  , dynMaybeToEventList
  , whichFired
  , ConWidget(..)
  , DynMBuildable(..)
  , AllDynMBuildable
  , dynamicToConWidgets
  , dynMaybeToConWidgets
  , dynamicToWidgetEvent
  , dynMaybeToWidgetEvent
  , NatAt(..)
  , mapFieldsAndSequence'
  , Generic
  , HasDatatypeInfo
  , All2
  , Code
  ) where



import           Control.Monad               (join)
import           Data.Functor.Compose
import           Data.Functor.Identity       (runIdentity)

import           Generics.SOP                (All2, Code, Generic,
                                              HasDatatypeInfo, SListI, hmap)
import           Generics.SOP.NP             (NP, sequence'_NP)

import           Generics.SOP.DMapUtilities
import           Generics.SOP.PerConstructor

import           Reflex                      (Dynamic, Event, Reflex,
                                              distributeDMapOverDynPure,
                                              fmapMaybe, leftmost, updated)


type DynMaybe t = Compose (Dynamic t) Maybe

dynamicToEventList::(Reflex t, Generic a)=>Dynamic t a -> [Event t a]
dynamicToEventList = functorToPerConstructorList (hmap dynIsConToEvent)

dynIsConToEvent::forall a t (f :: k -> *).Reflex t=>((Dynamic t :.: Maybe) :.: f) a -> (Event t :.: f) a
dynIsConToEvent = Comp . fmapMaybe id . updated . unComp . unComp

dynMaybeToEventList::(Reflex t, Generic a)=>DynMaybe t a -> [Event t a]
dynMaybeToEventList = functorToPerConstructorList (hmap dynMaybeIsConToEvent)

dynMaybeIsConToEvent::forall a t (f :: k -> *).Reflex t=>((DynMaybe t :.: Maybe) :.: f) a -> (Event t :.: f) a
dynMaybeIsConToEvent = Comp . fmapMaybe join . updated . getCompose . unComp . unComp

whichFired::Reflex t=>[Event t a]->Event t Int
whichFired = leftmost . zipWith (<$) [0..]

data ConWidget t m a = ConWidget { conName::ConstructorName, switchedTo::Event t a, widget::m (DynMaybe t a) }

class DynMBuildable t m a where
  dynMBuild::DynMaybe t a -> m (DynMaybe t a)

instance (Functor m, DynMBuildable t m a)=>NatAt (DynMaybe t) (Compose m (DynMaybe t)) a where
  eta = Compose . dynMBuild

type AllDynMBuildable t m a = (All2 (DynMBuildable t m) (Code a))

type MapAndSequenceDynMaybeFields t m a = MapFieldsAndSequence (DynMaybe t) (Compose m (DynMaybe t)) (Code a)

{-
f1::(Applicative m, SListI xs)=>NP (Compose m (DynMaybe t)) xs -> m (NP (Dynamic t) mxs)
f1 = npUnCompose . hmap (Comp . getCompose)

f2::NP (Dynamic t :.: Maybe) xs -> DM.DMap (TypeListTag xs) (Dynamic t Maybe)
f2 = fmap unComp . npToDMap

f3::DM.DMap (TypeListTag xs) (Dynamic t Maybe) -> DynMaybe t (NP I xs)
f3 = Compose . fmap (hsequence . dMapToNP) . distributeDMapOverDynPure

--sequenceWidgets::POP (Compose m (DynMaybe t)) xss -> NP ((Compose m (DynMaybe t)) :.: NP I) xss
--sequenceWidgets =  hmap f . unPOP


{-
widgetPerFieldAndSequence::(Generic a
                           , HasDatatypeInfo a
                           , AllDynMBuildable t m a)=>MapAndSequenceDynMaybeFields t m a
widgetPerFieldAndSequence =
  let buildC = Proxy :: Proxy (DynMBuildable t m)
  in . unPOP . hcliftA buildC dynMBuild
-}
-}

--fixMSDMF::MapAndSequenceDynMaybeFields t m a -> MapFieldsAndSequence (Dynamic t :.: Maybe) (Compose m (DynMaybe t)) (Code a)
fixMSDMF x = x . hmap (Compose . unComp)

dynamicToConWidgets::(Reflex t, Generic a,HasDatatypeInfo a, Applicative m)
  =>MapAndSequenceDynMaybeFields t m a->Dynamic t a-> [ConWidget t m a]
dynamicToConWidgets mapAndS dynA =
  let switchEvents = dynamicToEventList dynA
      mapFsAndS = fixMSDMF mapAndS
      namedWidgets = functorDoPerConstructorWithNames mapFsAndS dynA
  in zipWith (\ev (n,w) -> ConWidget n ev (getCompose w)) switchEvents namedWidgets

joinComposedMaybes::Reflex t=>(DynMaybe t :.: Maybe) a -> (Dynamic t :.: Maybe) a
joinComposedMaybes = Comp . fmap join . getCompose . unComp

dynMaybeToConWidgets::(Reflex t, Generic a,HasDatatypeInfo a, Applicative m)
  =>MapAndSequenceDynMaybeFields t m a->DynMaybe t a-> [ConWidget t m a]
dynMaybeToConWidgets mapAndS dynMA =
  let switchEvents = dynMaybeToEventList dynMA
      mapFsAndS = fixMSDMF mapAndS
      namedWidgets = functorDoPerConstructorWithNames (mapFsAndS . hmap joinComposedMaybes) dynMA
  in zipWith (\ev (n,w) -> ConWidget n ev (getCompose w)) switchEvents namedWidgets


dynamicToWidgetEvent::(Reflex t, Generic a,HasDatatypeInfo a, Applicative m)
  =>MapAndSequenceDynMaybeFields t m a->Dynamic t a->Event t (m (DynMaybe t a))
dynamicToWidgetEvent mapAndS dynA =
  let conWidgets = dynamicToConWidgets mapAndS dynA
      f (ConWidget _ ev w) = w <$ ev
  in leftmost $ f <$> conWidgets

dynMaybeToWidgetEvent::(Reflex t, Generic a,HasDatatypeInfo a, Applicative m)
  =>MapAndSequenceDynMaybeFields t m a->DynMaybe t a->Event t (m (DynMaybe t a))
dynMaybeToWidgetEvent mapAndS dynMA =
  let conWidgets = dynMaybeToConWidgets mapAndS dynMA
      f (ConWidget _ ev w) = w <$ ev
  in leftmost $ f <$> conWidgets

