{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Reflex.Dom.Contrib.SumType () where


-- FIXME:  make imports specific or qualify them
import           Generics.SOP            hiding (Compose)
import           Generics.SOP.Constraint (SListIN)
import           Generics.SOP.NP
import qualified GHC.Generics            as GHC

import           Data.Functor.Compose
import           Reflex
import           Reflex.Dom

{-
Trying to make a smart dynamic of sum type widget builder


1. Functor g=>g a -> NP (Compose g (Compose Maybe (NP I))) (Code a)  -- Done
2. NP (Compose (Compose (Dynamic t) Maybe) (NP I)) (Code a) -> NP (Compose (Event t) (NP I)) (Code a)
3. NP (Compose (Event t) (NP I)) (Code a) -> NP (Compose K (m (Event t a))) xs -- ?? xs::'[a,a,...] length of number of constructors
4. NP (Compose K (m (Event t a))) xs -> (Event t Int, [m (Event t a)]) where the first event tells you which of the second is active.
5. (Event t Int,[m (Event t a)]) -> m (Event t a) -- could use widgetHold or visibility here
6. m (Event t a) -> m (Dynamic t a)

-}


-- utilities

whichFired::Reflex t=>[Event t a]->Event t Int
whichFired = leftmost . zipWith (\n e -> n <$ e) [1..]


expand::forall f xs.(SListI xs)=>NS f xs -> NP (Compose Maybe f) xs
expand ns = go sList (Just ns) where
  go::forall ys.SListI ys => SList ys -> Maybe (NS f ys) -> NP (Compose Maybe f) ys
  go SNil _ = Nil
  go SCons mNS = case mNS of
    Nothing -> Compose Nothing :* go sList Nothing -- after Z
    Just ns -> case ns of
      Z fx -> Compose (Just fx) :* go sList Nothing -- at Z
      S ns' -> Compose Nothing :* go sList (Just ns') -- before Z


-- why doesn't the function above work?
expandNSNP::SListI xs=>NS (NP I) xs -> NP (Compose Maybe (NP I)) xs
expandNSNP ns = go sList (Just ns) where
  go::forall (f :: [*] -> *) ys.SListI ys => SList ys -> Maybe (NS f ys) -> NP (Compose Maybe f) ys
  go SNil _ = Nil
  go SCons mNS = case mNS of
    Nothing -> Compose Nothing :* go sList Nothing -- after Z
    Just ns -> case ns of
      Z fx -> Compose (Just fx) :* go sList Nothing -- at Z
      S ns' -> Compose Nothing :* go sList (Just ns') -- before Z


expandA::Generic a=>a->NP (Compose Maybe (NP I)) (Code a)
expandA = expandNSNP . unSOP . from

type WrappedProjection (g :: * -> *) (f :: k -> *) (xs :: [k]) = K (g (NP f xs)) -.-> Compose g f

wrappedProjections::forall xs g f.(Functor g,SListI xs) => NP (WrappedProjection g f xs) xs
wrappedProjections = case sList :: SList xs of
  SNil -> Nil
  SCons -> fn (Compose . fmap hd . unK) :* liftA_NP shiftWrappedProjection wrappedProjections

shiftWrappedProjection :: Functor g=>WrappedProjection g f xs a -> WrappedProjection g f (x ': xs) a
shiftWrappedProjection (Fn f) = Fn $ f . K . fmap tl . unK

type WrappedInjection (g :: * -> *) (f :: k -> *) (xs :: [k]) = Compose g f -.-> K (g (NS f xs))

wrappedInjections::forall xs g f. (Functor g, SListI xs) => NP (WrappedInjection g f xs) xs
wrappedInjections = case sList :: SList xs of
  SNil   -> Nil
  SCons  -> fn (K . fmap Z . getCompose) :* liftA_NP shiftWrappedInjection wrappedInjections

shiftWrappedInjection:: Functor g=>WrappedInjection g f xs a -> WrappedInjection g f (x ': xs) a
shiftWrappedInjection (Fn f) = Fn $ K . fmap S . unK . f

-- steps
-- This was a doozy...
functorToMaybeNP::forall g a xss.(Functor g,Generic a)=>g a -> NP (Compose g (Compose Maybe (NP I))) (Code a)
functorToMaybeNP ga = hap wrappedProjections (hpure $ K (expandA <$> ga))

-- should "updated" in the below be dynAsEv?
dynMaybeNPToEventNP::(Reflex t, SListI2 xss)=> NP (Compose (Dynamic t) (Compose Maybe (NP I))) xss -> NP (Compose (Event t) (NP I)) xss
dynMaybeNPToEventNP = hmap (Compose . fmapMaybe id . fmap getCompose . updated . getCompose)

-- this would have been a doozy but wasn't because step 1 was already this doozy
eventNPToEventNSNP::(Reflex t, SListI2 xss)=> NP (Compose (Event t) (NP I)) xss -> NP (K ((Event t) (NS (NP I) xss))) xss
eventNPToEventNSNP = hap wrappedInjections

-- SIDEBAR

eventNSNPToEvent::(Reflex t, SListI2 xss,Generic a, xss ~ Code a) =>NP (K ((Event t) (NS (NP I) xss))) xss -> NP (K (Event t a)) xss
eventNSNPToEvent = hliftA (K  . fmap (to . SOP) . unK)

npEventsToList::SListI2 xss=>NP (K (Event t a)) xss -> [Event t a]
npEventsToList = hcollapse


-- NB: we now have Dynamic t a -> NP (K (Event t a)) xss, a constructor indexed product of per-constructor events
dynamicToNPEvent::(Reflex t, Generic a)=>Dynamic t a -> NP (K (Event t a)) (Code a)
dynamicToNPEvent = eventNSNPToEvent . eventNPToEventNSNP . dynMaybeNPToEventNP . functorToMaybeNP


-- NB: we now have Dynamic t a -> [Event t a] where each is only for one constructor
eventPerConstructor::(Reflex t, Generic a)=>Dynamic t a -> [Event t a]
eventPerConstructor = npEventsToList . dynamicToNPEvent

-- END SIDEBAR



