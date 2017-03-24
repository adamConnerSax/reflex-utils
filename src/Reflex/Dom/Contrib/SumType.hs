{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
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

newtype IsCon a = IsCon { unIsCon::Maybe a} deriving (Functor,Applicative,Monad,Show)

notCon::IsCon a
notCon = IsCon Nothing

con::a -> IsCon a
con = IsCon . Just

expand::forall f xs.(SListI xs)=>NS f xs -> NP (IsCon :.: f) xs
expand ns = go sList (Just ns) where
  go::forall ys.SListI ys => SList ys -> Maybe (NS f ys) -> NP (IsCon :.: f) ys
  go SNil _ = Nil
  go SCons mNS = case mNS of
    Nothing -> Comp notCon :* go sList Nothing -- after Z
    Just ns -> case ns of
      Z fx -> Comp (con fx) :* go sList Nothing -- at Z
      S ns' -> Comp notCon :* go sList (Just ns') -- before Z


-- why doesn't the function above work?
expandNSNP::SListI xs=>NS (NP I) xs -> NP (IsCon :.: (NP I)) xs
expandNSNP ns = go sList (Just ns) where
  go::forall (f :: [*] -> *) ys.SListI ys => SList ys -> Maybe (NS f ys) -> NP (IsCon :.: f) ys
  go SNil _ = Nil
  go SCons mNS = case mNS of
    Nothing -> Comp notCon :* go sList Nothing -- after Z
    Just ns -> case ns of
      Z fx -> Comp (con fx) :* go sList Nothing -- at Z
      S ns' -> Comp notCon :* go sList (Just ns') -- before Z


expandA::Generic a=>a->NP (IsCon :.: (NP I)) (Code a)
expandA = expandNSNP . unSOP . from

type WrappedProjection (g :: * -> *) (f :: k -> *) (xs :: [k]) = K (g (NP f xs)) -.-> g :.: f

wrappedProjections::forall xs g f.(Functor g,SListI xs) => NP (WrappedProjection g f xs) xs
wrappedProjections = case sList :: SList xs of
  SNil -> Nil
  SCons -> fn (Comp . fmap hd . unK) :* liftA_NP shiftWrappedProjection wrappedProjections

shiftWrappedProjection :: Functor g=>WrappedProjection g f xs a -> WrappedProjection g f (x ': xs) a
shiftWrappedProjection (Fn f) = Fn $ f . K . fmap tl . unK

type WrappedInjection (g :: * -> *) (f :: k -> *) (xs :: [k]) = g :.: f -.-> K (g (NS f xs))

wrappedInjections::forall xs g f. (Functor g, SListI xs) => NP (WrappedInjection g f xs) xs
wrappedInjections = case sList :: SList xs of
  SNil   -> Nil
  SCons  -> fn (K . fmap Z . unComp) :* liftA_NP shiftWrappedInjection wrappedInjections

shiftWrappedInjection:: Functor g=>WrappedInjection g f xs a -> WrappedInjection g f (x ': xs) a
shiftWrappedInjection (Fn f) = Fn $ K . fmap S . unK . f

-- NB: For applicative h, this is an inverse of hsequence.  If h is not applicative, then this is not invertible.
distribute::(Functor h, SListI xs)=>h (NP g xs) -> NP (h :.: g) xs
distribute x = hap wrappedProjections (hpure $ K x)

distributeI::(Functor h, SListI xs)=>h (NP I xs) -> NP h xs
distributeI = hliftA (fmap unI . unComp) . distribute

functorToIsConNP::forall g a xss.(Functor g,Generic a)=>g a -> NP (g :.: (IsCon :.: (NP I))) (Code a)
functorToIsConNP ga = hap wrappedProjections (hpure $ K (expandA <$> ga))

reAssociate::Functor g=>(g :.: (f :.: h)) a -> ((g :.: f) :.: h) a
reAssociate = Comp . Comp . fmap unComp . unComp

reAssociateNP::(Functor g, SListI xss)=>NP (g :.: (f :.: h)) xss->NP ((g :.: f) :.: h) xss
reAssociateNP = hmap reAssociate

isConNPToPOPIsCon::(Functor g, SListI2 xss)=>NP (g :.: (IsCon :.: (NP I))) xss -> POP (g :.: IsCon) xss
isConNPToPOPIsCon =
  let proxyC = Proxy :: Proxy (SListI)
  in POP . hcliftA proxyC (distributeI . unComp . reAssociate)

f1::SListI2 xss=>(forall a.f a -> h a) -> POP f xss -> POP h xss
f1 = hliftA


f2::(Applicative h, SListI2 xss)=>(forall a.f a -> h a) -> POP f xss -> NP (h :.: (NP I)) xss
f2 q = . hliftA q 



-- should "updated" in the below be dynAsEv?
dynIsConNPToEventNP::(Reflex t, SListI2 xss)=> NP ((Dynamic t) :.: (IsCon :.: (NP I))) xss -> NP ((Event t) :.: (NP I)) xss
dynIsConNPToEventNP = hmap (Comp . fmapMaybe unIsCon . fmap unComp . updated . unComp)

-- this would have been a doozy but wasn't because step 1 was already this doozy
eventNPToEventNSNP::(Reflex t, SListI2 xss)=> NP ((Event t) :.: (NP I)) xss -> NP (K ((Event t) (NS (NP I) xss))) xss
eventNPToEventNSNP = hap wrappedInjections

-- SIDEBAR

eventNSNPToEvent::(Reflex t, SListI2 xss,Generic a, xss ~ Code a) =>NP (K ((Event t) (NS (NP I) xss))) xss -> NP (K (Event t a)) xss
eventNSNPToEvent = hliftA (K  . fmap (to . SOP) . unK)

npEventsToList::SListI2 xss=>NP (K (Event t a)) xss -> [Event t a]
npEventsToList = hcollapse


-- NB: we now have Dynamic t a -> NP (K (Event t a)) xss, a constructor indexed product of per-constructor events
dynamicToNPEvent::(Reflex t, Generic a)=>Dynamic t a -> NP (K (Event t a)) (Code a)
dynamicToNPEvent = eventNSNPToEvent . eventNPToEventNSNP . dynIsConNPToEventNP . functorToIsConNP


-- NB: we now have Dynamic t a -> [Event t a] where each is only for one constructor
eventPerConstructor::(Reflex t, Generic a)=>Dynamic t a -> [Event t a]
eventPerConstructor = npEventsToList . dynamicToNPEvent

-- END SIDEBAR



