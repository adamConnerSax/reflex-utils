{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
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
module Reflex.Dom.Contrib.SumType where


-- FIXME:  make imports specific or qualify them
import           Control.Monad (join)
import           Generics.SOP  hiding (Compose)
import           Reflex

-- utilities

newtype IsCon a = IsCon { unIsCon::Maybe a} deriving (Functor,Applicative,Monad,Show)

notCon::IsCon a
notCon = IsCon Nothing

con::a -> IsCon a
con = IsCon . Just

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure x= Comp (pure (pure x))
  fgF <*> fgA = Comp $ (<*>) <$> unComp fgF <*> unComp fgA

expand::forall (f :: [k] -> *) xs.(SListI xs)=>NS f xs -> NP (IsCon :.: f) xs
expand ns = go sList (Just ns) where
  go::forall ys.SListI ys => SList ys -> Maybe (NS f ys) -> NP (IsCon :.: f) ys
  go SNil _ = Nil
  go SCons mNS = case mNS of
    Nothing -> Comp notCon :* go sList Nothing -- after Z
    Just ms -> case ms of
      Z fx -> Comp (con fx) :* go sList Nothing -- at Z
      S ms' -> Comp notCon :* go sList (Just ms') -- before Z


expandA::Generic a=>a->NP (IsCon :.: NP I) (Code a)
expandA = expand . unSOP . from

type WrappedProjection (g :: * -> *) (f :: k -> *) (xs :: [k]) = K (g (NP f xs)) -.-> g :.: f

wrappedProjections::forall xs g f.(Functor g,SListI xs) => NP (WrappedProjection g f xs) xs
wrappedProjections = case sList :: SList xs of
  SNil -> Nil
  SCons -> fn (Comp . fmap hd . unK) :* hliftA shiftWrappedProjection wrappedProjections

shiftWrappedProjection :: Functor g=>WrappedProjection g f xs a -> WrappedProjection g f (x ': xs) a
shiftWrappedProjection (Fn f) = Fn $ f . K . fmap tl . unK

type WrappedInjection (g :: * -> *) (f :: k -> *) (xs :: [k]) = g :.: f -.-> K (g (NS f xs))

wrappedInjections::forall xs g f. (Functor g, SListI xs) => NP (WrappedInjection g f xs) xs
wrappedInjections = case sList :: SList xs of
  SNil   -> Nil
  SCons  -> fn (K . fmap Z . unComp) :* hliftA shiftWrappedInjection wrappedInjections

shiftWrappedInjection:: Functor g=>WrappedInjection g f xs a -> WrappedInjection g f (x ': xs) a
shiftWrappedInjection (Fn f) = Fn $ K . fmap S . unK . f

-- NB: For applicative h, this is an inverse of hsequence.  If h is not applicative, then this is not invertible.
distribute::(Functor h, SListI xs)=>h (NP g xs) -> NP (h :.: g) xs
distribute x = hap wrappedProjections (hpure $ K x)

distributeI::(Functor h, SListI xs)=>h (NP I xs) -> NP h xs
distributeI = hmap (fmap unI . unComp) . distribute

functorToIsConNP::forall g a.(Functor g,Generic a)=>g a -> NP (g :.: (IsCon :.: NP I)) (Code a)
functorToIsConNP ga = hap wrappedProjections (hpure $ K (expandA <$> ga))

reAssociate::Functor g=>(g :.: (f :.: h)) a -> ((g :.: f) :.: h) a
reAssociate = Comp . Comp . fmap unComp . unComp

reAssociateNP::(Functor g, SListI xss)=>NP (g :.: (f :.: h)) xss->NP ((g :.: f) :.: h) xss
reAssociateNP = hmap reAssociate

isConNPToPOPIsCon::(Functor g, SListI2 xss)=>NP ((g :.: IsCon) :.: NP I) xss -> POP (g :.: IsCon) xss
isConNPToPOPIsCon =
  let proxyC = Proxy :: Proxy SListI
  in POP . hcliftA proxyC (distributeI . unComp)

reconstructA::(Functor h, Generic a) => NP (h :.: NP I) (Code a) -> NP (K (h a)) (Code a)
reconstructA = hliftA (K . fmap (to . SOP) . unK) . hap wrappedInjections

type DoAndSequence f h xss = POP f xss -> NP (h :.: NP I) xss

doAndSequence::(Applicative h, SListI2 xss)=>(forall a.f a->h a) -> DoAndSequence f h xss --POP f xss -> NP (h :.: (NP I)) xss
doAndSequence q =
  let sListIC = Proxy :: Proxy SListI
  in hcliftA sListIC (Comp . hsequence) . unPOP . hliftA q

class Map (f :: k -> *) (h :: k -> *) (a :: k) where
  doMap::f a -> h a

doAndSequence'::forall f h xss.(Applicative h, SListI2 xss, All2 (Map f h) xss)=>DoAndSequence f h xss --POP f xss -> NP (h :.: (NP I)) xss
doAndSequence' =
  let sListIC = Proxy :: Proxy SListI
      mapIC = Proxy :: Proxy (Map f h)
  in hcliftA sListIC (Comp . hsequence) . unPOP . hcliftA mapIC doMap


{-
This is the heart of it.  Take a functorial value (Functor g=>g a) and a natural transformation (?), (g :.: IsCon) ~> h, and you get
a list of [Functor h=>h a], one per constructor of a.
(g :.: IsCon) is isomorphic to (g :.: Maybe)
-}
type TransformEach g h xss = NP ((g :.: IsCon) :.: NP I) xss -> NP (h :.: NP I) xss

functorToPerConstructorList::(Generic a, Functor g, Functor h)=>TransformEach g h (Code a)->g a->[h a]  -- one per constructor
functorToPerConstructorList transform = hcollapse . reconstructA . transform . reAssociateNP . functorToIsConNP

functorDoPerConstructor::(Generic a, Functor g, Applicative h)=>DoAndSequence (g :.: IsCon) h (Code a)->g a->[h a]  -- one per constructor
functorDoPerConstructor doAndS = functorToPerConstructorList (doAndS . isConNPToPOPIsCon)

functorDoPerConstructor'::forall a g h.(Generic a, HasDatatypeInfo a, Functor g, Applicative h)
    =>DoAndSequence (g :.: IsCon) h (Code a)
    ->g a
    ->[(ConstructorName,h a)]  -- one per constructor
functorDoPerConstructor' doAndS ga =
  let conNames = hcollapse . hliftA (K . constructorName) . constructorInfo $ datatypeInfo (Proxy :: Proxy a)  -- [ConstructorName]
  in zip conNames (functorDoPerConstructor doAndS ga)

-- now Reflex specific
dynamicToEventList::(Reflex t, Generic a)=>Dynamic t a -> [Event t a]
dynamicToEventList = functorToPerConstructorList (hmap dynIsConToEvent)

dynIsConToEvent::forall a t (f :: k -> *).Reflex t=>((Dynamic t :.: IsCon) :.: f) a -> (Event t :.: f) a
dynIsConToEvent = Comp . fmapMaybe unIsCon . updated . unComp . unComp

dynMaybeToEventList::(Reflex t, Generic a)=>(Dynamic t :.: Maybe) a -> [Event t a]
dynMaybeToEventList = functorToPerConstructorList (hmap dynMaybeIsConToEvent)

dynMaybeIsConToEvent::forall a t (f :: k -> *).Reflex t=>(((Dynamic t :.: Maybe) :.: IsCon) :.: f) a -> (Event t :.: f) a
dynMaybeIsConToEvent = Comp . fmapMaybe (join . fmap unIsCon) . updated . unComp . unComp . unComp

whichFired::Reflex t=>[Event t a]->Event t Int
whichFired = leftmost . zipWith (<$) [0..]

data ConWidget t m a = ConWidget { conName::ConstructorName, switchedTo::Event t a, widget::(m :.: (Dynamic t :.: Maybe)) a }

dynamicToConWidgets::(Reflex t, Generic a,HasDatatypeInfo a, Applicative m)
  =>DoAndSequence (Dynamic t :.: IsCon) (m :.: (Dynamic t :.: Maybe)) (Code a)
  ->Dynamic t a
  -> [ConWidget t m a]
dynamicToConWidgets doAndS dynA =
  let switchEvents = dynamicToEventList dynA
      namedWidgets = functorDoPerConstructor' doAndS dynA
  in zipWith (\ev (n,w) -> ConWidget n ev w) switchEvents namedWidgets


joinMaybeIsCon::Functor g=>((g :.: Maybe) :.: IsCon) a -> (g :.: IsCon) a
joinMaybeIsCon = Comp . fmap (IsCon . join . fmap unIsCon) . unComp . unComp

maybeDynToConWidgets::(Reflex t, Generic a,HasDatatypeInfo a, Applicative m)
  =>DoAndSequence (Dynamic t :.: IsCon) (m :.: (Dynamic t :.: Maybe)) (Code a)
  ->(Dynamic t :.: Maybe) a
  -> [ConWidget t m a]
maybeDynToConWidgets doAndS dynMA =
  let switchEvents = dynMaybeToEventList dynMA
      namedWidgets = functorDoPerConstructor' (doAndS . hmap joinMaybeIsCon) dynMA
  in zipWith (\ev (n,w) -> ConWidget n ev w) switchEvents namedWidgets


dynamicToWidgetEvent::(Reflex t, Generic a,HasDatatypeInfo a, Applicative m)
  =>DoAndSequence (Dynamic t :.: IsCon) (m :.: (Dynamic t :.: Maybe)) (Code a)
  ->Dynamic t a
  ->Event t ((m :.: (Dynamic t :.: Maybe)) a)
dynamicToWidgetEvent doAndS dynA =
  let conWidgets = dynamicToConWidgets doAndS dynA
      f (ConWidget _ ev w) = w <$ ev
  in leftmost $ f <$> conWidgets


maybeDynToWidgetEvent::(Reflex t, Generic a,HasDatatypeInfo a, Applicative m)
  =>DoAndSequence (Dynamic t :.: IsCon) (m :.: (Dynamic t :.: Maybe)) (Code a)
  ->(Dynamic t :.: Maybe) a
  ->Event t ((m :.: (Dynamic t :.: Maybe)) a)
maybeDynToWidgetEvent doAndS dynMA =
  let conWidgets = maybeDynToConWidgets doAndS dynMA
      f (ConWidget _ ev w) = w <$ ev
  in leftmost $ f <$> conWidgets

