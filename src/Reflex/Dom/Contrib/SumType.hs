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
import           Generics.SOP            hiding (Compose)
import           Generics.SOP.Constraint (And, SListIN)
import           Generics.SOP.NP
import qualified GHC.Generics            as GHC
--import           Data.Type.Equality
--import           Data.Type.Bool
import           Control.Monad           (join)
import           Data.Functor.Compose
import           Reflex
import           Reflex.Dom

-- utilities

whichFired::Reflex t=>[Event t a]->Event t Int
whichFired = leftmost . zipWith (\n e -> n <$ e) [1..]

newtype IsCon a = IsCon { unIsCon::Maybe a} deriving (Functor,Applicative,Monad,Show)

notCon::IsCon a
notCon = IsCon Nothing

con::a -> IsCon a
con = IsCon . Just


instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure x= Comp (pure (pure x))
  fgF <*> fgA = Comp $ ((<*>) <$> (unComp fgF) <*> (unComp fgA))

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

functorToIsConNP::forall g a.(Functor g,Generic a)=>g a -> NP (g :.: (IsCon :.: (NP I))) (Code a)
functorToIsConNP ga = hap wrappedProjections (hpure $ K (expandA <$> ga))

{-
functorMaybeToIsConNP::forall g a.(Functor g,Generic a)=>(g :.: Maybe) a -> NP (g :.: (IsCon :.: (NP I))) (Code a)
functorMaybeToIsConNP =
  let joinMIC::Maybe (IsCon a) -> IsCon a
      joinMIC = IsCon . join . fmap unIsCon
      q::SListI xs=>((g :.: Maybe) :.: (IsCon :.: (NP I))) xs -> (g :.: (IsCon :.: (NP I))) xs
      q = Comp . fmap Comp . hmap (fmap joinMIC) . unComp . fmap unComp . unComp
  in hmap q . functorToIsConNP
-}

reAssociate::Functor g=>(g :.: (f :.: h)) a -> ((g :.: f) :.: h) a
reAssociate = Comp . Comp . fmap unComp . unComp

reAssociateNP::(Functor g, SListI xss)=>NP (g :.: (f :.: h)) xss->NP ((g :.: f) :.: h) xss
reAssociateNP = hmap reAssociate

isConNPToPOPIsCon::(Functor g, SListI2 xss)=>NP (g :.: (IsCon :.: (NP I))) xss -> POP (g :.: IsCon) xss
isConNPToPOPIsCon =
  let proxyC = Proxy :: Proxy (SListI)
  in POP . hcliftA proxyC (distributeI . unComp . reAssociate)


type DoAndSequence f h xss = POP f xss -> NP (h :.: (NP I)) xss

doAndSequence::(Applicative h, SListI2 xss)=>(forall a.f a->h a) -> DoAndSequence f h xss --POP f xss -> NP (h :.: (NP I)) xss
doAndSequence q =
  let sListIC = Proxy :: Proxy (SListI)
  in hcliftA sListIC (Comp . hsequence) . unPOP . hliftA q

class Map (f :: k -> *) (h :: k -> *) (a :: k) where
  doMap::f a -> h a

doAndSequence'::forall f h xss.(Applicative h, SListI2 xss, All2 (Map f h) xss)=>DoAndSequence f h xss --POP f xss -> NP (h :.: (NP I)) xss
doAndSequence' =
  let sListIC = Proxy :: Proxy SListI
      mapIC = Proxy :: Proxy (Map f h)
  in hcliftA sListIC (Comp . hsequence) . unPOP . hcliftA mapIC doMap

reconstructA::(Functor h, Generic a) => NP (h :.: (NP I)) (Code a) -> NP (K (h a)) (Code a)
reconstructA = hliftA (K. fmap (to . SOP) . unK) . hap wrappedInjections


functorDoPerConstructor::(Generic a, Functor g, Applicative h)=>DoAndSequence (g :.: IsCon) h (Code a)->g a->[h a]  -- one per constructor
functorDoPerConstructor doAndS = hcollapse . reconstructA . doAndS . isConNPToPOPIsCon . functorToIsConNP

functorDoPerConstructor'::forall a g h.(Generic a, HasDatatypeInfo a, Functor g, Applicative h)
    =>DoAndSequence (g :.: IsCon) h (Code a)
    ->g a
    ->[(ConstructorName,h a)]  -- one per constructor
functorDoPerConstructor' doAndS ga =
  let conNames = hcollapse . hliftA (K . constructorName) . constructorInfo $ datatypeInfo (Proxy :: Proxy a)  -- [ConstructorName]
  in zip conNames (functorDoPerConstructor doAndS ga)


-- now Reflex specific

data ConWidget t h a = ConWidget { conName::ConstructorName, switchedTo::Event t a, widget::h a }

-- should "updated" in the below be dynAsEv?
dynIsConNPToEventNP::(Reflex t, SListI2 xss)=> NP ((Dynamic t) :.: (IsCon :.: (NP I))) xss -> NP ((Event t) :.: (NP I)) xss
dynIsConNPToEventNP = hmap (Comp . fmapMaybe unIsCon . fmap unComp . updated . unComp)

-- this would have been a doozy but wasn't because step 1 was already this doozy
eventNPToEventNSNP::(Reflex t, SListI2 xss)=> NP ((Event t) :.: (NP I)) xss -> NP (K ((Event t) (NS (NP I) xss))) xss
eventNPToEventNSNP = hap wrappedInjections



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


dynamicToConWidgets::(Reflex t, Generic a,HasDatatypeInfo a, Applicative h)
  =>DoAndSequence (Dynamic t :.: IsCon) (h :.: Maybe) (Code a)
  ->Dynamic t a
  -> [ConWidget t (h :.: Maybe) a]
dynamicToConWidgets doAndS dynA =
  let switchEvents = eventPerConstructor dynA
      namedWidgets = functorDoPerConstructor' doAndS dynA
  in zipWith (\ev (n,w) -> ConWidget n ev w) switchEvents namedWidgets

{-
maybeDynToConWidgets::(Reflex t, Generic a,HasDatatypeInfo a, Applicative h)
  =>DoAndSequence (Dynamic t :.: IsCon) h (Code a)
  ->(Dynamic t :.: Maybe) a
  -> [ConWidget t h a]
maybeDynToConWidgets doAndS dynMA =
  let cws = dynamicToConWidgets doAndS (unComp dynMA)
      cws' = \ConWidget n ev w -> ConWidget n (fmapMaybe id ev) 
  in 
-}
{-
dynamicToWidgetEvent::(Reflex t, Generic a,HasDatatypeInfo a, Applicative h)
  =>DoAndSequence (Dynamic t :.: IsCon) (h :.: Maybe) (Code a)
  ->Dynamic t a
  ->Event t (h a)
dynamicToWidgetEvent doAndS dynA =
  let conWidgets = dynamicToConWidgets doAndS dynA
      f (ConWidget _ ev w) = w <$ ev
  in leftmost $ f <$> conWidgets
-}

{-
functorDoPerConstructor::(Generic a, Functor g, Applicative h)=>(forall a.(g :.: IsCon) a -> h a)->g a->[h a]  -- one per constructor
functorDoPerConstructor doF = hcollapse . reconstructA . doAndSequence doF . isConNPToPOPIsCon . functorToIsConNP

functorDoPerConstructor'::forall a g h.(Generic a, HasDatatypeInfo a, Functor g, Applicative h)
    =>(forall x.(g :.: IsCon) x -> h x)
    ->g a
    ->[(ConstructorName,h a)]  -- one per constructor
functorDoPerConstructor' doF ga =
  let conNames = hcollapse . hliftA (K . constructorName) . constructorInfo $ datatypeInfo (Proxy :: Proxy a)  -- [ConstructorName]
  in zip conNames (functorDoPerConstructor doF ga)
-}

{-
type family IsIn (x :: k) (xs :: [k]) :: Bool where
  IsIn _ '[] = False
  IsIn x (y ': ys) = (x == y) || (IsIn x ys)

type family IsIn2 (x :: k) (xss :: [[k]]) :: Bool where
  IsIn2 _ '[] = False
  IsIn2 x (ys ': yss) = (IsIn x ys) || (IsIn2 x yss)


doAndSequence''::(Applicative h, SListI2 xss)=>(forall x.(IsIn2 x xss ~ True)=>f x->h x) -> POP f xss -> NP (h :.: (NP I)) xss
doAndSequence'' q =
  let sListIC = Proxy :: Proxy (SListI)
      qC =
  in hcliftA sListIC (Comp . hsequence) . unPOP . hliftA q
-}
