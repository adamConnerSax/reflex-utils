{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
module Reflex.Dom.Contrib.SumType () where


-- FIXME:  make imports specific or qualify them
import           Generics.SOP hiding (Compose)
import           Generics.SOP.NP
import           Generics.SOP.Constraint (SListIN)
import qualified GHC.Generics as GHC

import           Reflex
import           Reflex.Dom
import           Data.Functor.Compose

{-
Trying to make a smart dynamic of sum type widget builder

1. Dynamic t a -> Dynamic t (SOP I (Code a))
2. Dynamic t (SOP I (Code a)) -> NP (Compose (Compose (Dynamic t) Maybe) (NP I)) (Code a)  -- ??
3. NP (Compose (Compose (Dynamic t) Maybe) (NP I)) (Code a) -> NP (Compose (Event t) (NP I)) (Code a)
4. NP (Compose (Event t) (NP I)) (Code a) -> NP (Compose K (m (Event t a))) xs -- ?? xs::'[a,a,...] length of number of constructors
5. NP (Compose K (m (Event t a))) xs -> (Event t Int, [m (Event t a)]) where the first event tells you which of the second is active. 
6. (Event t Int,[m (Event t a)]) -> m (Event t a) -- could use widgetHold or visibility here
7. m (Event t a) -> m (Dynamic t a)

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

type MyProjection (g :: * -> *) (f :: k -> *) (xs :: [k]) = K (g (NP f xs)) -.-> Compose g f 

myProjections::forall xs g f.(Functor g,SListI xs) => NP (MyProjection g f xs) xs
myProjections = case sList :: SList xs of
  SNil -> Nil
  SCons -> fn (fmap hd . unK) :* liftA_NP shiftMyProjection myProjections

shiftMyProjection :: Functor g=>MyProjection g f xs a -> MyProjection g f (x ': xs) a
shiftMyProjection (Fn f) = Fn $ f . K . fmap tl . unK 

{-
step2a::forall g a xss.(Functor g,Generic a)=>g a -> NP (Compose g (Compose Maybe (NP I))) (Code a)
step2a ga = hliftA2 _ projections (hpure $ K ga) where
--  q prj kga = Compose $ (prj . K . expandA) <$> unK kga
-}

{-
fmapConMaybe::(Functor g, Generic a, SListI xs)=>K (g a) xs -> Compose g (Compose Maybe (NP I)) xs
fmapConMaybe = fmap () unK
-}

-- this compiles!!
projectors::SListI xs=>NP (Compose ((->) (NP f xs)) f) xs
projectors = hliftA (\(Fn prj) -> Compose $ prj . K) projections

{-
step2a::(Generic a, Functor g,SListI xs)=>g a -> NP (Compose g (Compose Maybe (NP I))) xs
step2a ga = hliftA q (hpure (K ga)) where
  q kga = Compose $ (hap projections . K . expandA) <$> (unK kga)
-}
{-
-- So wrong!!
f::forall g a xss.(Functor g,Generic a, SListI2 xss, xss ~ Code a)=>g a -> NP (Compose g (Compose Maybe (NP I))) xss
f ga = hliftA2 q projectors (hpure (K ga)) where
--  q::forall (f :: [*] -> *)  g xs x.(Functor g, SListI xs)=>(Compose ((->) (NP f xs)) f) x -> K (g a) x -> Compose g (Compose Maybe (NP I)) x 
  q p kga = Compose $ fmap ((getCompose p) . expandA) $ unK kga
-}
{-
distribute::(Functor g, SListI xss, xss ~ Code a)=> g (SOP I xss) -> NP (Compose g (Compose Maybe (NP I))) xss
distribute gsop =
  let d1 = expand . unSOP <$> gsop -- g (NP (Compose Maybe (NP I)) xss)
 


expandFunctor::(Functor g, SListI xss, xss ~ Code a)=> g (SOP I xss) -> NP (Compose Maybe (Compose g (NP I))) xss
expandFunctor gsop = go sList gsop where
  go::forall ys.SListI ys=>SList ys -> g (SOP I xss) -> NP (Compose Maybe (Compose g (NP I))) ys
  go SNil _ = Nil
  go SCons gsop = 
-- steps

step1::(Functor g,Generic a)=>g a -> g (SOP I (Code a))
step1 = fmap from

type GMaybe = Compose g Maybe

step2::(Reflex t, Functor g, Generic a)=>g (SOP I (Code a)) -> NP (Compose GMaybe (NP I)) (Code a)
step2 gsop =
  let step2a::SOP I (Code a) -> NS (NP I) (Code a)
      step2a = unSOP
      step2b::NS (NP I) (Code a) -> NP (Compose Maybe (NP I)) (Code a)
      step2b = expand
      step2c:: g (NP (Compose Maybe (NP I)) (Code a)) -> NP (Compose g (Compose Maybe (NP I))) (Code a)
      step2c 

-}        

{-
step2b::(Generic a,Functor f)=>SOP f (Code a) -> NP (Compose Maybe (NP f)) (Code a)
step2b sop =
  let nsnp = unSOP sop
      


-}
