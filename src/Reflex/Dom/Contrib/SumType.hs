{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}
module Reflex.Dom.Contrib.SumType () where


-- FIXME:  make imports specific or qualify them
import           Generics.SOP hiding (Compose)
import           Generics.SOP.NP
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

expand::forall f xs.(Functor f,SListI xs)=>NS f xs -> NP (Compose Maybe f) xs
expand ns = go sList (Just ns) where
  go::forall ys.SListI ys => SList ys -> Maybe (NS f ys) -> NP (Compose Maybe f) ys
  go SNil _ = Nil  
  go SCons mNS = case mNS of
    Nothing -> Compose Nothing :* go sList Nothing -- after Z
    Just ns -> case ns of
      Z fx -> Compose (Just fx) :* go sList Nothing -- at Z
      S ns' -> Compose Nothing :* go sList (Just ns') -- before Z



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

        

{-
step2b::(Generic a,Functor f)=>SOP f (Code a) -> NP (Compose Maybe (NP f)) (Code a)
step2b sop =
  let nsnp = unSOP sop
      


-}
