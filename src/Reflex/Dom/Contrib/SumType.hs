{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Dom.Contrib.SumType () where


-- FIXME:  make imports specific or qualify them
import           Generics.SOP hiding (Compose)
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

step1::(Functor g,Generic a)=>g a -> g (SOP I (Code a))
step1 = fmap from

step2a::forall f xs.(Functor f,SListI xs)=>NS f xs -> NP (Compose Maybe f) xs
step2a ns =
  let length = lengthSList (Proxy :: Proxy xs)
      nothingList::Int -> NP (Compose Maybe f) xs'
      nothingList 0 = Nil
      nothingList n = Compose Nothing :* nothingList (n-1)
      go::Maybe (NS f xs') -> Int -> NP (Compose Maybe f) xs''
      go (Just ns) depth = case ns of
        Z fx  -> Compose (Just fx) :* nothingList (length - depth - 1) --go Nothing (depth+1)
        S ns' -> (Compose Nothing) :* go (Just ns') (depth + 1)
      go Nothing depth = undefined -- if (depth < length) then (Compose Nothing) :* go Nothing (depth + 1) else Nil
  in go (Just ns) 0
      
{-
step2b::(Generic a,Functor f)=>SOP f (Code a) -> NP (Compose Maybe (NP f)) (Code a)
step2b sop =
  let nsnp = unSOP sop
      

type GMaybe = Compose g Maybe

step2::(Reflex t, Functor g)=>g (SOP I (Code a)) -> NP (Compose GMaybe (NP I)) (Code a)
step2 
-}
