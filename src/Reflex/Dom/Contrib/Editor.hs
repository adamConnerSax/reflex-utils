{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric #-}
module Reflex.Dom.Contrib.Editor
  (
    Editor (Editor)
  , runEditor
  , transformEditor
  , (|<|)
  , (|>|)
  , DynMaybe
  , Combinable (..)
  , Distributable (..)
  ) where

import           Reflex.Dynamic.FactorDyn (factorDyn')

import qualified Generics.SOP  as SOP
import           GHC.Generics (Generic)
import qualified Control.Category                             as C
import           Control.Monad                                (join)
import           Control.Monad.Fix (MonadFix)
import           Data.Bifunctor                               (bimap)
import           Data.Functor.Compose                         (Compose (Compose),
                                                               getCompose)
import           Data.Profunctor                              (Choice (..), Profunctor (dimap),
                                                               Strong (..))
import           Data.Profunctor.Traversing                   (Traversing (..))
import           Control.Arrow                                (Arrow(..), ArrowChoice (..))

import           Reflex                                       (Dynamic,
                                                               MonadHold,
                                                               Reflex,
                                                               buildDynamic,
                                                               constDyn)
import           Reflex.Dom                                   (DomBuilder,
                                                               PostBuild, dyn)



newtype Editor g f a b = Editor { runEditor :: g a -> f b }

transformEditor :: (q a -> g a') -> (f b -> h b') -> Editor g f a' b -> Editor q h a b'
transformEditor inF outF e = Editor $ outF . runEditor e . inF

instance Functor f => Functor (Editor g f a) where
  fmap h (Editor e) = Editor $ fmap h . e

instance (Functor g, Functor f) => Profunctor (Editor f g) where
  dimap q r (Editor e) = Editor $ fmap r . e . fmap q

instance Applicative f => Applicative (Editor g f a) where
  pure = Editor . const . pure
  (Editor eFxy) <*> (Editor ex) = Editor $ \ga -> eFxy ga <*> ex ga

instance (Applicative f, Functor g, f ~ Compose m g, Applicative m) => Strong (Editor g f) where
  first' :: Editor g f a b -> Editor g f (a,c) (b,c)
  first' (Editor e) = Editor $ \gac -> (,) <$> e (fst <$> gac) <*> (Compose $ pure (snd <$> gac))

-- | An odd thing which can be extract (for comonad g) or join (for Monad f => Combinable f f)
class Combinable g f where
  combine :: g (f a) -> f a

instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m, Combinable h (Compose m h)) => Combinable (Compose m h) (Compose m h) where
  combine :: Compose m h (Compose m h a) -> Compose m h a
  combine x = Compose $ getCompose x >>= getCompose . combine

-- | This gives all the powers (Choice, Category, Traversing) to Editors.
-- but to do so, a dyn happens.  So this is not efficient.  Compositions (categorical or uses of Profunctor Choice, e.g., wander) will need to redraw the widget (when?)
-- Can we do a factorDyn somehow, when the common/Left type has a generics-sop.Generic instance?

type DynMaybe t = Compose (Dynamic t) Maybe

instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m) => Combinable (DynMaybe t) (Compose m (DynMaybe t)) where
  combine :: DynMaybe t (Compose m (DynMaybe t) a) -> Compose m (DynMaybe t) a
  combine x =  Compose $ do
    let mdm2dm = Compose . fmap join . sequenceA . fmap getCompose  
        x1 = fmap (fmap mdm2dm . sequenceA) . getCompose . fmap getCompose $ x -- Dynamic t (m (DynMaybe t a))
    x2 <- dyn x1 -- Event t (DynMaybe t a)
    dmd <- buildDynamic (return $ constDyn Nothing) (getCompose <$> x2)
    return $ Compose $ join dmd

{-
instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m) => Combinable (h t) (Compose m (h t)) where
  combine :: h t (Compose m (h t) a) -> Compose m (h t) a
-}

-- Applicative (Pointed) is the obvious way to do this but there are others, 
class Monad m => Distributable g m where
  distribute :: g (Either a c) -> m (g (Either (g a) (g c)))

data MaybeEither a b = N | L a | R b deriving (Generic)
instance SOP.Generic (MaybeEither a b)

toMaybeEither :: Maybe (Either a b) -> MaybeEither a b
toMaybeEither = maybe N (either L R)

fromMaybeEither :: MaybeEither a b -> Maybe (Either a b)
fromMaybeEither N = Nothing
fromMaybeEither (L x) = Just $ Left x
fromMaybeEither (R x) = Just $ Right x
    
factorMaybeEither :: (Reflex t, MonadHold t m, MonadFix m) => Dynamic t (MaybeEither a b) -> m (Dynamic t (MaybeEither (Dynamic t a) (Dynamic t b)))
factorMaybeEither = factorDyn'
                                                                                               
instance (Reflex t, MonadHold t m, MonadFix m) => Distributable (DynMaybe t) m where
  distribute :: forall t a b m. (Reflex t, MonadHold t m, MonadFix m) => DynMaybe t (Either a b) -> m (DynMaybe t (Either (DynMaybe t a) (DynMaybe t b)))
  distribute x = do
    let x1 = toMaybeEither <$> getCompose x
    x2 <- factorMaybeEither x1 
    return $ fmap (bimap (Compose . fmap pure) (Compose . fmap pure)) $ Compose . fmap fromMaybeEither $ x2 

instance ( Applicative f
         , Functor g
         , Monad m
         , f ~ Compose m g
         , Distributable g m
         , Combinable g f) => Choice (Editor g f) where
  left' :: forall g f a b c m. (Applicative f, Functor g, Monad m, f ~ Compose m g, Distributable g m, Combinable g f) => Editor g f a b -> Editor g f (Either a c) (Either b c)
  left' ed = Editor $ \geac -> Compose $ do
    dist_geac :: g (Either (g a) (g c)) <- distribute geac    
    let x1 = fmap (bimap (runEditor ed) (Compose . pure)) dist_geac
        x2 = fmap (either (fmap Left) (fmap Right)) x1
    getCompose $ combine x2

instance ( Applicative g
         , Applicative f
         , f ~ Compose m g
         , Monad m
         , Distributable g m
         , Combinable g f) => Traversing (Editor g f) where
  wander :: (forall f. Applicative f => (a -> f b) -> (s -> f t)) -> Editor g f a b -> Editor g f s t
  wander vlt (Editor eab) = Editor $ combine . fmap (vlt (eab . pure))


(|<|) :: (Monad m, f ~ Compose m g) => Editor g f b c -> Editor g f a b -> Editor g f a c
(Editor ebc) |<| (Editor eab) =  Editor $ \ga -> Compose $ join $ fmap (getCompose . ebc) (getCompose $ eab ga)-- Editor g f b c -> Editor g f a b -> Editor g f a c
infixr 7 |<|

(|>|) :: (Monad m, f ~ Compose m g) => Editor g f a b -> Editor g f b c -> Editor g f a c
(|>|)  = flip (|<|)  
infixr 7 |>|
  
instance (Monad m, f ~ Compose m g) => C.Category (Editor g f) where
  id = Editor $ Compose . pure -- Editor g f a a
  ebc . eab = ebc |<| eab

instance Monad f => Monad (Editor g f a) where
  return = pure
  eab >>= h = Editor $ \ga -> runEditor eab ga >>= flip runEditor ga . h

instance (Applicative f, Functor g, Monad m, f ~ Compose m g) => Arrow (Editor g f) where
  arr :: (b -> c) -> Editor g f b c
  arr h = Editor $ Compose . return . fmap h 
  first = first' -- this is from Strong

instance ( Applicative f
         , Functor g
         , Monad m
         , f ~ Compose m g
         , Distributable g m
         , Combinable g f) => ArrowChoice (Editor g f) where
  left = left'  -- from Profunctor.Choice

  
  

{-    
-- | This makes any Editor with a (m (DynMaybe t)) result into a Category.
instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m) => Combinable (Compose m (DynMaybe t)) (Compose m (DynMaybe t)) where
  combine :: Compose m (DynMaybe t) (Compose m (DynMaybe t) a) -> Compose m (DynMaybe t) a
  combine x = Compose $ getCompose x >>= getCompose . combine

-- | This makes any Editor with a (Form t m) result into a Category.
instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m) => Combinable (Form t m) (Form t m) where
  combine :: Form t m (Form t m a) -> Form t m a
  combine x = Compose $ getCompose x >>= getCompose . combine
-}




