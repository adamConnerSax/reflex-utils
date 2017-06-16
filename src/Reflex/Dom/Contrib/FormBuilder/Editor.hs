{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Reflex.Dom.Contrib.FormBuilder.Editor
  (
    Form
  , Editor (Editor)
  , runEditor
  , transformEditor
  , DynEditor
  , runDynEditor
  , unEditedDynMaybe
  ) where


import           Reflex.Dom.Contrib.FormBuilder.Configuration (FR, FormResult)
import           Reflex.Dom.Contrib.FormBuilder.DynValidation (AccValidation (AccFailure),
                                                               DynMaybe,
                                                               FormError (FNothing),
                                                               avToMaybe,
                                                               dynValidationErr,
                                                               maybeToAV,
                                                               mergeAccValidation)
import           Reflex.Dom.Contrib.Widgets.WidgetResult      (constWidgetResult,
                                                               dynamicToWidgetResult,
                                                               dynamicWidgetResultToWidgetResult,
                                                               widgetResultToDynamic)


import qualified Control.Category                             as C
import           Control.Lens                                 (Lens)
import           Control.Monad                                (join)
import           Control.Monad.Trans                          (lift)
import           Control.Monad.Trans.Reader                   (ask, runReaderT)
import           Data.Bifunctor                               (bimap)
import           Data.Functor.Compose                         (Compose (Compose),
                                                               getCompose)
import           Data.Profunctor                              (Choice (..), Profunctor (dimap),
                                                               Strong (..))
import           Data.Profunctor.Traversing                   (Traversing (..))


import           Reflex                                       (Dynamic, Event,
                                                               MonadHold,
                                                               Reflex,
                                                               buildDynamic,
                                                               constDyn,
                                                               holdDyn)
import           Reflex.Dom                                   (DomBuilder,
                                                               PostBuild, dyn)


-- This is necessary because this functor and applicative are different from that of SFRW
-- NB: Form is *not* a Monad. So we do all the monadic widget building in (FRW t m a) and then wrap with makeForm
type Form t m = Compose (FR t m) (FormResult t)

-- | Form is a functor and applicative since its components (ReaderT and WidgetResult) are
-- DynMaybe is also a functor (and applicative) for the same reason.
dynMaybeToFormResult :: Reflex t => DynMaybe t a -> FormResult t a
dynMaybeToFormResult = Compose . dynamicToWidgetResult . fmap maybeToAV . getCompose

-- | This function just lifts a DynMaybe directly to a form, without any actual editing
unEditedDynMaybe :: (Reflex t, Applicative m) => DynMaybe t a -> Form t m a
unEditedDynMaybe = Compose . pure . dynMaybeToFormResult

type DynEditor t m a b = Editor (DynMaybe t) (Form t m) a b

runDynEditor :: DynEditor t m a b -> DynMaybe t a -> Form t m b
runDynEditor = runEditor

instance (Reflex t, Applicative m) => NatBetween (DynMaybe t) (Form t m) where
  nat = unEditedDynMaybe

instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m, Combinable h (Compose m h)) => Combinable (Compose m h) (Compose m h) where
  combine :: Compose m h (Compose m h a) -> Compose m h a
  combine x = Compose $ getCompose x >>= getCompose . combine

-- | This gives all the powers (Choice, Category, Traversing) to Editors.
-- but to do so, a dyn happens.  So this is not efficient.  Compositions (categorical or uses of Profunctor Choice, e.g., wander) will need to redraw the widget (when?)
-- Can we do a factorDyn somehow, when the common/Left type has a generics-sop.Generic instance?

-- Ditto for DynMaybe t m -> Form t m
-- NB: This one uses the instance for (FormResult t m) (Form t m) by upgrading the DynMaybe to a FormResult.
instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m) => Combinable (DynMaybe t) (Form t m) where
  combine :: DynMaybe t (Form t m a) -> Form t m a
  combine = combine . dynMaybeToFormResult

instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m) => Combinable (DynMaybe t) (Compose m (DynMaybe t)) where
  combine :: DynMaybe t (Compose m (DynMaybe t) a) -> Compose m (DynMaybe t) a
  combine x =  Compose $ do
    let mdm2dm = Compose . fmap join . sequenceA . fmap getCompose  
        x1 = fmap (fmap mdm2dm . sequenceA) . getCompose . fmap getCompose $ x -- Dynamic t (m (DynMaybe t a))
    x2 <- dyn x1 -- Event t (DynMaybe t a)
    dmd <- buildDynamic (return $ constDyn Nothing) (getCompose <$> x2)
    return $ Compose $ join dmd

-- Ditto for FormResult t m -> Form t m
instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m) => Combinable (FormResult t) (Form t m) where
  combine :: FormResult t (Form t m a) -> Form t m a
  combine x = Compose $ do
    let fvfr2fr = Compose . fmap mergeAccValidation . sequenceA . fmap getCompose
    let x1 = widgetResultToDynamic . fmap (fmap fvfr2fr . sequenceA) . getCompose . fmap getCompose $ x -- Dynamic t ((FR t m) (FormResult t a))
    x2 <- dyn x1 -- Event t (FormResult t a)
    x4 <- holdDyn (constWidgetResult $ AccFailure [FNothing]) (getCompose <$> x2) -- Dynamic t (WidgetResult t (FValidation a))
    return $ Compose $ dynamicWidgetResultToWidgetResult x4

{-
instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m) => Combinable (h t) (Compose m (h t)) where
  combine :: h t (Compose m (h t) a) -> Compose m (h t) a
-}

newtype Editor g f a b = Editor { runEditor :: g a -> f b }

transformEditor :: (q a -> g a) -> (f b -> h b) -> Editor g f a b -> Editor q h a b
transformEditor inNat outNat e = Editor $ outNat . runEditor e . inNat

instance Functor f => Functor (Editor g f a) where
  fmap h (Editor e) = Editor $ fmap h . e

instance (Functor g, Functor f) => Profunctor (Editor f g) where
  dimap q r (Editor e) = Editor $ fmap r . e . fmap q

instance Applicative f => Applicative (Editor g f a) where
  pure = Editor . const . pure
  (Editor eFxy) <*> (Editor ex) = Editor $ \ga -> eFxy ga <*> ex ga

class NatBetween g f where
  nat :: g a -> f a

instance (Applicative f, Functor g, NatBetween g f) => Strong (Editor g f) where
  first' :: Editor g f a b -> Editor g f (a,c) (b,c)
  first' (Editor e) = Editor $ \gac -> (,) <$> e (fst <$> gac) <*> nat (snd <$> gac)

-- | An odd thing which can be extract (for comonad g) or join (for Monad f => Combinable f f)
class Combinable g f where
  combine :: g (f a) -> f a

{-
instance (NatBetween g f, Combinable f f) => Combinable g f where
  combine = combine . nat
-}

instance (Applicative f, Applicative g, NatBetween g f, Combinable g f) => Choice (Editor g f) where
  left' :: Editor g f a b -> Editor g f (Either a c) (Either b c)
  left' ed =
    let s :: Either a c -> Either (g a) (g c)
        s = bimap pure pure
        r :: Editor g f x y -> Either (g x) (g c) -> Either (f y) (f c)
        r (Editor e1) = bimap e1 nat
        q :: Either (f b) (f c) -> f (Either b c)
        q = either (fmap Left) (fmap Right)
    in Editor $ \gexc -> combine $ fmap (q . r ed . s) gexc

instance (Applicative g, Applicative f, NatBetween g f, Combinable g f) => Traversing (Editor g f) where
  wander :: (forall f. Applicative f => (a -> f b) -> (s -> f t)) -> Editor g f a b -> Editor g f s t
  wander vlt (Editor eab) = Editor $ combine . fmap (vlt (eab . pure))

-- | We just need "Pointed g" for pure and
instance (Applicative g, Functor f, NatBetween g f, Combinable f f) => C.Category (Editor g f) where
  id = Editor nat -- Editor g f a a
  (Editor ebc) . (Editor eab) = Editor $ \ga -> combine $ fmap (ebc . pure) (eab ga) -- Editor g f b c -> Editor g f a b -> Editor g f a c

instance Monad f => Monad (Editor g f a) where
  return = pure
  eb >>= h = Editor $ \ga -> let q = flip runEditor ga in q eb >>= q . h


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




