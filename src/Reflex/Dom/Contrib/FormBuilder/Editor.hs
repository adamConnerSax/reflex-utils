{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
--  , DynEditor (DynEditor)
  , runDynEditor
  , unEditedDynMaybe
--  , FREditor (FREditor)
--  , runFREditor
--  , unEditedFR
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

-- | This function just lifts a DynMaybe directly to a form, without any actual editing
unEditedDynMaybe :: (Reflex t, Applicative m) => DynMaybe t a -> Form t m a
unEditedDynMaybe = Compose . pure . Compose . dynamicToWidgetResult . fmap maybeToAV . getCompose

type DynEditor t m a b = Editor (DynMaybe t) (Form t m) a b

runDynEditor :: DynEditor t m a b -> DynMaybe t a -> Form t m b
runDynEditor = runEditor

instance (Reflex t, Applicative m) => NatBetween (DynMaybe t) (Form t m) where
  nat = unEditedDynMaybe

instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m) => Combinable (Form t m) (Form t m) where
--combine :: Form t m (Form t m) -> Form t m
  combine x = Compose $ getCompose x >>= getCompose . combine . Compose . fmap avToMaybe . widgetResultToDynamic . getCompose
{-  combine x = Compose $ do
    x1 <- getCompose x -- FormResult t (Form t m a)
    let x2 = Compose $ fmap avToMaybe $ widgetResultToDynamic $ getCompose x1 -- DynMaybe t (Form t m a)
        x3 = combine x2
    getCompose x3 -- Form t m a
-}

--    . fmap (Compose . join . fmap getCompose . getCompose) . join . fmap sequenceA . getCompose . fmap getCompose

-- | This gives all the powers to Editors of the type Editor (DynMaybe t) (Compose m (DynMaybe) t) a b, e.g., widgets like DynMaybe t a -> m (DynMaybe t a)
-- but to do so, a dyn happens.  So this is not efficient.  Compositions (categorical or uses of Profunctor Choice, e.g., wander) will need to redraw the widget (when?)
-- Can we improve this using a chooser?  Where would that go?

instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m) => Combinable (DynMaybe t) (Compose m (DynMaybe t)) where
--  combine :: DynMaybe t (Compose m (DynMaybe t) a) -> Compose m (DynMaybe t) a
  combine x =  Compose $ do
    let x1 = fmap sequenceA . getCompose . fmap getCompose $ x -- Dynamic t (m (Maybe (DynMaybe t a)))
    x2 <- dyn x1 -- Event t (Maybe (DynMaybe t a)))
    let x3 = fmap join . sequenceA . fmap getCompose <$> x2 -- Event t (Dynamic t (Maybe a))
    dmd <- buildDynamic (return $ constDyn Nothing) x3
    return $ Compose $ join $ dmd

instance (Reflex t, Monad m, DomBuilder t m, MonadHold t m, PostBuild t m) => Combinable (DynMaybe t) (Form t m) where
--combine :: DynMaybe t (Form t m a) -> Form t m a
  combine x = Compose $ do
    let x1 = fmap sequenceA $ getCompose $ fmap getCompose $ x -- DynMaybe t ((FR t m) (FormResult t a))
    x2 <- dyn x1 -- Event t (Maybe (FormResult t a))
    let x3 = fmap (mergeAccValidation . maybeToAV) . sequenceA . fmap getCompose <$> x2 -- Event t (WidgetResult t (FValidation a))
    x4 <- holdDyn (constWidgetResult $ AccFailure [FNothing]) x3 -- Dynamic t (WidgetResult t (FValidation a))
    return $ Compose $ dynamicWidgetResultToWidgetResult x4


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
-- first' :: Editor g f a b -> Editor g f (a,c) (b,c)
  first' (Editor e) = Editor $ \gac -> (,) <$> e (fst <$> gac) <*> nat (snd <$> gac)

-- | An odd thing which can be extract (for comonad g) or join (for Monad f => Combinable f f)
class Combinable g f where
  combine :: g (f a) -> f a

{-
instance (NatBetween g f, Combinable f f) => Combinable g f where
  combine = combine . nat
-}

instance (Applicative f, Applicative g, NatBetween g f, Combinable g f) => Choice (Editor g f) where
-- left' :: Editor g f a b -> Editor g f (Either a c) (Either b c)
  left' ed =
    let s :: Either a c -> Either (g a) (g c)
        s = bimap pure pure
        r :: Editor g f x y -> Either (g x) (g c) -> Either (f y) (f c)
        r (Editor e1) = bimap e1 nat
        q :: Either (f b) (f c) -> f (Either b c)
        q = either (fmap Left) (fmap Right)
    in Editor $ \gexc -> combine $ fmap (q . r ed . s) gexc

instance (Applicative g, Applicative f, NatBetween g f, Combinable g f) => Traversing (Editor g f) where
--  wander :: (forall f. Applicative f => (a -> f b) -> (s -> f t)) -> Editor g f a b -> Editor g f s t
  wander vlt (Editor eab) = Editor $ combine . fmap (vlt (eab . pure))

-- | We just need "Pointed g" for pure and
instance (Applicative g, Functor f, NatBetween g f, Combinable f f) => C.Category (Editor g f) where
  id = Editor nat -- Editor g f a a
  (Editor ebc) . (Editor eab) = Editor $ \ga -> combine $ fmap (ebc . pure) (eab ga) -- Editor g f b c -> Editor g f a b -> Editor g f a c

instance Monad f => Monad (Editor g f a) where
  return = pure
  eb >>= h = Editor $ \ga -> let q = flip runEditor ga in q eb >>= q . h


{-
-- | Editor is just a wrapper around the usual form building function.  This wrapper also allows
-- all these instances to be written.
-- Editor is UpStar and DownStar?  There are two functors involved...
newtype DynEditor t m a b = DynEditor { runDynEditor :: DynMaybe t a -> Form t m b }

instance (Reflex t, Functor m) => Functor (DynEditor t m a) where
  fmap f (DynEditor w) = DynEditor $ fmap f . w

instance (Reflex t, Applicative m) => Applicative (DynEditor t m a) where
  pure x = DynEditor $ const $ pure x
  (DynEditor wfxy) <*> (DynEditor wx) = DynEditor $ \dma -> wfxy dma <*> wx dma

instance (Reflex t, Functor m) => Profunctor (DynEditor t m) where
  dimap f g (DynEditor w) = DynEditor $ fmap g . w . fmap f

instance (Reflex t, Applicative m) => Strong (DynEditor t m) where
  first' (DynEditor w) = DynEditor $ \dmic -> (,) <$> w (fst <$> dmic) <*> unEditedDynMaybe (snd <$> dmic)

{-
-- Seems like this should be possible for a monoidal b, but how do we even specify that here?
instance (Reflex t, Applicative m) => Choice (FormFrom t m) where
  left' (FormFrom w) = FormFrom $ \x -> do
    let fL :: DynMaybe t (Either a c) -> DynMaybe t a
        fL = Compose .fmap (join . fmap (either Just (const Nothing))). getCompose
        fR :: DynMaybe t (Either a c) -> DynMaybe t c
        fR = Compose . fmap (join . fmap (either (const Nothing) Just)). getCompose
        formL = w $ fL x
        formR = dynMaybeToForm $ fR x
-}

-- | This function just lifts a FormResult directly to a form, without any actual editing
unEditedFR :: (Reflex t, Applicative m) => FormResult t a -> Form t m a
unEditedFR = Compose . pure

-- | EditorFR is also almost--but for the final composition--Kleisli (FR t m) (FormResult t a) (FormResult t b)
newtype FREditor t m a b = FREditor { runFREditor :: FormResult t a -> Form t m b }

instance (Reflex t, Functor m) => Functor (FREditor t m a) where
  fmap f (FREditor w) = FREditor $ fmap f . w

instance (Reflex t, Applicative m) => Applicative (FREditor t m a) where
  pure x = FREditor $ const $ pure x
  (FREditor wfxy) <*> (FREditor wx) = FREditor $ \fra -> wfxy fra <*> wx fra

instance (Reflex t, Functor m) => Profunctor (FREditor t m) where
  dimap f g (FREditor w) = FREditor $ fmap g . w . fmap f

instance (Reflex t, Applicative m) => Strong (FREditor t m) where
  first' (FREditor w) = FREditor $ \fric -> (,) <$> w (fst <$> fric) <*> unEditedFR (snd <$> fric)
-}

{-
-- liftInput
newtype Editor (t :: k)  (m :: * -> *) (g :: k -> * -> *) (f :: k -> (* -> *) -> * -> *) (a :: *)  (b :: *) = Editor { runEditor :: g t a -> f t m b }

instance (Reflex t, Functor (f t m)) => Functor (Editor t m g f a) where
  fmap d (Editor e) = Editor $ fmap d . e

instance (Reflex t, Applicative (f t m)) => Applicative (Editor t m g f a) where
  pure x = Editor $ const $ pure x
  (Editor efxy) <*> (Editor ex) = Editor $ \a -> exy a <*> ex a

instance (Reflex t, Functor (g t), Functor (f t m)) => Profunctor (Editor t m g f) where
  dimap f g (Editor e) = Editor $ fmap g . w . f

instance (Reflex t, Functor (g t), Applicative (f t m)) => Strong (Editor t m) where
  first' (Editor e) = Editor $ \ic -> (,) <$> e (fst fric) <*> liftInput (snd fric)

-}



