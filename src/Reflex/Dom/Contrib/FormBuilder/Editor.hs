module Reflex.Dom.Contrib.FormBuilder.Editor
  (
    Form
  , DynEditor (DynEditor)
  , runDynEditor
  , unEditedDynMaybe
  , FREditor (FREditor)
  , runFREditor
  , unEditedFR
  ) where


import           Reflex.Dom.Contrib.FormBuilder.Configuration (FR, FormResult)
import           Reflex.Dom.Contrib.FormBuilder.DynValidation (DynMaybe,
                                                               maybeToAV)
import           Reflex.Dom.Contrib.Widgets.WidgetResult      (dynamicToWidgetResult)


import           Data.Functor.Compose                         (Compose (Compose),
                                                               getCompose)
import           Data.Profunctor                              (Choice (..), Profunctor (dimap),
                                                               Strong (..))


import           Reflex                                       (Reflex)

-- This is necessary because this functor and applicative are different from that of SFRW
-- NB: Form is *not* a Monad. So we do all the monadic widget building in (FRW t m a) and then wrap with makeForm
type Form t m a= Compose (FR t m) (FormResult t) a

-- | Form is a functor and applicative since its components (ReaderT and WidgetResult) are
-- DynMaybe is also a functor (and applicative) for the same reason.

-- | This function just lifts a DynMaybe directly to a form, without any actual editing
unEditedDynMaybe :: (Reflex t, Applicative m) => DynMaybe t a -> Form t m a
unEditedDynMaybe = Compose . pure . Compose . dynamicToWidgetResult . fmap maybeToAV . getCompose

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
{-
newtype Editor g f a b { runEditor ::
-}
