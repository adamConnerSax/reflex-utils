module Reflex.Dom.Contrib.FormBuilder.Form
  (
    FormFrom (FormFrom)
  , runFormFrom
  , FormFromFR (FormFromWR)
  , runFormFromFR
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

type Form t m a= Compose (FR t m) (FormResult t) a

-- | Form is a functor and applicative since its components (ReaderT and WidgetResult) are
-- DynMaybe is also a functor (and applicative) for the same reason.

dynMaybeToForm :: (Reflex t, Applicative m) => DynMaybe t a -> Form t m a
dynMaybeToForm = Compose . pure . Compose . dynamicToWidgetResult . fmap maybeToAV . getCompose

-- | FormFrom is just a wrapper around the usual form building function.  This wrapper also allows
-- all these instances to be written.
-- FormFrom is UpStar and DownStar?  There are two functors involved...
newtype FormFrom t m a b = FormFrom { runFormFrom :: DynMaybe t a -> Form t m b }

instance (Reflex t, Functor m) => Functor (FormFrom t m a) where
  fmap f (FormFrom w) = FormFrom $ fmap f . w

instance (Reflex t, Applicative m) => Applicative (FormFrom t m a) where
  pure x = FormFrom $ const $ pure x
  (FormFrom wfxy) <*> (FormFrom wx) = FormFrom $ \dma -> wfxy dma <*> wx dma

instance (Reflex t, Functor m) => Profunctor (FormFrom t m) where
  dimap f g (FormFrom w) = FormFrom $ fmap g . w . fmap f

instance (Reflex t, Applicative m) => Strong (FormFrom t m) where
  first' (FormFrom w) = FormFrom $ \dmic -> (,) <$> w (fst <$> dmic) <*> dynMaybeToForm (snd <$> dmic)


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

-- | FormFromFR is also almost--but for the final composition--Kleisli (FR t m) (FormResult t a) (FormResult t b)
newtype FormFromFR t m a b = FormFromFR { runFormFromFR :: FormResult t a -> Form t m b }

instance (Reflex t, Functor m) => Functor (FormFromFR t m a) where
  fmap f (FormFromFR w) = FormFromFR $ fmap f . w

instance (Reflex t, Applicative m) => Applicative (FormFromFR t m a) where
  pure x = FormFromFR $ const $ pure x
  (FormFromFR wfxy) <*> (FormFromFR wx) = FormFromFR $ \dma -> wfxy dma <*> wx dma

instance (Reflex t, Functor m) => Profunctor (FormFromFR t m) where
  dimap f g (FormFromFR w) = FormFromFR $ fmap g . w . fmap f

instance (Reflex t, Applicative m) => Strong (FormFromFR t m) where
  first' (FormFromFR w) = FormFromFR $ \fric -> (,) <$> w (fst <$> fric) <*> Compose (pure (snd <$> fric))

