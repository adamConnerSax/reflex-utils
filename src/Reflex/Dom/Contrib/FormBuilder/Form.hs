module Reflex.Dom.Contrib.FormBuilder.Form
  (
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

dynMaybeToForm :: (Reflex t, Monad m) => DynMaybe t a -> Form t m a
dynMaybeToForm = Compose . return . Compose . dynamicToWidgetResult . fmap maybeToAV . getCompose

-- | FormFrom is just a wrapper around the usual form building function.  This wrapper also allows
-- all these instances to be written.
-- FormFrom is UpStar?
newtype FormFrom t m a b = FormFrom { unFormFrom :: DynMaybe t a -> Form t m b }

instance (Reflex t, Functor m) => Functor (FormFrom t m a) where
  fmap f (FormFrom w) = FormFrom $ fmap f . w

instance (Reflex t, Applicative m) => Applicative (FormFrom t m a) where
  pure x = FormFrom $ const $ pure x
  (FormFrom wfxy) <*> (FormFrom wx) = FormFrom $ \dma -> wfxy dma <*> wx dma

instance (Reflex t, Functor m) => Profunctor (FormFrom t m) where
  dimap f g (FormFrom w) = FormFrom $ fmap g . w . fmap f

instance (Reflex t, Monad m) => Strong (FormFrom t m) where
  first' (FormFrom w) = FormFrom $ \dmic -> (,) <$> w (fst <$> dmic) <*> dynMaybeToForm (snd <$> dmic)

{-
instance (Reflex t, Applicative m) => Choice (FormFrom t m) where
  left' (FormFrom w) = FormFrom $ \x -> do
    let fL :: DynMaybe t (Either a c) -> DynMaybe t a
        fL = Compose .fmap (join . fmap (either Just (const Nothing))). getCompose
        fR :: DynMaybe t (Either a c) -> DynMaybe t c
        fR = Compose . fmap (join . fmap (either (const Nothing) Just)). getCompose
        formL = w $ fL x
        formR = dynMaybeToForm $ fR x
-}
