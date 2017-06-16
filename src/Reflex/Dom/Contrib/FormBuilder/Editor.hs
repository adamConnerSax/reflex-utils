{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
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
import           Reflex.Dom.Contrib.FormBuilder.DynValidation (AccValidation (AccSuccess, AccFailure),
                                                               DynMaybe,
                                                               FormError (FNothing), FormErrors,
                                                               avToMaybe,
                                                               dynValidationErr,
                                                               maybeToAV,
                                                               mergeAccValidation, accValidation)
import           Reflex.Dom.Contrib.Widgets.WidgetResult      (constWidgetResult,
                                                               dynamicToWidgetResult,
                                                               dynamicWidgetResultToWidgetResult,
                                                               widgetResultToDynamic)
import           Reflex.Dynamic.FactorDyn (factorDyn')


import qualified Generics.SOP  as SOP
import           GHC.Generics (Generic)
import qualified Control.Category                             as C
import           Control.Lens                                 (Lens)
import           Control.Monad                                (join)
import           Control.Monad.Fix (MonadFix)
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

instance (Applicative f, Functor g, f ~ Compose m g, Applicative m) => Strong (Editor g f) where
  first' :: Editor g f a b -> Editor g f (a,c) (b,c)
  first' (Editor e) = Editor $ \gac -> (,) <$> e (fst <$> gac) <*> (Compose $ pure (snd <$> gac))

-- | An odd thing which can be extract (for comonad g) or join (for Monad f => Combinable f f)
class Combinable g f where
  combine :: g (f a) -> f a

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
    let x1 :: Dynamic t (MaybeEither a b)
        x1 = fmap toMaybeEither $ getCompose x
    x2 :: Dynamic t (MaybeEither (Dynamic t a) (Dynamic t b)) <- factorMaybeEither x1 
    return $ fmap (bimap (Compose . fmap pure) (Compose . fmap pure)) $ Compose . fmap fromMaybeEither $ x2 


data AccValEither e a b = F e | SL a | SR b deriving (Generic)
instance SOP.Generic (AccValEither e a b)

toAccValEither :: AccValidation e (Either a b) -> AccValEither e a b
toAccValEither = accValidation F (either SL SR)

fromAccValEither :: AccValEither e a b -> AccValidation e (Either a b)
fromAccValEither (F x) = AccFailure x
fromAccValEither (SL x) = AccSuccess $ Left x
fromAccValEither (SR x) = AccSuccess $ Right x

factorAccValEither :: (Reflex t, MonadHold t m, MonadFix m) => Dynamic t (AccValEither e a b) -> m (Dynamic t (AccValEither (Dynamic t e) (Dynamic t a) (Dynamic t b)))
factorAccValEither = factorDyn'

{-
instance (Reflex t, MonadHold t m, MonadFix m) => Distributable (FormResult t) m where
  distribute :: forall t a b m. (Reflex t, MonadHold t m, MonadFix m) => FormResult t (Either a b) -> m (FormResult t (Either (FormResult t a) (FormResult t b)))
  distribute x = do
    let x1 :: Dynamic t (AccValEither FormErrors a b)
        x1 = widgetResultToDynamic . fmap toAccValEither $ getCompose x
    x2 :: Dynamic t (AccValEither (Dynamic t FormErrors) (Dynamic t a) (Dynamic t b)) <- factorAccValEither x1
    let x3 :: Dynamic t (AccValidation (Dynamic t FormErrors) (Either (Dynamic t a) (Dynamic t b)))
        x3 = fmap fromAccValEither $ x2
        f :: AccValidation (Dynamic t e) (Either c d) -> Dynamic t (AccValidation e (Either c d))  
        f x = case x of
          AccFailure de -> fmap AccFailure de
          AccSuccess y -> constDyn $ AccSuccess y
        x4 :: Dynamic t (AccValidation FormErrors (Either (Dynamic t a) (Dynamic t b)))
        x4 = Compose $ dynamicToWidgetResult $ join $ fmap f x3
    return $ fmap (bimap (dynMaybeToFormResult . Compose . fmap pure) (dynMaybeToFormResult . Compose . fmap pure)) $ Compose x4 
-}


instance (Applicative f, Functor g, Monad m, f ~ Compose m g, Distributable g m, Combinable g f) => Choice (Editor g f) where
  left' :: forall g f a b c m. (Applicative f, Functor g, Monad m, f ~ Compose m g, Distributable g m, Combinable g f) => Editor g f a b -> Editor g f (Either a c) (Either b c)
  left' ed = Editor $ \geac -> Compose $ do
    dist_geac :: g (Either (g a) (g c)) <- distribute geac    
    let x1 :: g (Either (f b) (f c))
        x1 = fmap (bimap (runEditor ed) (Compose . pure)) dist_geac
        x2 :: g (f (Either b c))
        x2 = fmap (either (fmap Left) (fmap Right)) x1
    getCompose $ combine x2

instance (Applicative g, Applicative f, f ~ Compose m g, Monad m, Distributable g m, Combinable g f) => Traversing (Editor g f) where
  wander :: (forall f. Applicative f => (a -> f b) -> (s -> f t)) -> Editor g f a b -> Editor g f s t
  wander vlt (Editor eab) = Editor $ combine . fmap (vlt (eab . pure))


instance (Monad m, f ~ Compose m g) => C.Category (Editor g f) where
  id = Editor $ Compose . pure -- Editor g f a a
  (Editor ebc) . (Editor eab) = Editor $ \ga -> Compose $ join $ fmap (getCompose . ebc) (getCompose $ eab ga)-- Editor g f b c -> Editor g f a b -> Editor g f a c


instance Monad f => Monad (Editor g f a) where
  return = pure
  eab >>= h = Editor $ \ga -> runEditor eab ga >>= flip runEditor ga . h


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




