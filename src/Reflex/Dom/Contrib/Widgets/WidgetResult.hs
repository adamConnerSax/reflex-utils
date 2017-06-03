{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
module Reflex.Dom.Contrib.Widgets.WidgetResult
  (
    WidgetResult
  , WrappedWidgetResult (..)
  , wrDyn
--  , wrInternalEv,
  , widgetResultToDynamic
  , updatedWidgetResult
  , currentWidgetResult
  , buildWidgetResult
  , dynamicWidgetResultToWidgetResult
  , dynamicToWidgetResult
  , unsafeBuildWidgetResult
  , buildWrappedWidgetResult
  , dynamicToWrappedWidgetResult
  , unsafeBuildWrappedWidgetResult
  , transformWrappedWidgetResult
  ) where

import           Reflex               (Behavior, Dynamic, Event, MonadHold,
                                       Reflex, buildDynamic, constDyn, current,
                                       fmapMaybe, leftmost, never, sample,
                                       switch, tagPromptlyDyn, updated)

import           Control.Lens         (makeLenses)
import           Control.Monad        (join)
import           Data.Functor.Compose (Compose (Compose), getCompose)


-- | A holder for a Dynamic and an event that signals a subset of updates.  Users may expect
-- | that any firing of the event is also a firing of the Dynamic.  unsafeBuild can be used to
-- | build one where that isn't true, but shouldn't be.
data WidgetResult t a = WidgetResult { _wrDyn :: Dynamic t a, _wrInternalEv :: Event t () }

makeLenses ''WidgetResult

widgetResultToDynamic :: WidgetResult t a -> Dynamic t a
widgetResultToDynamic = _wrDyn

updatedWidgetResult :: Reflex t => WidgetResult t a -> Event t a
updatedWidgetResult (WidgetResult d e) = tagPromptlyDyn d e

currentWidgetResult :: Reflex t => WidgetResult t a -> Behavior t a
currentWidgetResult = current . _wrDyn

buildWidgetResult :: (Reflex t, MonadHold t m) => Dynamic t a -> Event t a -> m (WidgetResult t a)
buildWidgetResult d0 updateEv = do
  d <- buildDynamic (sample $ current d0) $ leftmost [updated d0, updateEv]
  return $ WidgetResult d (() <$ updateEv)

dynamicWidgetResultToWidgetResult :: Reflex t => Dynamic t (WidgetResult t a) -> WidgetResult t a
dynamicWidgetResultToWidgetResult dwr =
  let d = join $ _wrDyn <$> dwr
      e = switch $ current $ _wrInternalEv <$> dwr
  in WidgetResult d e

-- here so a widget which returns just a Dynamic can return a WidgetResult
dynamicToWidgetResult :: Reflex t => Dynamic t a -> WidgetResult t a
dynamicToWidgetResult d = WidgetResult d (() <$ updated d)

-- this should only be used to retrofit a dynamic/event pair which already satisfy the
unsafeBuildWidgetResult :: Reflex t => Dynamic t a -> Event t a -> WidgetResult t a
unsafeBuildWidgetResult d e = WidgetResult d (() <$ e)


instance Reflex t => Functor (WidgetResult t) where
  fmap h (WidgetResult d e) = WidgetResult (h <$> d) e

instance Reflex t => Applicative (WidgetResult t) where
  pure a = WidgetResult (constDyn a) never
  (WidgetResult dF e1) <*> (WidgetResult da e2) = WidgetResult (dF <*> da) (leftmost [e1, e2])

instance Reflex t => Monad (WidgetResult t) where
  return = pure
  (WidgetResult da e) >>= f =
    let dwr = f <$> da
        WidgetResult db de = dynamicWidgetResultToWidgetResult dwr
    in WidgetResult db (leftmost [e, de])


-- this is only here so we can operate on the a directly via fmap or <*> when we need to.
type WrappedWidgetResult t f = Compose (WidgetResult t) f

buildWrappedWidgetResult :: (Functor f, Reflex t, MonadHold t m) => Dynamic t (f a) -> Event t (f a) -> m (WrappedWidgetResult t f a)
buildWrappedWidgetResult d e = Compose <$> buildWidgetResult d e

dynamicToWrappedWidgetResult :: (Reflex t, Functor f) => Dynamic t (f a) -> WrappedWidgetResult t f a
dynamicToWrappedWidgetResult = Compose . dynamicToWidgetResult

unsafeBuildWrappedWidgetResult :: (Reflex t, Functor f) => Dynamic t (f a) -> Event t (f a) -> WrappedWidgetResult t f a
unsafeBuildWrappedWidgetResult d e = Compose $ unsafeBuildWidgetResult d e

transformWrappedWidgetResult :: Reflex t => (forall b. f b -> g b) -> WrappedWidgetResult t f a -> WrappedWidgetResult t g a
transformWrappedWidgetResult nat wwr =
  let (WidgetResult d e) = getCompose wwr
  in Compose $ WidgetResult (nat <$> d) e

{-
instance (Functor f, Reflex t) => Functor (WrappedWidgetResult t f) where
  fmap h = WrappedWidgetResult . fmap (fmap h) . unWrappedWidgetResult

instance (Applicative f, Reflex t) => Applicative (WrappedWidgetResult t f) where
  pure a = WrappedWidgetResult $ pure $ pure a
  (WrappedWidgetResult (WidgetResult dF e1)) <*> (WrappedWidgetResult (WidgetResult da e2)) =
    WrappedWidgetResult $ WidgetResult (getCompose $ Compose dF <*> Compose da) (leftmost [e1, e2])
-}

{-
wrDyn :: Functor f => WidgetResult t f a -> Compose (Dynamic t) f a
wrDyn = _wrDyn

wrInternalEv :: WidgetResult t f a -> Event t ()
wrInternalEv = _wrInternalEv
-}
