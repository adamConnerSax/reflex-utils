{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Reflex.Dom.Contrib.Widgets.WidgetResult
  (
    WidgetResult
  , WrappedWidgetResult (..)
  , wrDyn
--  , wrInternalEv,
  , wrUpdateEv
  , buildWidgetResult
  , dynamicToWidgetResult
  , unsafeBuildWidgetResult
  , buildWrappedWidgetResult
  , dynamicToWrappedWidgetResult
  , unsafeBuildWrappedWidgetResult
  , transformWrappedWidgetResult
  ) where

import           Reflex (Dynamic, Event, MonadHold, Reflex, buildDynamic,
                         constDyn, current, never, sample, leftmost, updated, tagPromptlyDyn, switch, fmapMaybe)

import Data.Functor.Compose (Compose (Compose), getCompose)
import Control.Monad (join)
import Control.Lens (makeLenses)


-- | A holder for a Dynamic and an event that signals a subset of updates.  Users may expect
-- | that any firing of the event is also a firing of the Dynamic.  unsafeBuild can be used to
-- | build one where that isn't true, but shouldn't be.
data WidgetResult t a = WidgetResult { _wrDyn :: Dynamic t a, _wrInternalEv :: Event t () }

makeLenses ''WidgetResult

wrUpdateEv :: Reflex t => WidgetResult t a -> Event t a
wrUpdateEv (WidgetResult d e) = tagPromptlyDyn d e

buildWidgetResult :: (Reflex t, MonadHold t m) => Dynamic t a -> Event t a -> m (WidgetResult t a)
buildWidgetResult d0 updateEv = do
  d <- buildDynamic (sample $ current d0) $ leftmost [updated d0, updateEv]
  return $ WidgetResult d (() <$ updateEv)

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
        db = join $ _wrDyn <$> dwr
        de = switch $ current $ _wrInternalEv <$> dwr
    in WidgetResult db (leftmost [e, de])


-- this is only here so we can operate on the a directly via fmap or <*> when we need to.
newtype WrappedWidgetResult t f a = WrappedWidgetResult { unWrappedWidgetResult :: WidgetResult t (f a) }

buildWrappedWidgetResult :: (Functor f, Reflex t, MonadHold t m) => Dynamic t (f a) -> Event t (f a) -> m (WrappedWidgetResult t f a)
buildWrappedWidgetResult d e = WrappedWidgetResult <$> buildWidgetResult d e 

dynamicToWrappedWidgetResult :: (Reflex t, Functor f) => Dynamic t (f a) -> WrappedWidgetResult t f a
dynamicToWrappedWidgetResult = WrappedWidgetResult . dynamicToWidgetResult

unsafeBuildWrappedWidgetResult :: (Reflex t, Functor f) => Dynamic t (f a) -> Event t (f a) -> WrappedWidgetResult t f a
unsafeBuildWrappedWidgetResult d e = WrappedWidgetResult $ unsafeBuildWidgetResult d e

transformWrappedWidgetResult :: Reflex t => (forall b. f b -> g b) -> WrappedWidgetResult t f a -> WrappedWidgetResult t g a
transformWrappedWidgetResult nat wwr =
  let (WidgetResult d e) = unWrappedWidgetResult wwr
  in WrappedWidgetResult $ WidgetResult (nat <$> d) e

instance (Functor f, Reflex t) => Functor (WrappedWidgetResult t f) where
  fmap h = WrappedWidgetResult . fmap (fmap h) . unWrappedWidgetResult 

instance (Applicative f, Reflex t) => Applicative (WrappedWidgetResult t f) where
  pure a = WrappedWidgetResult $ pure $ pure a
  (WrappedWidgetResult (WidgetResult dF e1)) <*> (WrappedWidgetResult (WidgetResult da e2)) =
    WrappedWidgetResult $ WidgetResult (getCompose $ Compose dF <*> Compose da) (leftmost [e1, e2])


{-
wrDyn :: Functor f => WidgetResult t f a -> Compose (Dynamic t) f a
wrDyn = _wrDyn

wrInternalEv :: WidgetResult t f a -> Event t ()
wrInternalEv = _wrInternalEv
-}
