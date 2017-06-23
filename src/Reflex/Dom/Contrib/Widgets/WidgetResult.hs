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
  , constWidgetResult
  , buildWidgetResult
--  , updateWidgetResult
  , dynamicWidgetResultToWidgetResult
  , widgetResultOfDynamicToWidgetResult
  , dynamicToWidgetResult
  , buildReadOnlyWidgetResult
  , conservativeRetrofitWidgetResult
  , unsafeBuildWidgetResult
  , buildWrappedWidgetResult
  , dynamicToWrappedWidgetResult
  , unsafeBuildWrappedWidgetResult
  , transformWrappedWidgetResult
  ) where

import           Reflex                        (Behavior, Dynamic, Event,
                                                MonadHold, Reflex, buildDynamic,
                                                constDyn, current, fmapMaybe,
                                                leftmost, never, sample, switch,
                                                tagPromptlyDyn, updated)

import           Reflex.Dom.Contrib.EventUtils (leftWhenNotRight)

import           Control.Lens                  (makeLenses)
import           Control.Monad                 (join)
import           Data.Functor.Compose          (Compose (Compose), getCompose)


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


-- There is a subtlety here.  What if d0 updates in the same frame as an updateEv occurs?
-- We need to choose which one wins.
-- I think we should prefer updates to d0 since they will often reset the control.
-- but that means we need to suppress the updateEv iff it occurs in the same frame as
-- an update to d0.
-- If we don't do the suppression, the leftmost in buildDynamic needs to put updateEv first.
buildWidgetResult :: (Reflex t, MonadHold t m) => Dynamic t a -> Event t a -> m (WidgetResult t a)
buildWidgetResult d0 updateEv = do
  let d0Ev = updated d0
  d <- buildDynamic (sample $ current d0) $ leftmost [d0Ev, updateEv] -- order reversed
  return $ WidgetResult d (() <$ leftWhenNotRight updateEv d0Ev)

{-
-- this adds events to the WidgetResult, keeping the old
updateWidgetResult :: (Reflex t, MonadHold t m) => WidgetResult t a -> Event t a -> m (WidgetResult t a)
updateWidgetResult (WidgetResult d0 e0) e = do
  let updatedEv = leftmost [e0, () <$ e]
  updatedDyn <- buildDynamic (sample $ current d0) $ leftmost [updated d0, e]
  return $ WidgetResult updatedDyn updatedEv
-}

dynamicWidgetResultToWidgetResult :: Reflex t => Dynamic t (WidgetResult t a) -> WidgetResult t a
dynamicWidgetResultToWidgetResult dwr =
  let d = join $ _wrDyn <$> dwr
      e = switch $ current $ _wrInternalEv <$> dwr
  in WidgetResult d e

widgetResultOfDynamicToWidgetResult :: (Reflex t, MonadHold t m) => WidgetResult t (Dynamic t a) -> m (WidgetResult t a)
widgetResultOfDynamicToWidgetResult wrd = do
  let d = join $ _wrDyn wrd
  ed <- buildDynamic (sample $ current $ _wrDyn wrd) $ updatedWidgetResult wrd
  return $ WidgetResult d (() <$ updated ed) -- ??

-- here so a widget which returns just a Dynamic can return a WidgetResult
dynamicToWidgetResult :: Reflex t => Dynamic t a -> WidgetResult t a
dynamicToWidgetResult d = WidgetResult d (() <$ updated d)

buildReadOnlyWidgetResult :: Reflex t => Dynamic t a -> WidgetResult t a
buildReadOnlyWidgetResult d = WidgetResult d never

constWidgetResult :: Reflex t => a -> WidgetResult t a
constWidgetResult = buildReadOnlyWidgetResult . constDyn

-- this will make any Dynamic t a -> m (Dynamic t b) widget into one which returns a WidgetResult.  But it can't know what's happening inside so
-- it just assumes that any update coming out in the same frame as the input is updated is a result of that and
-- doesn't include that in the "change" piece.  This could be wrong if, coincidentally, the widget changes something in the same frame as
-- the input is updated.
conservativeRetrofitWidgetResult :: (Reflex t, MonadHold t m) => (Dynamic t a -> m (Dynamic t b)) -> Dynamic t a -> m (WidgetResult t b)
conservativeRetrofitWidgetResult w da = do
  db <- w da
  let filteredEvOut = leftWhenNotRight (updated db) (updated da)
  return $ unsafeBuildWidgetResult db filteredEvOut

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
