{-# LANGUAGE FlexibleInstances #-}
module Reflex.Dom.Contrib.Widgets.WidgetResult
  (
    WidgetResult,
    wrDyn,
    wrInternalEv,
    wrUpdateEv,
    buildWidgetResult
  )
where

import           Reflex (Dynamic, Event, MonadHold, Reflex, buildDynamic,
                         constDyn, current, never, sample, leftmost, updated, tagPromptlyDyn, switch, fmapMaybe)

import Data.Functor.Compose (Compose (Compose), getCompose)
import Control.Monad (join)
import Data.Functor.Identity (Identity (runIdentity))

data WidgetResult t f a = WidgetResult { _wrDyn :: Compose (Dynamic t) f a, _wrInternalEv :: Event t () }

{-
actOutside :: (g a -> q b) -> Compose f g a -> Compose f q b
actOutside h = Compose . h . getCompose

actInside :: (a -> b) -> Compose f g a -> Compose f g b
actInside h = Compose . fmap h . getCompose
-}

buildWidgetResult :: (Reflex t, Functor f, MonadHold t m) => Compose (Dynamic t) f a -> Compose (Event t) f a -> m (WidgetResult t f a)
buildWidgetResult dfa0 updateFaEv = do
  let ucDfa0 = getCompose dfa0
      ucUpdEv = getCompose updateFaEv
  d <- buildDynamic (sample $ current ucDfa0) $ leftmost [updated ucDfa0, ucUpdEv]
  return $ WidgetResult (Compose d) (() <$ ucUpdEv)

wrDyn :: Functor f => WidgetResult t f a -> Compose (Dynamic t) f a
wrDyn = _wrDyn

wrInternalEv :: WidgetResult t f a -> Event t ()
wrInternalEv = _wrInternalEv

wrUpdateEv :: (Functor f, Reflex t) => WidgetResult t f a -> Compose (Event t) f a
wrUpdateEv (WidgetResult d e) = Compose $ tagPromptlyDyn (getCompose d) e

instance (Functor f, Reflex t) => Functor (WidgetResult t f) where
  fmap h (WidgetResult d e) = WidgetResult (h <$> d) e

instance (Applicative f, Reflex t) => Applicative (WidgetResult t f) where
  pure a = WidgetResult (Compose $ pure $ pure a) never
  (WidgetResult dFAB e1) <*> (WidgetResult dA e2) = WidgetResult (dFAB <*> dA) (leftmost [e1, e2])

{-
instance Reflex t => Monad (WidgetResult t Identity) where
  return = pure
  (WidgetResult d e) >>= f = 
    let dwr = f <$> d
        dda = wrDyn <$> dwr
        de = switch $ current $ fmap runIdentity $ getCompose $ wrInternalEv <$> dwr
    in WidgetResult (join dda) (leftmost [e, de])
-}

{-
Applicative is lawful, essentially because Dynamic is lawful and a leftmost of Events that only return one value, (), doesn't depend on order.
1. pure id <*> v = v
   (WidgetResult (constDyn id) never) <*> (WidgetResult dv ev) = WidgetResult dv ev
   WidgetResult (constDyn id <*> dv) (leftmost [ev, never]) = WidgetResult dv ev
   WidgetResult dv ev = WidgetResult dv ev

2. pure f <*> pure x = pure (f x)
   WidgetResult (constDyn f) never <*> WidgetResult (constDyn x) never = WidgetResult (constDyn $ f x) never
   WidgetResult (constDyn f <*> constDyn x) (leftmost [never, never]) = WidgetResult (constDyn $ f x) never
   WidgetResult (constDyn $ f x) never =  WidgetResult (constDyn $ f x) never

3. u <*> pure y = pure ($ y) <*> u
   WidgetResult du eu <*> WidgetResult (constDyn y) never = WidgetResult (constDyn ($ y)) never <*> WidgetResult du eu
   WidgetResult (du <*> constDyn y) (leftmost [eu, never]) = WidgetResult (pure ($ y) <*> du) (leftmost [never, eu])
   WidgetResult (du <*> pure y) eu = WidgetResult (du <*> pure y) eu

4. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
   There are a lot of steps here.  But we know the Dynamic part works out because Dynamic is applicative.
   So we only need to check the events.  But leftmost of a set of Event t () is order independent since all events carry the same value.
   And pure (.) does not contribute any events. So these are the same.
-}
