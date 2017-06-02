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
                         constDyn, current, never, sample, leftmost, updated, tagPromptlyDyn, switch)

import Control.Monad (join)

data WidgetResult t a = WidgetResult { _wrDyn :: Dynamic t a, _wrInternalEv :: Event t () }

buildWidgetResult :: (Reflex t, MonadHold t m) => Dynamic t a -> Event t a -> m (WidgetResult t a)
buildWidgetResult d0 updateEv = do
  d <- buildDynamic (sample $ current d0) $ leftmost [updated d0, updateEv]
  return $ WidgetResult d (() <$ updateEv)

wrDyn :: WidgetResult t a -> Dynamic t a
wrDyn = _wrDyn

wrInternalEv :: WidgetResult t a -> Event t ()
wrInternalEv = _wrInternalEv

wrUpdateEv :: Reflex t => WidgetResult t a -> Event t a
wrUpdateEv (WidgetResult d e) = tagPromptlyDyn d e

instance Reflex t => Functor (WidgetResult t) where
  fmap f (WidgetResult d e) = WidgetResult (f <$> d) e

instance Reflex t => Applicative (WidgetResult t) where
  pure a = WidgetResult (constDyn a) never
  (WidgetResult dFAB e1) <*> (WidgetResult dA e2) = WidgetResult (dFAB <*> dA) (leftmost [e1, e2])

instance Reflex t => Monad (WidgetResult t) where
  return = pure
  (WidgetResult d e) >>= f = 
    let dwr = f <$> d
        dda = wrDyn <$> dwr
        de = wrInternalEv <$> dwr
    in WidgetResult (join dda) (leftmost [e,switch $ current de])


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
