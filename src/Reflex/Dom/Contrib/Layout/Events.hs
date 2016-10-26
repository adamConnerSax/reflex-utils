{-# LANGUAGE FlexibleContexts #-}
module Reflex.Dom.Contrib.Layout.Events
       (
         addKeyedClassesBelow
       , addKeyedCssUpdateEventBelow
       , addKeyedCssUpdateEventsBelow
       , addMultipleKeyedCssUpdateEventsBelow
       , addKeyedCssUpdateEventBelow'
       , addKeyedCssUpdateEventsBelow'
       , getKeyedCssUpdateEvent
       ) where


import Reflex.Dom.Contrib.Layout.Types
import Reflex.Dom.Contrib.Layout.LayoutM

import qualified Reflex as R
import qualified Reflex.Dom as RD

import qualified Data.Map as M
import Control.Lens (set,(.~),(^.),(<>~),(<>=),(%=),(.=),use)
import Data.Monoid (mappend)
import Data.Maybe (fromJust)
import Data.List (foldl')
import Control.Monad (foldM)
import Control.Monad.Fix (MonadFix)


addKeyedClassBelow::(R.Reflex t,Monad m)=>(LayoutClassKey,CssClasses)->LayoutM t m ()
addKeyedClassBelow kc = addKeyedClassesBelow [kc]

addKeyedClassesBelow::(R.Reflex t,Monad m)=>[(LayoutClassKey,CssClasses)]->LayoutM t m ()
addKeyedClassesBelow keyedCss = LayoutM $ do
  let f m (key,css)  = M.insertWith mappend key css m
  lsClassMap %= (\m->foldl f m keyedCss)


getKeyedCssUpdateEvent::(R.Reflex t, MonadFix m, RD.MonadHold t m)=>LayoutClassKey->LayoutM t m (Maybe (R.Event t CssUpdate))
getKeyedCssUpdateEvent key = LayoutM $ do
  dynamicCssMap <- use lsDynamicCssMap
  return $ _lcdEvent <$> (M.lookup key dynamicCssMap)

addKeyedCssUpdateEventBelow::(R.Reflex t, MonadFix m,RD.MonadHold t m)=>LayoutClassKey->CssClasses->R.Event t CssUpdate->LayoutM t m ()
addKeyedCssUpdateEventBelow key initialCss ev = addKeyedCssUpdateEventsBelow (key,initialCss,[ev])

addKeyedCssUpdateEventBelow'::(R.Reflex t, MonadFix m,RD.MonadHold t m)=>LayoutClassKey->CssClasses->R.Event t CssUpdate->LayoutM t m (R.Event t CssUpdate)
addKeyedCssUpdateEventBelow' key initialCss ev = addKeyedCssUpdateEventsBelow' (key,initialCss,[ev])

addKeyedCssUpdateEventsBelow::(R.Reflex t, MonadFix m,RD.MonadHold t m)=>(LayoutClassKey,CssClasses,[R.Event t CssUpdate])->LayoutM t m ()
addKeyedCssUpdateEventsBelow kEv = addMultipleKeyedCssUpdateEventsBelow [kEv]

addKeyedCssUpdateEventsBelow'::(R.Reflex t, MonadFix m,RD.MonadHold t m)=>(LayoutClassKey,CssClasses,[R.Event t CssUpdate])->LayoutM t m (R.Event t CssUpdate)
addKeyedCssUpdateEventsBelow' kEv@(key,_,_) = do
  addMultipleKeyedCssUpdateEventsBelow [kEv]
  fromJust <$> getKeyedCssUpdateEvent key

addMultipleKeyedCssUpdateEventsBelow::(R.Reflex t, MonadFix m,RD.MonadHold t m)=>[(LayoutClassKey,CssClasses,[R.Event t CssUpdate])]->LayoutM t m ()
addMultipleKeyedCssUpdateEventsBelow kEvs = do
  newMap <- do
    let createDyn initialCss cssUpdEv = R.foldDyn (flip addCssUpdate) initialCss cssUpdEv
        f m (key,initialCss,cssEvs)  = do
          let newEv = R.mergeWith mergeCssUpdates $ maybe cssEvs (\x->(_lcdEvent x):cssEvs) (M.lookup key m)
          cssDyn <- createDyn initialCss newEv
          return $ M.insert key (LayoutClassDynamic cssDyn newEv) m
    dynamicCssMap <- askDynamicCssMap
    foldM f dynamicCssMap kEvs
  LayoutM $ lsDynamicCssMap .= newMap




