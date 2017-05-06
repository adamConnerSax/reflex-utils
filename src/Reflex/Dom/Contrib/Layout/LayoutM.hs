{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Reflex.Dom.Contrib.Layout.LayoutM
       (
         emptyClassMap
       , emptyDynamicCssMap
       , toStaticAttributes
       , addCssUpdate
       , LayoutClassDynamic(..)
       , LayoutClassDynamicMap
       , mergeCssUpdates
       , emptyCss
       , SupportsLayoutM
       , runLayoutMain
       , runLayoutM
       , addNewLayoutNode
       ) where

import           Data.Functor.Misc               (ComposeMaybe (..))
import           GHCJS.DOM.Types                 (MonadJSM (liftJSM'))
import qualified Reflex                          as R
import qualified Reflex.Class                    as R
import qualified Reflex.Dom                      as RD
import qualified Reflex.Host.Class               as RC


import           Control.Monad                   (sequence)
import           Control.Monad.Exception         (MonadAsyncException)
import           Control.Monad.Fix               (MonadFix)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Reader            (ask, runReaderT)
import           Control.Monad.Ref               (MonadRef (..), Ref)
import           Control.Monad.State             (StateT (StateT), evalStateT,
                                                  get, lift, runStateT)
import           Control.Monad.Trans             (MonadTrans)
import           Control.Monad.Trans.Control     (MonadTransControl (..))
import           Data.Coerce                     (coerce)
import qualified Data.Dependent.Map              as DMap
import           Data.List                       (foldl')
import qualified Data.List.NonEmpty              as NE
import qualified Data.Map                        as M
import           Data.Maybe                      (catMaybes, fromMaybe)
import qualified Data.Text                       as T
import           Reflex.Dom.Contrib.Layout.Types
--import qualified Reflex.Dom.Old                  as RD


import           Control.Lens                    ((^.))

import           Data.Monoid                     ((<>))
import           Data.Traversable                (sequenceA)


data LayoutNodeCss = LayoutNodeCss { _lncStatic::CssClasses, _lncDynamic::CssClasses }
instance IsCssClass LayoutNodeCss where
  toCssString (LayoutNodeCss s d) = toCssString s <> " " <> toCssString d

-- for left fold.  needs to be flipped for foldDyn.
addCssUpdate::CssClasses->CssUpdate->CssClasses
addCssUpdate _ (UpdateDynamic d) = d
addCssUpdate d (AddToDynamic d') = d <> d'

-- biased to 2nd arg.
mergeCssUpdates::CssUpdate->CssUpdate->CssUpdate
mergeCssUpdates _ (UpdateDynamic d)                 = UpdateDynamic d
mergeCssUpdates (UpdateDynamic d) (AddToDynamic d') = UpdateDynamic (d <> d')
mergeCssUpdates (AddToDynamic d1) (AddToDynamic d2) = AddToDynamic (d1 <> d2)

emptyClassMap::LayoutClassMap
emptyClassMap = M.empty

emptyDynamicCssMap::R.Reflex t=>LayoutClassDynamicMap t
emptyDynamicCssMap = M.empty

newInfo::R.Reflex t=>LayoutDescription t->LayoutInfo t
newInfo ld = LayoutInfo ld (CssClasses []) Nothing

--TODO: Figure out how to use "pull" here to make "sample" okay
dynamicCss::(R.Reflex t, R.MonadHold t m, MonadFix m)=>LayoutDescription t->LayoutClassDynamicMap t->m (Maybe (R.Dynamic t CssClasses))
dynamicCss desc dynMap = do
  let classKeys = (desc ^. ldLayoutClassKeys)
      makeDyn::(R.Reflex t, MonadFix m, R.MonadHold t m)=>LayoutClassDynamic t->m (R.Dynamic t CssClasses)
      makeDyn (LayoutClassDynamic initDyn ev) = do
        initial <- R.sample (R.current initDyn)
        R.foldDyn (flip addCssUpdate) initial ev
      getMaybeDyn key = makeDyn <$> M.lookup key dynMap -- Maybe (m (Dynamic t CssClasses))
  getLMaybeDyn <- sequence $ catMaybes (getMaybeDyn <$> classKeys) --m [Dynamic t CssClasses]
  if null classKeys || null getLMaybeDyn then return Nothing else Just <$> return (mconcat getLMaybeDyn)


addNewLayoutNode::(SupportsLayoutM t m,RD.PostBuild t m, R.MonadHold t m,MonadFix m)=>LayoutDescription t->LayoutM t m a->LayoutM t m a
addNewLayoutNode desc child = LayoutM $ do
  lc <- ask
  ls <- get
  let classKeys = desc ^. ldLayoutClassKeys
      staticClassMap = ls ^. lsClassMap
      dynamicCssMap = ls ^. lsDynamicCssMap
      staticClasses = (mconcat . catMaybes $ fmap (`M.lookup` staticClassMap) classKeys) <> (desc ^. ldClasses)
      child' = runLayoutM child ls lc
  lift . lift $ do
    mDynClasses <- dynamicCss desc dynamicCssMap
    RD.elDynAttr "div" (classesToAttributesDyn' staticClasses mDynClasses) child'

cssToAttr::IsCssClass c=>c->RD.AttributeMap
cssToAttr cssClass = "class" RD.=: (toCssString cssClass)

updateCss::LayoutNodeCss->CssUpdate->LayoutNodeCss
updateCss (LayoutNodeCss s _) (UpdateDynamic d) = LayoutNodeCss s d
updateCss (LayoutNodeCss s d) (AddToDynamic d') = LayoutNodeCss s (d <> d')

updateCssNE::NE.NonEmpty CssUpdate->LayoutNodeCss->LayoutNodeCss
updateCssNE cssUpdates nodeCss = foldl' updateCss nodeCss (NE.toList cssUpdates)

classesToAttributesDyn::R.Reflex t=>CssClasses->R.Dynamic t CssClasses->R.Dynamic t (M.Map T.Text T.Text)
classesToAttributesDyn staticCss dynamicCssDyn = (cssToAttr . LayoutNodeCss staticCss) <$> dynamicCssDyn

classesToAttributesDyn'::R.Reflex t=>CssClasses->Maybe (R.Dynamic t CssClasses)->R.Dynamic t (M.Map T.Text T.Text)
classesToAttributesDyn' staticCss mDynamicCssDyn =
  let x = fromMaybe (R.constDyn emptyCss) mDynamicCssDyn in classesToAttributesDyn staticCss x

-- utilities for map merging
-- biased to 2nd argument for initial conditons
mergeLayoutClassDynamic::(R.Reflex t, R.MonadHold t m,MonadFix m)=>LayoutClassDynamic t->LayoutClassDynamic t->m (LayoutClassDynamic t)
mergeLayoutClassDynamic (LayoutClassDynamic _ evA) (LayoutClassDynamic initialBDyn evB) = do
  let newEv = R.mergeWith mergeCssUpdates [evA,evB]
  initialValue <- R.sample $ R.current initialBDyn
  newInitialDyn <- R.foldDyn (flip addCssUpdate) initialValue newEv
  return $ LayoutClassDynamic newInitialDyn newEv

unionM::(Monad m,Ord k)=>(a->a->m a)->M.Map k a->M.Map k a->m (M.Map k a)
unionM f m1 m2 = sequenceA $ M.mergeWithKey (\_ a b -> Just $ f a b) (fmap return) (fmap return) m1 m2


runLayoutMain::(SupportsLayoutM t m, RD.PostBuild t m)=>LayoutConfig t->LayoutM t m a->m a
runLayoutMain lc lma = runLayoutM lma (LayoutS emptyClassMap emptyDynamicCssMap) lc

-- Class Instances
liftLM::Monad m=>m a->LayoutM t m a
liftLM = LayoutM . lift . lift

instance RD.MonadSample t m=>RD.MonadSample t (LayoutM t m) where
  sample = liftLM . RD.sample

instance RD.MonadHold t m=>RD.MonadHold t (LayoutM t m) where
  hold a0 = liftLM . RD.hold a0
  holdDyn a0 = liftLM . RD.holdDyn a0
  holdIncremental a0 = liftLM . RD.holdIncremental a0

instance MonadTrans (LayoutM t) where
  lift = liftLM

runLayoutM::Functor m=>LayoutM t m a->LayoutS t->LayoutConfig t->m a
runLayoutM lma ls lc = fst <$> runReaderT (runStateT (unLayoutM lma) ls) lc


instance MonadTransControl (LayoutM t) where
  type StT (LayoutM t) a = a
  liftWith f = LayoutM $ do
    lc <- ask
    ls <- get
    lift . lift $ f $ \t -> runLayoutM t ls lc
  restoreT  = liftLM

--type SupportsImmediateDomBuilder t m = (Reflex t, MonadIO m, MonadHold t m, MonadFix m, PerformEvent t m, Performable m ~ m, MonadReflexCreateTrigger t m, Deletable t m, MonadRef m, Ref m ~ Ref IO)


--TriggerEvent makes me nervous
type SupportsLayoutM t m = (R.Reflex t, MonadIO m, R.MonadHold t m, MonadFix m, R.PerformEvent t m, RC.MonadReflexCreateTrigger t m, MonadRef m, Ref m ~ Ref IO, RD.DomBuilder t m, RD.DomSpace (RD.DomBuilderSpace m), RD.DomBuilderSpace m ~ RD.GhcjsDomSpace, RD.TriggerEvent t m, Ref (R.Performable m) ~ Ref IO, RD.HasWebView (R.Performable m), MonadAsyncException (R.Performable m), MonadRef (R.Performable m), R.MonadSample t (R.Performable m))

instance (RD.PostBuild t m, SupportsLayoutM t m) => RD.DomBuilder t (LayoutM t m) where
  type DomBuilderSpace (LayoutM t m) = RD.DomBuilderSpace m
{-  placeholder (RD.PlaceholderConfig insertAbove delete) = LayoutM $ do
    lc <- ask
    ls <- get
    let f x = runLayoutM x ls lc
        insertAbove' = fmap f insertAbove
    lift . lift $ RD.placeholder (RD.PlaceholderConfig insertAbove' delete)
-}
instance RD.PerformEvent t m=>RD.PerformEvent t (LayoutM t m) where
  type Performable (LayoutM t m) = RD.Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ R.performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ R.performEvent e

instance RD.PostBuild t m => RD.PostBuild t (LayoutM t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift RD.getPostBuild

instance RC.MonadReflexCreateTrigger t m => RC.MonadReflexCreateTrigger t (LayoutM t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . RC.newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ RC.newFanEventWithTrigger f

instance MonadRef m => MonadRef (LayoutM t m) where
  type Ref (LayoutM t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r


instance RD.TriggerEvent t m => RD.TriggerEvent t (LayoutM t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = lift RD.newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = lift RD.newTriggerEventWithOnComplete
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete = lift . RD.newEventWithLazyTriggerWithOnComplete


instance R.MonadAdjust t m => R.MonadAdjust t (StateT s m) where
  runWithReplace a0 a' = do
    s <- get
    lift $ R.runWithReplace (evalStateT a0 s) $ fmap (`evalStateT` s) a'
  traverseDMapWithKeyWithAdjust f dm0 dm' = do
    s <- get
    lift $ R.traverseDMapWithKeyWithAdjust (\k v -> evalStateT (f k v) s) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    s <- get
    lift $ R.traverseDMapWithKeyWithAdjustWithMove (\k v -> evalStateT (f k v) s) dm0 dm'


instance R.MonadAdjust t m => R.MonadAdjust t (LayoutM t m) where
  runWithReplace a0 a' = LayoutM $ R.runWithReplace (coerce a0) (RD.coerceEvent a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = LayoutM $ R.traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = LayoutM $ R.traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'

{-
instance RD.HasWebView m => RD.HasWebView (LayoutM t m) where
  type WebViewPhantom (LayoutM t m) = RD.WebViewPhantom m
  askWebView = lift RD.askWebView
-}

instance MonadJSM m => MonadJSM (LayoutM t m) where
  liftJSM' = liftLM . liftJSM'


instance RD.HasJSContext m => RD.HasJSContext (LayoutM t m) where
  type JSContextPhantom (LayoutM t m) = RD.JSContextPhantom m
  askJSContext = lift RD.askJSContext

instance RD.HasJS js m => RD.HasJS js (LayoutM t m) where
  type JSX (LayoutM t m) = RD.JSX m
  liftJS = lift . RD.liftJS

-- For debugging

printLayoutConfig::LayoutConfig t->IO ()
printLayoutConfig lc = do
  putStrLn $ "static Css=" ++ show (M.toList (_lcStaticCssMap lc))
  putStrLn $ "dynamicCss Keys=" ++ show (M.keys (_lcDynamicCssMap lc))

