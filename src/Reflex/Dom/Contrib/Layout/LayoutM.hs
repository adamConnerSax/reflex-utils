{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

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
       , runStyledLayout
       , runLayout
       , runLayoutMain
       , getLayoutPropertyDef
       , addNewClassesToTreeTop
       , addNewLayoutNode
       ) where

import qualified GHCJS.DOM.Element               as E
import           GHCJS.DOM.Types                 (IsElement)
import qualified Reflex                          as R
import qualified Reflex.Class                    as RC
import qualified Reflex.Dom                      as RD
import qualified Reflex.Dom.Builder.Class        as RD
import qualified Reflex.Dom.Class                as RD
import qualified Reflex.Host.Class               as RC
--import qualified Reflex.Dom.Old                  as RD


import           Control.Lens                    (makeClassy, makeLenses, set,
                                                  use, view, (%=), (.=), (.~),
                                                  (<>=), (<>~), (^.))
import           Control.Monad                   (forM, forM_, mapM_, sequence)
import           Control.Monad.Fix               (MonadFix)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Reader            (MonadReader, ReaderT, ask,
                                                  runReaderT)
import           Control.Monad.Ref               (MonadRef, Ref)
import           Control.Monad.State             (MonadState, StateT, get, lift,
                                                  put, runStateT)
import           Control.Monad.Trans             (MonadTrans)
import           Control.Monad.Trans.Control     (MonadTransControl (..),
                                                  liftThrough)
import           Control.Monad.Exception         (MonadAsyncException)
import           Data.List                       (foldl')
import qualified Data.List.NonEmpty              as NE
import qualified Data.Map                        as M
import           Data.Maybe                      (catMaybes, fromJust)
import           Data.Monoid                     ((<>))
import           Data.String                     (unwords, words)
import qualified Data.Text                       as T
import           Data.Traversable                (sequenceA)

import           Reflex.Dom.Contrib.Layout.Types

noProperties::LayoutPropertyMap
noProperties = M.empty

data LayoutNodeCss = LayoutNodeCss { _lncStatic::CssClasses, _lncDynamic::CssClasses }
instance IsCssClass LayoutNodeCss where
  toCssString (LayoutNodeCss s d) = toCssString s <> " " <> toCssString d

-- for left fold.  needs to be flipped for foldDyn.
addCssUpdate::CssClasses->CssUpdate->CssClasses
addCssUpdate _ (UpdateDynamic d) = d
addCssUpdate d (AddToDynamic d') = d <> d'

-- biased to 2nd arg.
mergeCssUpdates::CssUpdate->CssUpdate->CssUpdate
mergeCssUpdates _ (UpdateDynamic d) = UpdateDynamic d
mergeCssUpdates (UpdateDynamic d) (AddToDynamic d') = UpdateDynamic (d <> d')
mergeCssUpdates (AddToDynamic d1) (AddToDynamic d2) = AddToDynamic (d1 <> d2)

emptyClassMap::LayoutClassMap
emptyClassMap = M.empty

emptyDynamicCssMap::R.Reflex t=>LayoutClassDynamicMap t
emptyDynamicCssMap = M.empty

newInfo::R.Reflex t=>LayoutDescription t->LayoutInfo t
newInfo ld = LayoutInfo ld (CssClasses []) Nothing Nothing

newTree::R.Reflex t=>LayoutDescription t->LayoutTree t
newTree desc = LayoutNode (newInfo desc) []

getLayoutProperty::R.Reflex t=>LayoutPropertyKey->LayoutTree t->Maybe LayoutProperty
getLayoutProperty key lTree =
  let pm = lTree ^. (lnInfo.liDescription.ldProperties) in M.lookup key pm

getLayoutPropertyDef::R.Reflex t=>LayoutProperty->LayoutPropertyKey->LayoutTree t->LayoutProperty
getLayoutPropertyDef def key lTree = maybe def id $ getLayoutProperty key lTree

addNewClassesToTreeTop::R.Reflex t=>CssClasses->LayoutTree t->LayoutTree t
addNewClassesToTreeTop cssCs lt = lnInfo.liNewClasses <>~ cssCs $ lt

addNode::R.Reflex t=>LayoutTree t->LayoutTree t->LayoutTree t
addNode ln newChild = lnChildren <>~ [newChild] $ ln

addNode'::(R.Reflex t,Monad m)=>LayoutTree t->LayoutM t m ()
addNode' tree = LayoutM $ do
  lsTree %= flip addNode tree

addStaticClasses::Monad m=>LayoutDescription t->LayoutTree t->LayoutM t m (LayoutTree t)
addStaticClasses desc lTree = LayoutM $ do
  classMap <- use lsClassMap
  let getCss key = maybe emptyCss id $ M.lookup key classMap
      css = mconcat $ getCss <$> (desc ^. ldLayoutClassKeys)
  return (lnInfo.liNewClasses <>~ css $ lTree)

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
  if (null classKeys) || (null getLMaybeDyn) then return Nothing else Just <$> (return $ mconcat getLMaybeDyn)

addDynamicCss::(R.Reflex t, R.MonadHold t m, MonadFix m)=>LayoutDescription t->LayoutTree t->LayoutM t m (LayoutTree t)
addDynamicCss desc lTree = do
  dynamicCssMap <- use lsDynamicCssMap
  mDyn <- dynamicCss desc dynamicCssMap
  return (lnInfo.liDynamicCss .~ mDyn $ lTree)

setElement::E.Element->LayoutTree t->LayoutTree t
setElement elt lTree = (lnInfo.liElt .~ Just elt) $ lTree

newLayoutS::(R.Reflex t,Monad m)=>LayoutDescription t->LayoutM t m (LayoutS t)
newLayoutS desc = LayoutS (newTree desc) <$> use lsClassMap <*> use lsDynamicCssMap

{-
addNewLayoutNode::(RD.MonadWidget t m,R.MonadHold t m,MonadFix m)=>LayoutDescription t->LayoutM t m a->LayoutM t m a
addNewLayoutNode desc newChildren = do
  emptyLS <- newLayoutS desc
  (elt,(x,childLS)) <- lift $ RD.el' "div" $ runStateT newChildren emptyLS
  let taggedChild = setElement (RDC._raw_element elt) (childLS ^. lsTree)
  addStaticClasses desc taggedChild >>= addDynamicCss desc >>= addNode'
  return x
-}

-- ??
addNewLayoutNode::(RD.MonadWidget t m,R.MonadHold t m,MonadFix m)=>LayoutDescription t->LayoutM t m a->LayoutM t m a
addNewLayoutNode desc child = do
  curLS <- get
  emptyLS <- newLayoutS desc
  put emptyLS
  x <- child
  childLS <- get
  addStaticClasses desc (childLS ^. lsTree) >>= addDynamicCss desc >>= addNode'
  put curLS
  return x

cssToAttr::IsCssClass c=>c->RD.AttributeMap
cssToAttr cssClass = ("class" RD.=: (toCssString cssClass))

-- add attributes to element.  If no events are hooked up, do it statically
--addClassesStatic::(RD.MonadWidget t m, IsElement e)=>CssClasses->e->m ()
--addClassesStatic css elt = RD.addAttributes (cssToAttr css) elt

updateCss::LayoutNodeCss->CssUpdate->LayoutNodeCss
updateCss (LayoutNodeCss s _) (UpdateDynamic d) = LayoutNodeCss s d
updateCss (LayoutNodeCss s d) (AddToDynamic d') = LayoutNodeCss s (d <> d')

updateCssNE::NE.NonEmpty CssUpdate->LayoutNodeCss->LayoutNodeCss
updateCssNE cssUpdates nodeCss = foldl' updateCss nodeCss (NE.toList cssUpdates)

classesToAttributesDyn::R.Reflex t=>CssClasses->R.Dynamic t CssClasses->R.Dynamic t (M.Map T.Text T.Text)
classesToAttributesDyn staticCss dynamicCssDyn = (cssToAttr . LayoutNodeCss staticCss) <$> dynamicCssDyn

{-
addClassesDynamic::(RD.MonadWidget t m, IsElement e)=>CssClasses->e->R.Dynamic t CssClasses->m ()
addClassesDynamic staticCss elt dynamicCssDyn = do
  cssDyn <- RD.mapDyn (\x->(LayoutNodeCss staticCss x)) dynamicCssDyn
  attrsDyn <- RD.mapDyn cssToAttr cssDyn
  RD.addAttributes attrsDyn elt

addClasses::(RD.MonadWidget t m, IsElement e)=>CssClasses->Maybe (R.Dynamic t CssClasses)->e->m ()
addClasses css mCssDyn elt = maybe (addClassesStatic css elt) (addClassesDynamic css elt) mCssDyn
-}

-- utilities for map merging
-- biased to 2nd argument for initial conditons
mergeLayoutClassDynamic::(R.Reflex t, R.MonadHold t m,MonadFix m)=>LayoutClassDynamic t->LayoutClassDynamic t->m (LayoutClassDynamic t)
mergeLayoutClassDynamic (LayoutClassDynamic initialADyn evA) (LayoutClassDynamic initialBDyn evB) = do
  let newEv = R.mergeWith mergeCssUpdates [evA,evB]
  initialValue <- R.sample $ R.current initialBDyn
  newInitialDyn <- R.foldDyn (flip addCssUpdate) initialValue newEv
  return $ LayoutClassDynamic newInitialDyn newEv

unionM::(Monad m,Ord k)=>(a->a->m a)->M.Map k a->M.Map k a->m (M.Map k a)
unionM f m1 m2 = sequenceA $ M.mergeWithKey (\_ a b -> Just $ f a b) (fmap return) (fmap return) m1 m2

-- do current node then children
runLayoutTree::(RD.MonadWidget t m,MonadIO (R.PushM t))=>LayoutConfig t->CssClasses->LayoutTree t->m ()
runLayoutTree lc classesForAll lt = do
  let lt' = (lt  ^. lnInfo.liDescription.ldLayoutF) lc lt
--      elt = fromJust (lt' ^. lnInfo.liElt)
      classesForElt = (lt' ^. lnInfo.liNewClasses) <> classesForAll
      children = (lt' ^. lnChildren)
      dynamicCss = maybe (R.constDyn mempty) id (lt' ^. lnInfo.liDynamicCss)
      child = mapM_ (runLayoutTree lc classesForAll) children
  RD.elDynAttr "div" (classesToAttributesDyn classesForElt dynamicCss) child


runStyledLayout::(RD.MonadWidget t m, MonadIO (RD.PushM t))=>CssClasses->LayoutClassMap->LayoutClassDynamicMap t->LayoutConfig t->LayoutM t m a->m a
runStyledLayout classesForAll staticCssMap dynamicCssMap conf layout = do
  combinedDynamicCssMap <- unionM mergeLayoutClassDynamic (conf ^. lcDynamicCssMap) dynamicCssMap --dynamic classes are
  let combinedStaticCssMap = M.union staticCssMap (conf ^. lcStaticCssMap) --static classes are overwritten by later classes. ??
      emptyD = LayoutDescription (const id) noProperties []
      emptyS = LayoutS (newTree emptyD) combinedStaticCssMap combinedDynamicCssMap
  (x,layoutS) <- runReaderT (runStateT (unLayoutM layout) emptyS) conf
  mapM_ (runLayoutTree conf classesForAll) (layoutS ^. lsTree.lnChildren) -- there is no elt on top
  return x

runLayout::(RD.MonadWidget t m, MonadIO (RD.PushM t))=>LayoutClassMap->LayoutClassDynamicMap t -> LayoutConfig t->LayoutM t m a->m a
runLayout = runStyledLayout emptyCss

runLayoutMain::(RD.MonadWidget t m, MonadIO (RD.PushM t))=>LayoutConfig t->LayoutM t m a->m a
runLayoutMain = runLayout emptyClassMap emptyDynamicCssMap

-- Class Instances
liftLM::Monad m=>m a->LayoutM t m a
liftLM = LayoutM . lift . lift

instance RD.MonadSample t m=>RD.MonadSample t (LayoutM t m) where
  sample = liftLM . RD.sample

instance RD.MonadHold t m=>RD.MonadHold t (LayoutM t m) where
  hold a0 = liftLM . RD.hold a0
  holdDyn a0 = liftLM . RD.holdDyn a0
  holdIncremental a0 = liftLM . RD.holdIncremental a0


{-
 -- Is this necessary?
instance (RD.MonadWidget t m,RD.MonadIORestore m, MonadIO (RD.PushM t)) => RD.MonadIORestore (LayoutM t m) where
  askRestore = do
    lc <- ask
    dynamicCssMap <- use lsDynamicCssMap
    staticCssMap <- use lsClassMap
    parentRestore <- liftL RD.askRestore
    return $ RD.Restore $ \sma -> RD.restore parentRestore $ runLayout staticCssMap dynamicCssMap lc sma


instance RD.HasPostGui t h m => RD.HasPostGui t h (LayoutM t m) where
  askPostGui = liftL RD.askPostGui
  askRunWithActions = liftL RD.askRunWithActions
-}
-- which to use??  Both work on demo.  So far.  Need to add widgetHold or dyn...
layoutInside::(MonadIO (RD.PushM t),SupportsLayoutM t m)=>(m a -> m b)->LayoutM t m a->LayoutM t m b
layoutInside f w = do
  lc <- askLayoutConfig
  dynamicCssMap <- use lsDynamicCssMap
  staticCssMap <- use lsClassMap
  liftLM $ f $ runLayout staticCssMap dynamicCssMap lc w

noLayoutInside::(RD.MonadWidget t m ,s~LayoutS t)=>(d->(b,s))->(m (a,s) -> m d)->LayoutM t m a->LayoutM t m b
noLayoutInside f action w = do
  s <- get
  lc <- askLayoutConfig
  (x,s') <- f <$> (liftLM $ action $ runReaderT (runStateT (unLayoutM w) s) lc)
  put s'
  return x

--deriving instance R.MonadSample t m => R.MonadSample t (LayoutM t m)
--deriving instance R.MonadHold t m   => R.MonadHold t (LayoutM t m)

instance MonadTrans (LayoutM t) where
  lift = liftLM


--type LayoutMStack t m = StateT (LayoutS t) (ReaderT (LayoutConfig t) m)

runLayoutM::LayoutM t m a->LayoutS t->LayoutConfig t->m (a,LayoutS t)
runLayoutM lma ls lc = runReaderT (runStateT (unLayoutM lma) ls) lc


instance MonadTransControl (LayoutM t) where
  type StT (LayoutM t) a = (a,LayoutS t)
  liftWith f = do
    lc <- askLayoutConfig
    ls <- get
    liftLM $ f $ \t -> runLayoutM t ls lc
  restoreT x = do
    (a,ls) <- liftLM x
    put ls
    return a

{-
instance (RD.DomBuilder t m, RD.DomBuilderSpace (LayoutM t m) ~ RD.GhcjsDomSpace)=>MonadTransControl (LayoutM t) where
  type StT (LayoutM t) a = a
  liftWith f = do
    lc <- ask
    dynamicCssMap <- use lsDynamicCssMap
    staticCssMap <- use lsClassMap
    liftLM $ f $ \t -> runLayout staticCssMap dynamicCssMap lc t
  restoreT = liftLM
-}



type SupportsLayoutM t m = (R.Reflex t, MonadIO m, R.MonadHold t m, MonadFix m, R.PerformEvent t m, R.Performable m ~ m, RD.Deletable t m, MonadRef m, Ref m ~ Ref IO, MonadRef (LayoutM t m), Ref (LayoutM t m) ~ Ref IO, RD.DomBuilder t m, RD.DomSpace (RD.DomBuilderSpace m), RD.DomBuilderSpace m ~ RD.GhcjsDomSpace, MonadAsyncException m, RD.TriggerEvent t m, RD.PostBuild t m, RC.MonadReflexCreateTrigger t m, RD.HasWebView m)


instance SupportsLayoutM t m => RD.DomBuilder t (LayoutM t m) where
  type DomBuilderSpace (LayoutM t m) = RD.DomBuilderSpace m
  element t cfg child = liftWith $ \run -> RD.element t (RD.liftElementConfig cfg) $ fst <$> run child
  selectElement cfg child = do
    let cfg' = cfg
          { RD._selectElementConfig_elementConfig = RD.liftElementConfig $ RD._selectElementConfig_elementConfig cfg
          }
    liftWith $ \run -> RD.selectElement cfg' $ fst <$> run child
  placeholder = lift . RD.placeholder



instance RD.Deletable t m => RD.Deletable t (LayoutM t m) where
  deletable d = liftThrough $ RD.deletable d

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


instance RD.TriggerEvent t m => RD.TriggerEvent t (LayoutM t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = lift RD.newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = lift RD.newTriggerEventWithOnComplete 
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete = lift . RD.newEventWithLazyTriggerWithOnComplete 


instance RD.HasWebView m => RD.HasWebView (LayoutM t m) where
  type WebViewPhantom (LayoutM t m) = RD.WebViewPhantom m
  askWebView = lift RD.askWebView



{-
instance (RD.MonadWidget t m,MonadIO (RD.PushM t))=>RD.MonadWidget t (LayoutM t m) where
  type WidgetHost (LayoutM t m) = RD.WidgetHost m
  type GuiAction  (LayoutM t m) = RD.GuiAction m
  askParent = liftL RD.askParent
  subWidget n w = layoutInside (RD.subWidget n) w
  subWidgetWithVoidActions n w = layoutInside (RD.subWidgetWithVoidActions n)  w
  liftWidgetHost = liftL . RD.liftWidgetHost
  schedulePostBuild = liftL . RD.schedulePostBuild
  addVoidAction = liftL . RD.addVoidAction

-- What happens if w changes the state??
  getRunWidget = do
    lc <- ask
    dynamicCssMap <- use lsDynamicCssMap
    staticCssMap <- use lsClassMap
    runWidget <- liftL RD.getRunWidget
    return $ \rootElement w -> runWidget rootElement $ runLayout staticCssMap dynamicCssMap lc w
-}

-- For debugging

printLayoutConfig::LayoutConfig t->IO ()
printLayoutConfig lc = do
  putStrLn $ "grid=" ++ (show $ _lcGridConfig lc)
  putStrLn $ "static Css=" ++ (show $ M.toList (_lcStaticCssMap lc))
  putStrLn $ "dynamicCss Keys=" ++ (show $ M.keys (_lcDynamicCssMap lc))

printLayoutTree::LayoutTree t->T.Text
printLayoutTree (LayoutNode i c) = "Node: classes=" <> toCssString (i ^. liNewClasses) <> "\n\t" <> (mconcat $ printLayoutTree <$> c)
