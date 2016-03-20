{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Dom.Contrib.Layout.Core
       (
         emptyClassMap
       , emptyDynamicCssMap
       , toStaticAttributes
       , addCssUpdate
       , LayoutClassDynamic(..)
       , LayoutClassDynamicMap
       , mergeCssUpdates
       , emptyCss
       , runStyledLayout
       , runLayout
       , runLayoutMain
       , getLayoutPropertyDef
       , addNewClassesToTreeTop
       , addNewLayoutNode
       ) where

import qualified Reflex as R
import qualified Reflex.Class as RC
import qualified Reflex.Dom as RD
import qualified GHCJS.DOM.Element as E


import Control.Monad.IO.Class (liftIO,MonadIO)
import Data.String (words,unwords)
import Control.Monad (forM_,forM,mapM_,sequence)
import Control.Lens (set,makeLenses,makeClassy,(.~),(^.),(<>~),(<>=),(%=),use,(.=),view)
import Control.Monad.State (StateT,get,put,runStateT,MonadState,lift)
import Control.Monad.Reader (ReaderT,ask,MonadReader,runReaderT)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (MonadTrans)
import qualified Data.Map as M
import Data.Maybe (fromJust,catMaybes)
import Data.Monoid ((<>))
import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import Data.Traversable (sequenceA)


import Reflex.Dom.Contrib.Layout.Types

noProperties::LayoutPropertyMap
noProperties = M.empty

data LayoutNodeCss = LayoutNodeCss { _lncStatic::CssClasses, _lncDynamic::CssClasses }
instance IsCssClass LayoutNodeCss where
  toCssString (LayoutNodeCss s d) = toCssString s ++ " " ++ toCssString d

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
addNode' tree = do
  lsTree %= flip addNode tree

addStaticClasses::Monad m=>LayoutDescription t->LayoutTree t->LayoutM t m (LayoutTree t)
addStaticClasses desc lTree = do
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
  if (null classKeys) || (null getLMaybeDyn) then return Nothing else Just <$> R.mconcatDyn getLMaybeDyn

addDynamicCss::(R.Reflex t, R.MonadHold t m, MonadFix m)=>LayoutDescription t->LayoutTree t->LayoutM t m (LayoutTree t)
addDynamicCss desc lTree = do
  dynamicCssMap <- use lsDynamicCssMap
  mDyn <- dynamicCss desc dynamicCssMap
  return (lnInfo.liDynamicCss .~ mDyn $ lTree)
  
setElement::E.Element->LayoutTree t->LayoutTree t
setElement elt lTree = (lnInfo.liElt .~ Just elt) $ lTree

newLayoutS::(R.Reflex t,Monad m)=>LayoutDescription t->LayoutM t m (LayoutS t)
newLayoutS desc = LayoutS (newTree desc) <$> use lsClassMap <*> use lsDynamicCssMap 

addNewLayoutNode::(RD.MonadWidget t m,R.MonadHold t m,MonadFix m)=>LayoutDescription t->LayoutM t m a->LayoutM t m a
addNewLayoutNode desc newChildren = do
  emptyLS <- newLayoutS desc
  (RD.El e _,(x,childLS)) <- lift $ RD.el' "div" $ runStateT newChildren emptyLS
  let taggedChild = setElement e (childLS ^. lsTree)
  addStaticClasses desc taggedChild >>= addDynamicCss desc >>= addNode'
  return x

cssToAttr::IsCssClass c=>c->RD.AttributeMap
cssToAttr cssClass = ("class" RD.=: (toCssString cssClass)) 

-- add attributes to element.  If no events are hooked up, do it statically
addClassesStatic::(RD.MonadWidget t m, E.IsElement e)=>CssClasses->e->m ()
addClassesStatic css elt = RD.addAttributes (cssToAttr css) elt

updateCss::LayoutNodeCss->CssUpdate->LayoutNodeCss
updateCss (LayoutNodeCss s _) (UpdateDynamic d) = LayoutNodeCss s d
updateCss (LayoutNodeCss s d) (AddToDynamic d') = LayoutNodeCss s (d <> d')

updateCssNE::NE.NonEmpty CssUpdate->LayoutNodeCss->LayoutNodeCss
updateCssNE cssUpdates nodeCss = foldl' updateCss nodeCss (NE.toList cssUpdates)

addClassesDynamic::(RD.MonadWidget t m, E.IsElement e)=>CssClasses->e->R.Dynamic t CssClasses->m ()
addClassesDynamic staticCss elt dynamicCssDyn = do
  cssDyn <- RD.mapDyn (\x->(LayoutNodeCss staticCss x)) dynamicCssDyn 
  attrsDyn <- RD.mapDyn cssToAttr cssDyn
  RD.addAttributes attrsDyn elt

addClasses::(RD.MonadWidget t m, E.IsElement e)=>CssClasses->Maybe (R.Dynamic t CssClasses)->e->m ()
addClasses css mCssDyn elt = maybe (addClassesStatic css elt) (addClassesDynamic css elt) mCssDyn


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
      elt = fromJust $ (lt' ^. lnInfo.liElt)
      classesForElt = (lt' ^. lnInfo.liNewClasses) <> classesForAll
      children = (lt' ^. lnChildren)
      mDynamicCss = (lt' ^. lnInfo.liDynamicCss)
  addClasses classesForElt mDynamicCss elt
  mapM_ (runLayoutTree lc classesForAll) children

runStyledLayout::(RD.MonadWidget t m, MonadIO (RD.PushM t))=>CssClasses->LayoutClassMap->LayoutClassDynamicMap t->LayoutConfig t->LayoutM t m a->m a
runStyledLayout classesForAll staticCssMap dynamicCssMap conf layout = do
  combinedDynamicCssMap <- unionM mergeLayoutClassDynamic (conf ^. lcDynamicCssMap) dynamicCssMap --dynamic classes are 
  let combinedStaticCssMap = M.union staticCssMap (conf ^. lcStaticCssMap) --static classes are overwritten by later classes. ??
      emptyD = LayoutDescription (const id) noProperties []
      emptyS = LayoutS (newTree emptyD) combinedStaticCssMap combinedDynamicCssMap
  (x,layoutS) <- runReaderT (runStateT layout emptyS) conf
  mapM_ (runLayoutTree conf classesForAll) (layoutS ^. lsTree.lnChildren) -- there is no elt on top
  return x

runLayout::(RD.MonadWidget t m, MonadIO (RD.PushM t))=>LayoutClassMap->LayoutClassDynamicMap t -> LayoutConfig t->LayoutM t m a->m a
runLayout = runStyledLayout emptyCss 

runLayoutMain::(RD.MonadWidget t m, MonadIO (RD.PushM t))=>LayoutConfig t->LayoutM t m a->m a
runLayoutMain = runLayout emptyClassMap emptyDynamicCssMap

-- Class Instances
liftL::Monad m=>m a->LayoutM t m a 
liftL = lift . lift


instance RC.MonadSample t m => RC.MonadSample t (LayoutM t m) where
  sample = liftL . RC.sample

instance RC.MonadHold t m => RC.MonadHold t (LayoutM t m) where
  hold a0 = liftL . RC.hold a0 


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


-- which to use??  Both work on demo.  So far.  Need to add widgetHold or dyn...
layoutInside::(MonadIO (RD.PushM t),RD.MonadWidget t m)=>(m a -> m b)->LayoutM t m a->LayoutM t m b
layoutInside f w = do
  lc <- ask
  dynamicCssMap <- use lsDynamicCssMap
  staticCssMap <- use lsClassMap
  liftL $ f $ runLayout staticCssMap dynamicCssMap lc w

noLayoutInside::(RD.MonadWidget t m ,s~LayoutS t)=>(d->(b,s))->(m (a,s) -> m d)->LayoutM t m a->LayoutM t m b
noLayoutInside f action w = do
  s <- get
  lc <- ask
  (x,s') <- f <$> (liftL $ action $ runReaderT (runStateT w s) lc)
  put s'
  return x

instance (RD.MonadWidget t m,MonadIO (RD.PushM t))=>RD.MonadWidget t (LayoutM t m) where
  type WidgetHost (LayoutM t m) = RD.WidgetHost m
  type GuiAction  (LayoutM t m) = RD.GuiAction m
  askParent = liftL RD.askParent
--  subWidget n w = noLayoutInside id (RD.subWidget n) w
--  subWidgetWithVoidActions n w = noLayoutInside (\((a,s),ev)->((a,ev),s)) (RD.subWidgetWithVoidActions n)  w
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


-- For debugging

printLayoutConfig::LayoutConfig t->IO ()
printLayoutConfig lc = do
  putStrLn $ "grid=" ++ (show $ _lcGridConfig lc)
  putStrLn $ "static Css=" ++ (show $ M.toList (_lcStaticCssMap lc))
  putStrLn $ "dynamicCss Keys=" ++ (show $ M.keys (_lcDynamicCssMap lc))

printLayoutTree::LayoutTree t->String
printLayoutTree (LayoutNode i c) = "Node: classes=" ++ toCssString (i ^. liNewClasses) ++ "\n\t" ++ (mconcat $ printLayoutTree <$> c)
