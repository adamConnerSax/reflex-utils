{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Reflex.Dom.Contrib.Layout.LayoutP
  (
--    LNodeConstraint(..)
    layoutDiv
  , layoutDivSimple
  , doOptimizedLayout
  , doUnoptimizedLayout
  , StackedMW
--  , IdentityMW
--  , LayoutP
  , MonadLayout
--  , MonadLayoutC
  , MonadWidgetLC
  ) where

import qualified Reflex.Dom.Contrib.Layout.Types as LT

import qualified Reflex as R
import qualified Reflex.Class as RC
import qualified Reflex.Host.Class as RHC
import qualified Reflex.Dom as RD
import GHCJS.DOM.Node (Node,toNode,appendChild)
import GHCJS.DOM.Element (Element)
import GHCJS.DOM.Document (createElement)

import Control.Monad (join)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Exception (MonadException,MonadAsyncException)
import Control.Monad.State (StateT(..),runStateT,execStateT,evalStateT,modify,mapStateT,MonadState(..),put,withStateT)
import Control.Monad.Trans (MonadIO,MonadTrans,lift)
import Control.Monad.Trans.Identity (IdentityT,runIdentityT)
import Control.Monad.Ref (MonadRef(..),Ref)
import Control.Monad.Morph (MFunctor,hoist)
import Control.Monad.IO.Class (liftIO)
import Data.Sequence (Seq,(|>),empty)
import Data.Monoid ((<>))
import Data.Maybe (fromJust)
import Data.Foldable (foldl',foldlM)


newtype StackedMW (t :: (* -> *) -> * -> *) m a = StackedMW { unS::t m a }
  deriving (Functor,Applicative,Monad,
            MonadFix,MonadIO,
            MonadException,MonadAsyncException,
            MonadTrans,MFunctor)

instance (MonadTrans t, Monad (t m), MonadRef m)=>MonadRef (StackedMW t m) where
  type Ref (StackedMW t m) = Ref m
  newRef = StackedMW . lift . newRef
  readRef = lift . readRef
  writeRef a  = lift . writeRef a 

instance (MonadTrans t, Monad (t m), RD.HasWebView m) => RD.HasWebView (StackedMW t m) where
  askWebView = StackedMW $ lift RD.askWebView
  
instance (MonadTrans t, Monad (t m), RD.HasDocument m) => RD.HasDocument (StackedMW t m) where
  askDocument = StackedMW $ lift RD.askDocument
  
instance (MonadTrans t, Monad (t m),
          RHC.MonadReflexCreateTrigger rt m) => RHC.MonadReflexCreateTrigger rt (StackedMW t m) where
  newEventWithTrigger  = StackedMW . lift . RHC.newEventWithTrigger  
  newFanEventWithTrigger f = StackedMW $ lift  $  RHC.newFanEventWithTrigger f
  
instance (MonadTrans t, RD.HasPostGui rt h m, MonadRef (t m),
          Ref (StackedMW t m) ~ Ref h) => RD.HasPostGui rt h (StackedMW t m) where
  askPostGui = StackedMW $ lift RD.askPostGui
  askRunWithActions = StackedMW $ lift RD.askRunWithActions

instance (RC.MonadSample t m, MonadTrans l, Monad (l m))   => RC.MonadSample t (StackedMW l m) where
  sample = StackedMW . lift . R.sample

instance (RC.MonadHold t m, MonadTrans l, Monad (l m)) => RC.MonadHold t (StackedMW l m) where
  hold x ev = StackedMW . lift $ R.hold x ev

instance Monad m => MonadState s (StackedMW (StateT s) m) where
  get = StackedMW get
  put = StackedMW . put
  
class (MonadTrans l, Monad (l m))=>MonadLayout l m where
  layoutInstruction::LT.LayoutInstruction -> l m a -> l m a
--  lSubWidget::Node->l m a -> l m a
  liftSW::(Node -> m a -> m b) -> Node -> l m a -> l m b
  lower::l m a->m a --loses information!
  lAskParent::l m Node

type MonadWidgetLC lmw mw t m = (RD.MonadWidget t mw, MonadLayout lmw mw, m ~ lmw mw, RD.MonadWidget t m)  

--class (RD.MonadWidget t mw, MonadLayout lmw mw, RD.MonadWidget t (lmw mw),m ~ lmw mw)=>MonadWidgetL t m

type StatefulMW s = StackedMW (StateT s)

instance (RD.MonadWidget t m, MonadIO (RD.PushM t),
          MonadFix (l m), MonadAsyncException (l m), MonadRef (l m),
          MonadTrans l, MFunctor l, Monad (l m), Ref (StackedMW l m) ~ Ref IO,
          MonadLayout (StackedMW l) m)=>RD.MonadWidget t (StackedMW l m) where
  type WidgetHost (StackedMW l m) = RD.WidgetHost m
  type GuiAction  (StackedMW l m) = RD.GuiAction m
  askParent = lAskParent 
  subWidget n  = liftSW RD.subWidget n
  subWidgetWithVoidActions n  = liftSW RD.subWidgetWithVoidActions n
  liftWidgetHost = lift . RD.liftWidgetHost
  schedulePostBuild = lift . RD.schedulePostBuild
  addVoidAction = lift . RD.addVoidAction
  
  getRunWidget = do
    runWidget <- lift RD.getRunWidget
    return  (\n w -> runWidget n (lower w))



type LayoutP = StatefulMW (Maybe LT.OpenLNode)
getAndClear::Monad m => LayoutP m (Maybe LT.OpenLNode)
getAndClear = do
  x <- get
  put Nothing
  return x

closeCurrentNode::(RD.MonadWidget t m,MonadIO (R.PushM t)) => Maybe LT.OpenLNode -> LayoutP m a -> LayoutP m a
closeCurrentNode Nothing = id
closeCurrentNode (Just oln) = LT.lNodeToFunction $ LT.closeLNode oln

newNodeType::(RD.MonadWidget t m,MonadIO (R.PushM t))=>LT.LayoutInstruction -> Maybe LT.OpenLNode -> LayoutP m a -> LayoutP m a
newNodeType (LT.LayoutInstruction lc oln) mOln lma = do
--  liftIO $ putStrLn $ "newNodeType " ++ show li ++ " " ++ show mOln
  let f1 = closeCurrentNode mOln
      f2 x = put (Just oln) >> f1 x
  case lc of
    LT.ClosesLNode -> f1 $ (closeCurrentNode $ Just oln) lma 
    _           -> f2 lma

sameNodeType::(RD.MonadWidget t m,MonadIO (R.PushM t))=>LT.LNodeConstraint->LT.CssClasses->LT.OpenLNode->LayoutP m a ->LayoutP m a
sameNodeType lc css oln lma = do
--  liftIO $ putStrLn $ "sameNodeType " ++ show lc ++ " " ++ show css ++ " " ++ show oln  
  let oln' = LT.openNAddCss css oln
  case lc of
    LT.OpensLNode  -> put (Just $ LT.OpenLNode (LT.olnType oln) css) >> closeCurrentNode (Just oln) lma
    LT.InLNode     -> put (Just oln') >> lma
    LT.ClosesLNode -> closeCurrentNode (Just oln') lma 


doOneInstruction::(RD.MonadWidget t m,MonadIO (R.PushM t))=>LT.LayoutInstruction -> LayoutP m a -> LayoutP m a
doOneInstruction li@(LT.LayoutInstruction lc oln) lma = do
  mOln <- getAndClear
--  liftIO $ putStrLn $ "doOneInstruction " ++ show li ++ " (state was: " ++ show mOln ++ ")"
  case mOln of
    Nothing -> newNodeType li Nothing lma
    (Just on) -> if LT.olnType on == LT.olnType oln
                 then sameNodeType lc (LT.olnCss oln) on lma
                 else newNodeType li (Just on) lma -- newNodeType also handles no previous node

-- for debugging purposes
doOneInstruction'::(RD.MonadWidget t m,MonadIO (R.PushM t))=>LT.LayoutInstruction -> LayoutP m a -> LayoutP m a
doOneInstruction' (LT.LayoutInstruction lc oln) = closeCurrentNode (Just oln)
          
liftAction::Monad m=>(m a->m b)->LayoutP m a -> LayoutP m b
liftAction f lpa = StackedMW $ StateT (\s -> flip (,) s <$> f (evalStateT (unS lpa) s))

instance (RD.MonadWidget t m,MonadIO (R.PushM t))=>MonadLayout LayoutP m where
  layoutInstruction = doOneInstruction
  liftSW swF n lma = liftIO (putStrLn "sw: ") >>  get >>= liftIO . putStr . show >> fixNode n >>= \n' -> liftAction (swF n') lma
  lower lma = evalStateT (unS lma) Nothing 
  lAskParent = liftIO (putStrLn "ap: ") >> get >>= liftIO . putStr . show >> lift RD.askParent >>= fixNode

fixNode::(RD.MonadWidget t m, MonadIO (R.PushM t))=>Node->LayoutP m Node
fixNode n = do
  mOln <- getAndClear
  lift $ case mOln of
           Nothing  -> return n
           Just oln -> addLNode n $ LT.closeLNode oln
 
addLNode::(RD.MonadWidget t m, MonadIO (R.PushM t))=>Node->LT.LNode->m Node
addLNode p (LT.LNode nType css) = do
  doc <- RD.askDocument
  Just e <- liftIO $ createElement doc (Just $ LT.nodeTypeTag nType)
  RD.addAttributes ("class" RD.=: LT.toCssString css) e
  _ <- appendChild p $ Just e
  return $ toNode e 

-- so we can doUnoptimizedlayout for debugging
type IdentityMW = StackedMW IdentityT

instance RD.MonadWidget t m=>MonadLayout IdentityMW m where
  layoutInstruction (LT.LayoutInstruction _ oln) = hoist (LT.lNodeToFunction $ LT.closeLNode oln)
  liftSW swF n = StackedMW . lift . (swF n) . runIdentityT . unS 
  lower = runIdentityT . unS
  lAskParent = lift RD.askParent

doUnoptimizedLayout::RD.MonadWidget t m =>IdentityMW m a -> m a
doUnoptimizedLayout = lower

doOptimizedLayout::(RD.MonadWidget t m,MonadIO (R.PushM t))=>LayoutP m a -> m a
doOptimizedLayout = lower 

layoutDiv::(MonadLayout l m, RD.MonadWidget t m)=>LT.LNodeConstraint->LT.CssClasses->l m a -> l m a
layoutDiv lc css = layoutInstruction (LT.LayoutInstruction lc (LT.OpenLNode LT.LDiv css))

layoutDivSimple::(MonadLayout l m, RD.MonadWidget t m)=>LT.LNodeConstraint->String->l m a -> l m a
layoutDivSimple lc cls = layoutDiv lc (LT.CssClasses [LT.CssClass cls])


