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
    LNodeConstraint(..)
  , layoutDiv
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

import qualified Reflex.Dom.Contrib.Layout.Types as LT

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
  newFanEventWithTrigger f = StackedMW . lift  $  RHC.newFanEventWithTrigger f
  
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
  layoutInstruction::LayoutInstruction -> l m a -> l m a
  liftF::(m a -> m b) -> l m a -> l m b
  lower::l m a->m a --loses information!
  insertLayout::Node -> l m Node

type MonadWidgetLC lmw mw t m = (RD.MonadWidget t mw, MonadLayout lmw mw, m ~ lmw mw, RD.MonadWidget t m)  

--class (RD.MonadWidget t mw, MonadLayout lmw mw, RD.MonadWidget t (lmw mw),m ~ lmw mw)=>MonadWidgetL t m

type StatefulMW s = StackedMW (StateT s)

instance (RD.MonadWidget t m, MonadIO (RD.PushM t),
          MonadFix (l m), MonadAsyncException (l m), MonadRef (l m),
          MonadTrans l, MFunctor l, Monad (l m), Ref (StackedMW l m) ~ Ref IO,
          MonadLayout (StackedMW l) m)=>RD.MonadWidget t (StackedMW l m) where
  type WidgetHost (StackedMW l m) = RD.WidgetHost m
  type GuiAction  (StackedMW l m) = RD.GuiAction m
  askParent = lift RD.askParent >>= insertLayout 
  subWidget n  = liftF (RD.subWidget n)
  subWidgetWithVoidActions n  = liftF (RD.subWidgetWithVoidActions n)
  liftWidgetHost = lift . RD.liftWidgetHost
  schedulePostBuild = lift . RD.schedulePostBuild
  addVoidAction = lift . RD.addVoidAction
  
  getRunWidget = do
    runWidget <- lift RD.getRunWidget
    return  (\n w -> runWidget n (lower w))


data LNodeConstraint = OpensLNode | InLNode | ClosesLNode deriving (Show)

data LNodeType = LDiv deriving (Eq,Show)
nodeTypeTag::LNodeType -> String
nodeTypeTag LDiv = "div"

data LNode = LNode LNodeType LT.CssClasses deriving (Show)

lNodeToFunction::RD.MonadWidget t m=> LNode -> m a -> m a
lNodeToFunction (LNode LDiv css) = RD.divClass (LT.toCssString css)

data OpenLNode = OpenLNode { olnType::LNodeType, olnCss::LT.CssClasses } deriving (Show)
closeLNode::OpenLNode -> LNode
closeLNode (OpenLNode nt css) = LNode nt css

openNAddCss::LT.CssClasses -> OpenLNode -> OpenLNode
openNAddCss css (OpenLNode nt css') = OpenLNode nt (css <> css')

data LayoutInstruction = LayoutInstruction LNodeConstraint OpenLNode deriving (Show)

type LayoutP = StatefulMW (Maybe OpenLNode)
getAndClear::Monad m => LayoutP m (Maybe OpenLNode)
getAndClear = do
  x <- get
  put Nothing
  return x

closeCurrentNode::(RD.MonadWidget t m,MonadIO (R.PushM t)) => Maybe OpenLNode -> LayoutP m a -> LayoutP m a
closeCurrentNode Nothing = id
closeCurrentNode (Just oln) = lNodeToFunction $ closeLNode oln

newNodeType::(RD.MonadWidget t m,MonadIO (R.PushM t))=>LayoutInstruction -> Maybe OpenLNode -> LayoutP m a -> LayoutP m a
newNodeType li@(LayoutInstruction lc oln) mOln lma = do
--  liftIO $ putStrLn $ "newNodeType " ++ show li ++ " " ++ show mOln
  let f1 = closeCurrentNode mOln
      f2 x = put (Just oln) >> f1 x
  case lc of
    ClosesLNode -> f1 $ (closeCurrentNode $ Just oln) lma 
    _           -> f2 lma

sameNodeType::(RD.MonadWidget t m,MonadIO (R.PushM t))=>LNodeConstraint->LT.CssClasses->OpenLNode->LayoutP m a ->LayoutP m a
sameNodeType lc css oln lma = do
--  liftIO $ putStrLn $ "sameNodeType " ++ show lc ++ " " ++ show css ++ " " ++ show oln  
  let oln' = openNAddCss css oln
  case lc of
    OpensLNode  -> put (Just $ OpenLNode (olnType oln) css) >> closeCurrentNode (Just oln) lma
    InLNode     -> put (Just oln') >> lma
    ClosesLNode -> closeCurrentNode (Just oln') lma 


doOneInstruction::(RD.MonadWidget t m,MonadIO (R.PushM t))=>LayoutInstruction -> LayoutP m a -> LayoutP m a
doOneInstruction li@(LayoutInstruction lc oln) lma = do
  mOln <- getAndClear
--  liftIO $ putStrLn $ "doOneInstruction " ++ show li ++ " (state was: " ++ show mOln ++ ")"
  case mOln of
    Nothing -> newNodeType li Nothing lma
    (Just on) -> if (olnType on == olnType oln)
                 then sameNodeType lc (olnCss oln) on lma
                 else newNodeType li (Just on) lma -- newNodeType also handles no previous node

-- for debugging purposes
doOneInstruction'::(RD.MonadWidget t m,MonadIO (R.PushM t))=>LayoutInstruction -> LayoutP m a -> LayoutP m a
doOneInstruction' (LayoutInstruction lc oln) = closeCurrentNode (Just oln)
          
liftAction::Monad m=>(m a->m b)->LayoutP m a -> LayoutP m b
liftAction f lpa = StackedMW $ StateT (\s -> flip (,) s <$> f (evalStateT (unS lpa) s))

instance (RD.MonadWidget t m,MonadIO (R.PushM t))=>MonadLayout LayoutP m where
  layoutInstruction = doOneInstruction
  liftF = liftAction
  lower lma = evalStateT (unS lma) Nothing 
  insertLayout = lpInsertLayout

lpInsertLayout::forall t m.(RD.MonadWidget t m,MonadIO (R.PushM t))=>Node -> LayoutP m Node
lpInsertLayout n = do
  let buildNode::LNode->m Node
      buildNode (LNode nType css) = do
        e <- RD.buildEmptyElement (nodeTypeTag nType) ("class" RD.=: LT.toCssString css)
        return $ toNode e
  mOln <- getAndClear
  case mOln of
    Nothing -> return n
    Just oln -> lift . buildNode $ closeLNode oln

-- so we can doUnoptimizedlayout for debugging
type IdentityMW = StackedMW IdentityT

instance RD.MonadWidget t m=>MonadLayout IdentityMW m where
  layoutInstruction (LayoutInstruction _ oln) = hoist (lNodeToFunction $ closeLNode oln)
  liftF f = StackedMW . lift . f . runIdentityT . unS 
  lower = runIdentityT . unS
  insertLayout = return

doUnoptimizedLayout::RD.MonadWidget t m =>IdentityMW m a -> m a
doUnoptimizedLayout = lower

doOptimizedLayout::(RD.MonadWidget t m,MonadIO (R.PushM t))=>LayoutP m a -> m a
doOptimizedLayout = lower 

layoutDiv::(MonadLayout l m, RD.MonadWidget t m)=>LNodeConstraint->LT.CssClasses->l m a -> l m a
layoutDiv lc css = layoutInstruction (LayoutInstruction lc (OpenLNode LDiv css))

layoutDivSimple::(MonadLayout l m, RD.MonadWidget t m)=>LNodeConstraint->String->l m a -> l m a
layoutDivSimple lc cls = layoutDiv lc (LT.CssClasses [LT.CssClass cls])


