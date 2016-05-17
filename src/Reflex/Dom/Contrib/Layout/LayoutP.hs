{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

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
  ) where


import qualified Reflex as R
import qualified Reflex.Class as RC
import qualified Reflex.Host.Class as RHC
import qualified Reflex.Dom as RD
import GHCJS.DOM.Node (Node,toNode,appendChild)
import GHCJS.DOM.Element (Element)
import GHCJS.DOM.Document (createElement)
--import GHCJS.DOM.Types hiding (Event)

import Control.Monad (join)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Exception (MonadException,MonadAsyncException)
import Control.Monad.State (StateT(..),runStateT,execStateT,evalStateT,modify,mapStateT,MonadState(..),put)
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

--instance MonadTrans t=>MFunctor (StackedMW t)

{-
instance (MonadTrans t, Functor m)=>Functor (StackedMW t m)
instance (MonadTrans t, Applicative m)=>Applicative (StackedMW t m)
instance (MonadTrans t, Monad m)=>Monad (StackedMW t m)

instance (MonadTrans t, MonadFix m)=>MonadFix (StackedMW t m)
instance (MonadTrans t, MonadException m) => MonadException (StackedMW t m)
instance (MonadTrans t, MonadIO m) => MonadIO (StackedMW t m)

instance (MonadException (StackedMW t m), MonadIO (StackedMW t m)) => MonadAsyncException (StackedMW t m)
-}
instance (MonadTrans t, Monad (t m), MonadRef m)=>MonadRef (StackedMW t m) where
  type Ref (StackedMW t m) = Ref m
  newRef = StackedMW . lift . newRef
  readRef = lift . readRef
  

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
  

class MonadLayout l m where
  layoutInstruction::LayoutInstruction -> l m a -> l m a
  liftF::(m a -> m b) -> l m a -> l m b
  lower::l m a->m a --loses information!
  insertLayout::Node -> l m Node

{-
liftAction::Monad m=>(d->(b,s))->(m (a,s) -> m d)->StatefulMW s m a->StatefulMW s m b
liftAction f action lpa = StackedMW $ do
  s <- get
  (x,s') <- f <$> (lift . action $ runStateT (unSMW lpa) s)
  put s' 
  return x

layoutInside::(MonadTrans l, MonadLayout l m, Monad m)=>(m a -> m b)->l m a->l m b
layoutInside f lma = lift . f $ doLayout lma  
-}

type StatefulMW s = StackedMW (StateT s) 

instance (RD.MonadWidget t m, MonadIO (RD.PushM t),
          MonadFix (l m), MonadAsyncException (l m), MonadRef (l m),
          MonadTrans l, MFunctor l, Monad (l m), Ref (StackedMW l m) ~ Ref IO,
          MonadLayout (StackedMW l) m)=>RD.MonadWidget t (StackedMW l m) where
  type WidgetHost (StackedMW l m) = RD.WidgetHost m
  type GuiAction  (StackedMW l m) = RD.GuiAction m
  askParent = {- (liftIO $ putStrLn "askParent") >> -} lift RD.askParent >>= insertLayout
  subWidget n w = {- (liftIO $ putStrLn "subWidget") >> -} liftF (RD.subWidget n) w
  subWidgetWithVoidActions n w = {- (liftIO $ putStrLn "subWidgetWVA") >> -} liftF (RD.subWidgetWithVoidActions n) w 
  liftWidgetHost = lift . RD.liftWidgetHost
  schedulePostBuild = lift . RD.schedulePostBuild
  addVoidAction = lift . RD.addVoidAction
  
  getRunWidget = do
--    liftIO $ putStrLn "getRunWidget"
    runWidget <- lift RD.getRunWidget
    return  (\n w -> {- (liftIO $ putStrLn "runWidget") >> -}  runWidget n (lower w))



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
data LS = LS (Seq LNode) (Maybe OpenLNode)


closeCurrentNode::LS -> LS
closeCurrentNode (LS nodes mONode) =
  let nodes' = maybe nodes ((nodes |>) . closeLNode) mONode
  in LS nodes' Nothing

newNodeType::LayoutInstruction -> LS -> LS
newNodeType (LayoutInstruction lc oln) ls =
  let (LS nodes' _) = closeCurrentNode ls
  in case lc of
    ClosesLNode -> LS (nodes' |> closeLNode oln) Nothing
    _           -> LS nodes' (Just oln)

sameNodeType::LNodeConstraint->LT.CssClasses->Seq LNode->OpenLNode->LS
sameNodeType lc css nodes oln =
  case lc of
    OpensLNode  -> LS (nodes |> closeLNode oln) (Just $ OpenLNode (olnType oln) css)
    InLNode     -> LS nodes (Just $ openNAddCss css oln)
    ClosesLNode -> LS (nodes |> closeLNode (openNAddCss css oln)) Nothing


doOneInstruction::LayoutInstruction -> LS -> LS
doOneInstruction li@(LayoutInstruction lc oln) ls@(LS nodes mONode) =
  let isNew = maybe True (\on -> olnType on == olnType oln) mONode
  in if isNew
     then newNodeType li ls
     else sameNodeType lc (olnCss oln) nodes (fromJust mONode) -- safe because mONode == Nothing => isNew == True 

doOneInstruction'::LayoutInstruction -> LS -> LS
doOneInstruction' (LayoutInstruction lc oln) (LS nodes _) = LS (nodes |> closeLNode oln) Nothing
          
type LayoutP = StatefulMW (Seq LayoutInstruction)

doLayoutP::forall t m a.(RD.MonadWidget t m)=>LayoutP m a -> m a
doLayoutP lma = 
  liftIO (putStrLn "doLayoutP") >> runStateT (unS lma) empty >>= f where
  f (a,instrs) = do
    let optimize = foldl' (flip doOneInstruction) (LS empty Nothing)
        getNodes (LS nodes _) = nodes
        lfs:: Seq (m a -> m a)
        lfs = fmap lNodeToFunction . getNodes $ optimize instrs
        layoutF:: (m a -> m a) 
        layoutF = foldl' (.) id lfs -- (m a -> m a)
    layoutF (return a)
--  join $ fmap ($ ma) mLayoutF -- m a

liftAction::Monad m=>(m a->m b)->LayoutP m a -> LayoutP m b
liftAction f lpa = StackedMW $ StateT (\s -> flip (,) s <$> f (evalStateT (unS lpa) s))

instance (RD.MonadWidget t m,MonadIO (R.PushM t))=>MonadLayout LayoutP m where
--  layoutInstruction li w = modify (\nodes-> nodes |> li) >>  w
  layoutInstruction li w = get >>= put . (|> li) >> w
  liftF = liftAction
  lower lma = evalStateT (unS lma) empty 
  insertLayout = insertLayout'

addLNode::RD.MonadWidget t m=> Node -> LNode -> m Node
addLNode n (LNode nt css) = do
  doc <- RD.askDocument
  Just e <- liftIO $ createElement doc (Just "div")
  RD.addAttributes ("class" RD.=: LT.toCssString css) e
  mNode <- appendChild n $ Just e -- do I need to subWidget these to each other?
  _ <- case mNode of
    Nothing -> liftIO $ putStrLn "Nothing in addLNode"
    Just n' -> RD.subWidget n $ return () -- SCREM
  return $ toNode e
  
insertLayout'::(RD.MonadWidget t m, MonadIO (R.PushM t))=>Node -> LayoutP m Node
insertLayout' n = do
  instrs <- get
  let optimize = foldl' (flip doOneInstruction) (LS empty Nothing)
      getNodes (LS nodes _) = nodes
      lNodes = getNodes $ optimize instrs
  lift $ foldlM addLNode n lNodes

-- So we can use Layout functions without doing the optimization
--type MW = RD.Widget R.Spider (RD.Gui R.Spider (RD.WithWebView R.SpiderHost) (RHC.HostFrame R.Spider))
--newtype MWT a = MWT { unMW::RD.Widget R.Spider (RD.Gui R.Spider (RD.WithWebView R.SpiderHost) (RHC.HostFrame R.Spider)) a }

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

{-}
addContent::(MonadLayout l m, RD.MonadWidget t m)=>l m a -> l m a
addContent lma = do
-}

--flexItem::(MonadLayout l m, RD.MonadWidget t m)=>l m a -> l m a
--flexItem = layoutDivSimple InLNode "flex-item"

