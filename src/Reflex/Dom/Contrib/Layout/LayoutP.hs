{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Dom.Contrib.Layout.LayoutP
  (
    LNodeConstraint(..)
  , layoutDiv
  , layoutDivSimple
  , doOptimizedLayout
  , doUnoptimizedLayout
--  , MonadLayout(layoutInstruction)
  ) where


import qualified Reflex as R
import qualified Reflex.Class as RC
import qualified Reflex.Host.Class as RHC
import qualified Reflex.Dom as RD


import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Exception (MonadException,MonadAsyncException)
import Control.Monad.State (StateT,runStateT,modify,MonadState(..))
import Control.Monad.Trans (MonadIO,MonadTrans,lift)
import Control.Monad.Trans.Identity (IdentityT,runIdentityT)
import Control.Monad.Ref (MonadRef,Ref)
import Control.Monad.Morph (MFunctor,hoist)
import Data.Sequence (Seq,(|>),empty)
import Data.Monoid ((<>))
import Data.Maybe (fromJust)
import Data.Foldable (foldl')

import qualified Reflex.Dom.Contrib.Layout.Types as LT

newtype StackedMW (t :: (* -> *) -> * -> *) m a = StackedMW { unS::t m a }

-- GeneralizedNewtypeDeriving, FTW!
instance Functor m=>Functor (StackedMW t m)
instance Applicative m=>Applicative (StackedMW t m)
instance Monad m=>Monad (StackedMW t m)
instance MFunctor (StackedMW t)
instance MonadFix m=>MonadFix (StackedMW t m)
instance MonadException m => MonadException (StackedMW t m)
instance MonadIO m => MonadIO (StackedMW t m)
instance (MonadException m, MonadIO (StackedMW t m)) => MonadAsyncException (StackedMW t m)
instance MonadTrans (StackedMW t) 
instance MonadRef m=>MonadRef (StackedMW t m)
instance RD.HasWebView m => RD.HasWebView (StackedMW t m) 
instance RD.HasDocument m => RD.HasDocument (StackedMW t m) 
instance RHC.MonadReflexCreateTrigger rt m => RHC.MonadReflexCreateTrigger rt (StackedMW t m) 
instance (RD.HasPostGui rt h m, Ref (StackedMW t m) ~ Ref h) => RD.HasPostGui rt h (StackedMW t m)

instance Monad m => MonadState s (StackedMW (StateT s) m)

instance (RC.MonadSample t m, MonadTrans l, Monad (l m))   => RC.MonadSample t (l m) where
  sample = lift . RC.sample

instance (RC.MonadHold t m, MonadTrans l, Monad (l m)) => RC.MonadHold t (l m) where
  hold a0 = lift . RC.hold a0 

class MonadLayout l m where
  layoutInstruction::LayoutInstruction -> l m a -> l m a
  doLayout::l m a->m a
  beforeInsert::l m a->l m a


{-
liftAction::Monad m=>(d->(b,s))->(m (a,s) -> m d)->StatefulMW s m a->StatefulMW s m b
liftAction f action lpa = StackedMW $ do
  s <- get
  (x,s') <- f <$> (lift . action $ runStateT (unSMW lpa) s)
  put s' 
  return x
-}

layoutInside::(MonadTrans l, MonadLayout l m, Monad m)=>(m a -> m b)->l m a->l m b
layoutInside f ma = lift . f $ doLayout ma  

type StatefulMW s = StackedMW (StateT s) 

instance (RD.MonadWidget t m, MonadIO (RD.PushM t),
          Ref (StatefulMW s m) ~ Ref IO,
          MonadLayout (StatefulMW s) m)=>RD.MonadWidget t (StatefulMW s m) where
  type WidgetHost (StatefulMW s m) = RD.WidgetHost m
  type GuiAction  (StatefulMW s m) = RD.GuiAction m
  askParent = lift RD.askParent
  subWidget n = beforeInsert . hoist (RD.subWidget n) 
  subWidgetWithVoidActions n = beforeInsert . layoutInside (RD.subWidgetWithVoidActions n) 
  --liftAction  (\((a,s),ev)->((a,ev),s)) (RD.subWidgetWithVoidActions n)
  liftWidgetHost = lift . RD.liftWidgetHost
  schedulePostBuild = lift . RD.schedulePostBuild
  addVoidAction = lift . RD.addVoidAction

-- What happens if w changes the state??
  getRunWidget = do
    runWidget <- lift RD.getRunWidget
    return  (\n w -> runWidget n (doLayout w))


{-
instance (MonadTrans l, MFunctor l, Monad (l m), RD.MonadWidget t m,
          MonadIO (RD.PushM t), Ref (l m) ~ Ref IO,
          MonadLayout l m)=>RD.MonadWidget t (l m) where
  type WidgetHost (l m) = RD.WidgetHost m
  type GuiAction  (l m) = RD.GuiAction m
  askParent = lift RD.askParent
  subWidget n = hoist (RD.subWidget n)
  subWidgetWithVoidActions n = layoutInside  (RD.subWidgetWithVoidActions n)
  liftWidgetHost = lift . RD.liftWidgetHost
  schedulePostBuild = lift . RD.schedulePostBuild
  addVoidAction = lift . RD.addVoidAction

-- What happens if w changes the state??
  getRunWidget = do
    runWidget <- lift RD.getRunWidget
    return $ layoutInside . runWidget

-}
data LNodeConstraint = OpensLNode | InLNode | ClosesLNode deriving (Show)
data LNodeType = LDiv deriving (Eq,Show)
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
  
type LayoutP = StatefulMW (Seq LayoutInstruction)

doLayoutP::RD.MonadWidget t m=>LayoutP m a -> m a
doLayoutP lma = do
  let optimize = foldl' (flip doOneInstruction) (LS empty Nothing)
      getNodes (LS nodes _) = nodes
      mPair = runStateT (unS lma) empty
      mLNodes = (fmap lNodeToFunction . getNodes . optimize . snd) <$> mPair
      ma = fst <$> mPair
  layoutF <- foldl' (.) id <$> mLNodes
  layoutF ma
--  join $ (foldl' (.) id <$> mLNodes) <*> (return $ fst <$> mPair)
  
instance RD.MonadWidget t m=>MonadLayout LayoutP m where
  layoutInstruction li w = modify (|> li) >> w
  doLayout = doLayoutP
  beforeInsert = lift . doLayoutP

-- So we can use Layout functions without doing the optimization
--type MW = RD.Widget R.Spider (RD.Gui R.Spider (RD.WithWebView R.SpiderHost) (RHC.HostFrame R.Spider))
--newtype MWT a = MWT { unMW::RD.Widget R.Spider (RD.Gui R.Spider (RD.WithWebView R.SpiderHost) (RHC.HostFrame R.Spider)) a }

type IdentityMW = StackedMW IdentityT

instance RD.MonadWidget t m=>MonadLayout IdentityMW m where
  layoutInstruction (LayoutInstruction _ oln) = hoist (lNodeToFunction $ closeLNode oln)
  doLayout = runIdentityT . unS
  beforeInsert = id

doUnoptimizedLayout::RD.MonadWidget t m=>IdentityMW m a -> m a
doUnoptimizedLayout = doLayout

doOptimizedLayout::RD.MonadWidget t m=>LayoutP m a -> m a
doOptimizedLayout = doLayout

layoutDiv::(MonadLayout l m, RD.MonadWidget t m)=>LNodeConstraint->LT.CssClasses->l m a -> l m a
layoutDiv lc css = layoutInstruction (LayoutInstruction lc (OpenLNode LDiv css))

layoutDivSimple::(MonadLayout l m, RD.MonadWidget t m)=>LNodeConstraint->String->l m a -> l m a
layoutDivSimple lc cls = layoutDiv lc (LT.CssClasses [LT.CssClass cls])

--flexItem::(MonadLayout l m, RD.MonadWidget t m)=>l m a -> l m a
--flexItem = layoutDivSimple InLNode "flex-item"

