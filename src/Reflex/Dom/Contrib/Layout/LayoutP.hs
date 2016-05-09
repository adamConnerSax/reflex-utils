{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Dom.Contrib.Layout.LayoutP where


import qualified Reflex as R
import qualified Reflex.Class as RC
import qualified Reflex.Host.Class as RHC
import qualified Reflex.Dom as RD

import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Exception (MonadException,MonadAsyncException)
import Control.Monad.State (StateT,runStateT,evalStateT,get,put)
import Control.Monad.Trans (MonadIO,MonadTrans,lift)
import Control.Monad.Ref (MonadRef,Ref)
import Control.Monad.Morph

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

                          
class MonadLayout t n where
  doLayout::t n a->n a
  beforeInsert::t n a->t n a


--newtype StatefulMW s m a = SMW {  unSMW::StateT s m a }
type StatefulMW s = StackedMW (StateT s) 


instance (RC.MonadSample t m, MonadTrans l, Monad (l m))   => RC.MonadSample t (l m) where
  sample = lift . RC.sample

instance (RC.MonadHold t m, MonadTrans l, Monad (l m)) => RC.MonadHold t (l m) where
  hold a0 = lift . RC.hold a0 


{-
instance (RD.MonadWidget t m,RD.MonadIORestore m, MonadIO (RD.PushM t)) => RD.MonadIORestore (LayoutP t m) where
  askRestore = LayoutP $ do
    parentRestore <- lift RD.askRestore
    curState <- get
    return $ RD.Restore $ \(Layoutinstance RD.HasWebView m => RD.HasWebView (LayoutP t m) where
  askWebView = LayoutP RD.askWebViewP sma) -> RD.restore parentRestore $ execStateT sma curState
-}

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


instance (RD.MonadWidget t m, MonadIO (RD.PushM t),
          Ref (StatefulMW s m) ~ Ref IO,
          MonadLayout (StatefulMW s) m)=>RD.MonadWidget t (StatefulMW s m) where
  type WidgetHost (StatefulMW s m) = RD.WidgetHost m
  type GuiAction  (StatefulMW s m) = RD.GuiAction m
  askParent = lift RD.askParent
  subWidget n = hoist (RD.subWidget n) . beforeInsert 
  subWidgetWithVoidActions n = layoutInside (RD.subWidgetWithVoidActions n) . beforeInsert
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
data LayoutConstraint = OpensNode | InNode | ClosesNode
data LayoutNodeType = LDiv 
data LayoutInstruction = LayoutInstruction LayoutConstraint LayoutNodeType LT.CssClasses

newtype LPS = LPS [LayoutInstruction]

type LayoutP m a = StatefulMW LPS m a



