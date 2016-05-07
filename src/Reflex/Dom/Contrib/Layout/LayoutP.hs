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
import Control.Monad.State (StateT,runStateT,evalStateT,lift,get,put)
import Control.Monad.Trans (MonadIO,MonadTrans)
import Control.Monad.Ref (MonadRef,Ref)
import Control.Monad.Morph

import qualified Reflex.Dom.Contrib.Layout.Types as LT

data LayoutConstraint = OpensNode | InNode | ClosesNode
data LayoutNodeType = LDiv 
data LayoutInstruction = LayoutInstruction LayoutConstraint LayoutNodeType LT.CssClasses

newtype LPS = LPS [LayoutInstruction]

newtype LayoutP t m a = LayoutP {  unLayoutP::StateT LPS m a }

instance Functor m=>Functor (LayoutP t m)
instance Applicative m=>Applicative (LayoutP t m)
instance Monad m=>Monad (LayoutP t m)
instance MFunctor (LayoutP t)
instance MonadFix m=>MonadFix (LayoutP t m)
instance MonadException m => MonadException (LayoutP t m)
instance MonadIO m => MonadIO (LayoutP t m)
instance (MonadException m,MonadIO (LayoutP t m)) => MonadAsyncException (LayoutP t m)

instance MonadTrans (LayoutP t) where
  lift m = LayoutP $ lift m

instance MonadRef m=>MonadRef (LayoutP t m)

instance RC.MonadSample t m => RC.MonadSample t (LayoutP t m) where
  sample = LayoutP . lift . RC.sample

instance RC.MonadHold t m => RC.MonadHold t (LayoutP t m) where
  hold a0 = LayoutP . lift . RC.hold a0 

instance RD.HasWebView m => RD.HasWebView (LayoutP t m) where
  askWebView = LayoutP RD.askWebView

instance RD.HasDocument m => RD.HasDocument (LayoutP t m) where
  askDocument = LayoutP RD.askDocument

instance RHC.MonadReflexCreateTrigger t m => RHC.MonadReflexCreateTrigger t (LayoutP t m) where
  newEventWithTrigger = LayoutP . RHC.newEventWithTrigger
  newFanEventWithTrigger x = LayoutP $ RHC.newFanEventWithTrigger x

{-
instance (RD.MonadWidget t m,RD.MonadIORestore m, MonadIO (RD.PushM t)) => RD.MonadIORestore (LayoutP t m) where
  askRestore = LayoutP $ do
    parentRestore <- lift RD.askRestore
    curState <- get
    return $ RD.Restore $ \(LayoutP sma) -> RD.restore parentRestore $ execStateT sma curState
-}

instance (RD.HasPostGui t h m, Ref (LayoutP t m) ~ Ref h) => RD.HasPostGui t h (LayoutP t m) where
  askPostGui = LayoutP $ lift RD.askPostGui
  askRunWithActions = LayoutP $ lift RD.askRunWithActions

liftAction::Monad m=>(d->(b,LPS))->(m (a,LPS) -> m d)->LayoutP t m a->LayoutP t m b
liftAction f action lpa = LayoutP $ do
  s <- get
  (x,s') <- f <$> (lift . action $ runStateT (unLayoutP lpa) s)
  put s' 
  return x

instance (RD.MonadWidget t m, MonadIO (RD.PushM t), Ref (LayoutP t m) ~ Ref IO)=>RD.MonadWidget t (LayoutP t m) where
  type WidgetHost (LayoutP t m) = RD.WidgetHost m
  type GuiAction  (LayoutP t m) = RD.GuiAction m
  askParent = lift RD.askParent
  subWidget n = hoist (RD.subWidget n)
  subWidgetWithVoidActions n = liftAction  (\((a,s),ev)->((a,ev),s)) (RD.subWidgetWithVoidActions n)
  liftWidgetHost = lift . RD.liftWidgetHost
  schedulePostBuild = lift . RD.schedulePostBuild
  addVoidAction = lift . RD.addVoidAction

-- What happens if w changes the state??
  getRunWidget = LayoutP $ do
    curS <- get
    runWidget <- lift RD.getRunWidget
    return $ \rootElement w -> runWidget rootElement $ evalStateT (unLayoutP w) curS
