{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Dom.Contrib.Layout.LayoutP where


import qualified Reflex as R
import qualified Reflex.Class as RC
import qualified Reflex.Dom as RD

import Control.Monad.State (StateT,runStateT,lift,get,put)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Ref (MonadRef)

import qualified Reflex.Dom.Contrib.Layout.Types as LT

data LayoutConstraint = OpensNode | InNode | ClosesNode
data LayoutNodeType = LDiv 
data LayoutInstruction = LayoutInstruction LayoutConstraint LayoutNodeType LT.CssClasses

newtype LPS = LPS [LayoutInstruction]

newtype LayoutP t m a = LayoutP (StateT LPS m a)

instance Functor m=>Functor (LayoutP t m)
instance Applicative m=>Applicative (LayoutP t m)
instance Monad m=>Monad (LayoutP t m)
instance MonadRef m=>MonadRef (LayoutP t m)

instance RC.MonadSample t m => RC.MonadSample t (LayoutP t m) where
  sample = LayoutP . lift . RC.sample

instance RC.MonadHold t m => RC.MonadHold t (LayoutP t m) where
  hold a0 = LayoutP . lift . RC.hold a0 

instance (RD.MonadWidget t m,RD.MonadIORestore m, MonadIO (RD.PushM t)) => RD.MonadIORestore (LayoutP t m) where
  askRestore = LayoutP $ do
    parentRestore <- lift RD.askRestore
    curState <- get
    return $ RD.Restore $ \(LayoutP sma) -> RD.restore parentRestore $ fst <$> runStateT sma curState


instance RD.HasPostGui t h m => RD.HasPostGui t h (LayoutP t m) where
  askPostGui = LayoutP $ lift RD.askPostGui
  askRunWithActions = LayoutP $ lift RD.askRunWithActions

{-
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

-}
