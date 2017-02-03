{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Reflex.Dom.Contrib.StackedMonadWidget
  (
    StackedMW
  , MonadLayout
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
  
{-
instance (MonadTrans t, Monad (t m), RD.HasDocument m) => RD.HasDocument (StackedMW t m) where
  askDocument = StackedMW $ lift RD.askDocument
-}

instance (MonadTrans t, Monad (t m),
          RHC.MonadReflexCreateTrigger rt m) => RHC.MonadReflexCreateTrigger rt (StackedMW t m) where
  newEventWithTrigger  = StackedMW . lift . RHC.newEventWithTrigger  
  newFanEventWithTrigger f = StackedMW $ lift  $  RHC.newFanEventWithTrigger f

{-  
instance (MonadTrans t, RD.HasPostGui rt h m, MonadRef (t m),
          Ref (StackedMW t m) ~ Ref h) => RD.HasPostGui rt h (StackedMW t m) where
  askPostGui = StackedMW $ lift RD.askPostGui
  askRunWithActions = StackedMW $ lift RD.askRunWithActions
-}

instance (RC.MonadSample t m, MonadTrans l, Monad (l m))   => RC.MonadSample t (StackedMW l m) where
  sample = StackedMW . lift . R.sample

instance (RC.MonadHold t m, MonadTrans l, Monad (l m)) => RC.MonadHold t (StackedMW l m) where
  hold x ev = StackedMW . lift $ R.hold x ev

instance Monad m => MonadState s (StackedMW (StateT s) m) where
  get = StackedMW get
  put = StackedMW . put

class (MonadTrans l, Monad (l m))=>MonadLayout l m where
  layoutInstruction::LT.LayoutInstruction -> l m a -> l m a
  liftSW::(Node -> m a -> m b) -> Node -> l m a -> l m b
  lower::l m a->m a --loses information!
  lAskParent::l m Node

type MonadWidgetLC lmw mw t m = (RD.MonadWidget t mw, MonadLayout lmw mw, m ~ lmw mw, RD.MonadWidget t m)  

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





