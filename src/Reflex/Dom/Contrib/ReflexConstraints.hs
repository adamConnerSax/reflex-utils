{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Dom.Contrib.ReflexConstraints where

import qualified Reflex.Dom as RD
import Reflex.Host.Class (MonadReflexCreateTrigger)
import Control.Monad.Ref (MonadRef,Ref)
import GHC.IORef (IORef)
import GHCJS.DOM.Types (JSM,MonadJSM)


type MWExtrasC t m = (RD.DomBuilderSpace m ~ RD.GhcjsDomSpace, Ref m ~ IORef,Ref (RD.Performable m) ~ IORef
                     , RD.MonadSample t (RD.Performable m), MonadReflexCreateTrigger t m, RD.PerformEvent t m
                     , RD.TriggerEvent t m, MonadRef m,MonadRef (RD.Performable m)) 

#ifdef USE_JSADDLE
type MonadWidgetExtraC t m = (MWExtrasC t m, MonadJSM m, MonadJSM (RD.Performable m), RD.HasJSContext m, RD.HasJSContext (RD.Performable m))
#else
type MonadWidgetExtraC t m = MWExtras t m
#endif
