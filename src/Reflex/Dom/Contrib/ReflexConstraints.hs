{-# LANGUAGE CPP              #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Reflex.Dom.Contrib.ReflexConstraints where

import           Control.Monad.Ref (MonadRef, Ref)
import           GHC.IORef         (IORef)
import           GHCJS.DOM.Types   (MonadJSM)
import qualified Reflex.Dom        as RD
import           Reflex.Host.Class (MonadReflexCreateTrigger)


type MWExtrasC t m = (RD.DomBuilderSpace m ~ RD.GhcjsDomSpace, Ref m ~ IORef,Ref (RD.Performable m) ~ IORef
                     , RD.MonadSample t (RD.Performable m), MonadReflexCreateTrigger t m, RD.PerformEvent t m
                     , RD.TriggerEvent t m, MonadRef m, MonadRef (RD.Performable m))

#ifdef USE_JSADDLE
type MonadWidgetExtraC t m = (MWExtrasC t m, RD.HasDocument m, MonadJSM m, MonadJSM (RD.Performable m), RD.HasJSContext m, RD.HasJSContext (RD.Performable m))
#else
type MonadWidgetExtraC t m = (MWExtrasC t m, MonadJSM m, MonadJSM (RD.Performable m), RD.HasDocument m, RD.HasJSContext m, RD.HasJSContext (RD.Performable m))
#endif
