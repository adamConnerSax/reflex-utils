{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Dom.Contrib.Layout.LayoutP where


import qualified Reflex as R
import qualified Reflex.Class as RC
import qualified Reflex.Dom as RD

import Control.Monad.State (StateT)

import qualified Reflex.Dom.Contrib.Layout.Types as LT

data LayoutConstraint = OpensNode | InNode | ClosesNode
data LayoutInstruction = LayoutInstruction LayoutConstraint CssClasses


newtype LayoutP t m a = LayoutP (StateT LPS m a)

