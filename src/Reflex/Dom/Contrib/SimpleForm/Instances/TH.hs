{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Dom.Contrib.SimpleForm.Instances.TH () where

-- reflex imports
import qualified DataBuilder as B
import Reflex.Dom.Contrib.SimpleForm.Builder

-- template builders here to avoid the stage restriction

deriveSFMaybe::Q [Dec]
deriveSFMaybe = do
  [d|instance (SimpleFormC e t m,Builder (SimpleForm e t m) a)=>Builder (SimpleFormR e t m) (Maybe a) where
       buildA md Nothing  = liftF (itemL attrs0 . layoutHoriz) ($(B.handleNothingL typeName) md)
       buildA md (Just x) = liftF (itemL attrs0 . layoutHoriz) ($(B.handleJustL typeName) md x)|]

