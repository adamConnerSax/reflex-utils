module Reflex.Dom.Contrib.FormBuilder.Instances
       (
         formWidget
       , buildDynReadMaybe
       , buildDynReadable
       , BasicC
       , FormInstanceC
       , VFormBuilderC
       ) where

import           Reflex.Dom.Contrib.FormBuilder.Instances.Basic      ( buildDynReadMaybe
                                                                     , buildDynReadable
                                                                     , formWidget
                                                                     , BasicC
                                                                     , FormInstanceC)
import           Reflex.Dom.Contrib.FormBuilder.Builder             (VFormBuilderC)
import           Reflex.Dom.Contrib.FormBuilder.Instances.Containers
--import           Reflex.Dom.Contrib.SimpleForm.Instances.Extras
