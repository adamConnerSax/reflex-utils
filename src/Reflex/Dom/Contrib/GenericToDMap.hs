module Reflex.Dom.Contrib.GenericToDMap where ()

import GHC.Generics (Generic)
import Generics.SOP
import Control.Monad.ST
import Data.Dependent.Map

usingDMap::(f a -> DMap tag f)->(DMap tag f -> f b) -> (DMap tag f -> DMap tag f) -> a -> b 

class ToDMap a where
  toDMap::a -> DMap

--class GToDMap  


  
