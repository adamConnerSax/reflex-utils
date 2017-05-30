module Reflex.Dom.Contrib.DynamicUtils
  (
    dynBasedOn
  , dynAsEv
  , traceDynAsEv
  , mDynAsEv
  , dynamicMaybeAsEv
  , traceMDynAsEv
  , traceDynMAsEv
  ) where

import qualified Reflex as R
import qualified Reflex.Dom as RD


dynBasedOn :: (R.Reflex t, R.MonadHold t m) => R.Dynamic t a -> R.Event t a -> m (R.Dynamic t a)
dynBasedOn d e = R.buildDynamic (R.sample $ R.current d) e

-- NB: It's crucial that the updated event be first.  If the dyn is updated by the caller's use of postbuild then
-- that's the value we want not the tagged current value.
dynAsEv :: RD.PostBuild t m => R.Dynamic t a -> m (R.Event t a)
dynAsEv dyn = (\x -> R.leftmost [R.updated dyn, R.tag (R.current dyn) x]) <$> RD.getPostBuild

traceDynAsEv :: RD.PostBuild t m => (a -> String) -> R.Dynamic t a -> m (R.Event t a)
traceDynAsEv f dyn = do
  postbuild <- RD.getPostBuild
  let f' prefix x = prefix ++ f x
      upEv = R.traceEventWith (f' "update-") $ R.updated dyn
      pbEv = R.traceEventWith (f' "postbuild-") $ R.tag (R.current dyn) postbuild
  return $ R.leftmost [upEv, pbEv] 


-- turn a Maybe (Dynamic t a) into an Event with an initial firing to represent the value at postbuild.  Nothing -> never fire
mDynAsEv :: (R.Reflex t, RD.PostBuild t m) => Maybe (R.Dynamic t a) -> m (R.Event t a)
mDynAsEv mDyn = maybe (return R.never) dynAsEv mDyn

-- turn a Dynamic t (Maybe a) into an Event with an initial firing to represent the value at postbuild. Only fires on values.
dynamicMaybeAsEv :: (R.Reflex t, RD.PostBuild t m) => R.Dynamic t (Maybe a) -> m (R.Event t a)
dynamicMaybeAsEv dma = R.fmapMaybe id <$> dynAsEv dma

traceMDynAsEv :: (R.Reflex t, RD.PostBuild t m) => (a -> String) -> Maybe (R.Dynamic t a) -> m (R.Event t a)
traceMDynAsEv f mDyn = maybe (return R.never) (traceDynAsEv f) mDyn


traceDynMAsEv :: RD.PostBuild t m => (a -> String) -> R.Dynamic t (Maybe a) -> m (R.Event t a)
traceDynMAsEv f dma = R.fmapMaybe id <$> traceDynAsEv (maybe "Nothing" f) dma 
