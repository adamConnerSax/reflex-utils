module Reflex.Dom.Contrib.EventUtils
  (
    leftWhenNotRight
  ) where

import qualified Reflex as R
--import qualified Reflex.Dom as RD


leftWhenNotRight :: R.Reflex t => R.Event t a -> R.Event t b -> R.Event t a
leftWhenNotRight fstEv sndEv = R.fmapMaybe id $ R.leftmost [Nothing <$ sndEv, Just <$> fstEv]
