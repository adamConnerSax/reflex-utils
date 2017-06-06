module Reflex.Dom.Contrib.EventUtils
  (
    leftWhenNotRight
  , fanBool
  ) where

import qualified Reflex as R

leftWhenNotRight :: R.Reflex t => R.Event t a -> R.Event t b -> R.Event t a
leftWhenNotRight fstEv sndEv = R.fmapMaybe id $ R.leftmost [Nothing <$ sndEv, Just <$> fstEv]

boolToEither::Bool -> Either () ()
boolToEither True  = Right ()
boolToEither False = Left ()

-- NB: right event fires if true, left if false--(FalseEv,TrueEv)--which fits Either but is not intuitive, at least to me
fanBool::R.Reflex t=>R.Event t Bool->(R.Event t (), R.Event t ())
fanBool = R.fanEither . fmap boolToEither
