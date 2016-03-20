module Reflex.Dom.Contrib.Layout.ClayUtils where

import Clay (Css,renderWith,pretty)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL


cssToBS::Css->B.ByteString
cssToBS css = B.concat . BL.toChunks . encodeUtf8  $ renderWith pretty [] css
