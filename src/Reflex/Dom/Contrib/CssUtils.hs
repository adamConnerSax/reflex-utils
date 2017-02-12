{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Contrib.CssUtils
       (
         CssPathList
       , cssPrepend
       , embedCssFiles
       , CssLink(..)
       , CssLinks(..)
       , headElt
       ) where

import           Data.FileEmbed      (embedFile)
import           Language.Haskell.TH (ExpQ, listE)
import qualified Data.ByteString as B
import Reflex.Dom.Core (DomBuilder,el,elAttr,text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Text.Encoding (decodeUtf8)
import Data.Monoid (Monoid(..),(<>))

cssPath::String->String->FilePath
cssPath basePath cssFile = basePath ++ cssFile

type CssPathList = [FilePath]
cssPrepend::String->CssPathList->CssPathList
cssPrepend pre paths = (mappend pre) <$> paths

embedCssFiles::CssPathList->ExpQ
embedCssFiles cpl = listE $ embedFile <$> cpl

data CssLink = CssLink { url::T.Text, integrity::Maybe T.Text, crossorigin::Maybe T.Text } -- TODO: this should be a proper URL type
newtype CssLinks = CssLinks [CssLink] 

instance Monoid CssLinks where
  mempty = CssLinks []
  (CssLinks a) `mappend` (CssLinks b) = CssLinks (a `mappend` b)

styleSheet::DomBuilder t m=> CssLink -> m ()
styleSheet (CssLink u mi mco) =
  let integrityAttrF = maybe id (M.insert "integrity") mi
      crossOriginAttrF = maybe id (M.insert "crossorigin") mco
      attrs = integrityAttrF . crossOriginAttrF $ M.fromList [("rel", "stylesheet")
                                                              , ("type", "text/css")
                                                              , ("href", u)
                                                              ]
  in elAttr "link" attrs $ return ()


headElt::DomBuilder t m=> T.Text->CssLinks->B.ByteString->m ()
headElt title (CssLinks cssLinks) embeddedCss = do
  el "title" (text title)
  mapM_ styleSheet cssLinks
  el "style" (text $ decodeUtf8 embeddedCss)
