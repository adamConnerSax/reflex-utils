{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Contrib.CssUtils
       (
         CssPathList
       , cssPrepend
       , embedCssFiles
       , CssLink
       , CssLinks
       , headElt
       ) where

import           Data.FileEmbed      (embedFile)
import           Language.Haskell.TH (ExpQ, listE)
import qualified Data.ByteString as B
import Reflex.Dom.Core (DomBuilder,el,elAttr,text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Text.Encoding (decodeUtf8)

cssPath::String->String->FilePath
cssPath basePath cssFile = basePath ++ cssFile

type CssPathList = [FilePath]
cssPrepend::String->CssPathList->CssPathList
cssPrepend pre paths = (mappend pre) <$> paths

embedCssFiles::CssPathList->ExpQ
embedCssFiles cpl = listE $ embedFile <$> cpl

type CssLink = T.Text -- TODO: this should be a proper URL type
type CssLinks = [CssLink] 

cssLinkToText::CssLink -> T.Text
cssLinkToText = id

styleSheet::DomBuilder t m=> CssLink -> m ()
styleSheet link =
  let attrs = M.fromList [("rel", "stylesheet")
                         , ("type", "text/css")
                         , ("href", cssLinkToText link)
                         ]
  in elAttr "link" attrs $ return ()


headElt::DomBuilder t m=> T.Text->CssLinks->B.ByteString->m ()
headElt title cssLinks embeddedCss = do
  el "title" (text title)
  mapM_ styleSheet cssLinks
  el "style" (text $ decodeUtf8 embeddedCss)
