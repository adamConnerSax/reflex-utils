{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
module Reflex.Dom.Contrib.Layout.FlexLayout
       (
         flexCssBS
       , numberFlexGrowOptions
       , (##)
       , (#$)
       , flexFill
       , flexCenter
       , flexRow
       , flexCol
       , flexItem
       , flexSizedItem
       , flexRow'
       , flexCol'
       , flexItem'
       , flexSizedItem'
       ) where

import qualified Reflex as R
import qualified Reflex.Dom as RD

import Control.Monad.IO.Class (MonadIO)

import Clay hiding (id)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.String (fromString)
import Data.Monoid ((<>))
import Reflex.Dom.Contrib.Layout.Types (toCssString,CssClasses,emptyCss,LayoutOrientation(..),LayoutDirection(..))

flexFillStyles :: Css
flexFillStyles = do
  ".flexFillH" ? do
    ("display" -: "flex")
    ("display" -: "-webkit-flex")
    ("flex-direction" -: "row")
    ("-webkit-flex-direction" -: "row")
    ("align-items" -: "stretch")
    ("-webkit-align-items" -: "stretch")    
    ("flex" -: "1 0 auto")
    ("-webkit-flex" -: "1 0 auto")
    ".fill" <? do
      ("flex" -: "1")
      ("-webkit-flex" -: "1")
  ".flexFillV" ? do
    ("display" -: "flex")
    ("display" -: "-webkit-flex")    
    ("flex-direction" -: "column")
    ("-webkit-flex-direction" -: "column")    
    ("align-items" -: "stretch")
    ("-webkit-align-items" -: "stretch")    
    ("flex" -: "1 0 auto")
    ("-webkit-flex" -: "1 0 auto")
    ".fill" <? do
      ("flex" -: "1")
      ("-webkit-flex" -: "1")

numberFlexGrowOptions :: Int
numberFlexGrowOptions = 12

-- I would prefer all these "auto"s to be "0%" but that is buggy in safari.  

flexContainerStyle :: Css
flexContainerStyle = do
  "display" -: "flex"
  "display" -: "-webkit-flex"
  "align-items" -: "stretch"
  "-webkit-align-items" -: "stretch"  
  "flex" -: "1 0 auto"
  "-webkit-flex" -: "1 0 auto"  

flexGridStyles :: Css
flexGridStyles = do
  ".gl-flex-row" ? do
    "flex-direction" -: "row"
    "-webkit-flex-direction" -: "row"    
    flexContainerStyle
  ".gl-flex-col" ? do
    "flex-direction" -: "column"
    "-webkit-flex-direction" -: "column"    
    flexContainerStyle
  let flexItemStyle n = (fromString (".gl-flex-item-" ++ show n)) <? do { "flex" -: fromString (show n ++ " 0 auto"); "-webkit-flex" -: fromString (show n ++ " 0 auto") }
  mapM_ flexItemStyle [1..numberFlexGrowOptions]
  let flexJustifyStyle s = (fromString (".gl-justify-" ++ s)) ? do { ("display" -: "flex"); ("display" -: "-webkit-flex"); ("justify-content" -: fromString s); ("-webkit-justify-content" -: fromString s) }
      flexJustifyStyles = ["flex-start","flex-end","center","space-between","space-around"]
  mapM_ flexJustifyStyle flexJustifyStyles

flexCss :: Css
flexCss = do
  flexFillStyles
  flexGridStyles

flexCssBS::B.ByteString
flexCssBS = B.concat . BL.toChunks . encodeUtf8  $ renderWith pretty []  flexCss

flexRow'::(RD.DomBuilder t m,MonadIO (R.PushM t))=>CssClasses->m a->m a
flexRow' classes = RD.divClass ("gl-flex-row " <> toCssString classes) 

flexCol'::(RD.DomBuilder t m,MonadIO (R.PushM t))=>CssClasses->m a->m a
flexCol' classes = RD.divClass ("gl-flex-col "  <> toCssString classes)

flexItem'::(RD.DomBuilder t m,MonadIO (R.PushM t))=>CssClasses->m a->m a
flexItem' classes = RD.divClass ("gl-flex-item " <> toCssString classes)

flexSizedItem'::(RD.DomBuilder t m,MonadIO (R.PushM t))=>CssClasses->Int->m a->m a
flexSizedItem' classes n = let n' = Prelude.min n numberFlexGrowOptions in RD.divClass $ T.pack ("gl-flex-item-" ++ show n')  <> toCssString classes

flexRow::(RD.DomBuilder t m,MonadIO (R.PushM t))=>m a->m a
flexRow = flexRow' emptyCss 

flexCol::(RD.DomBuilder t m,MonadIO (R.PushM t))=>m a->m a
flexCol = flexCol' emptyCss

flexItem::(RD.DomBuilder t m,MonadIO (R.PushM t))=>m a->m a
flexItem = flexItem' emptyCss

flexSizedItem::(RD.DomBuilder t m,MonadIO (R.PushM t))=>Int->m a->m a
flexSizedItem = flexSizedItem' emptyCss


wrapWidget::RD.DomBuilder t m=>m a->m a
wrapWidget = RD.divClass "" 


flexFill::(RD.DomBuilder t m,MonadIO (R.PushM t))=>LayoutDirection->m a->m a
flexFill LayoutRight w =
  RD.divClass "flexFillH" $ do
    x <- wrapWidget w
    RD.divClass "fill" RD.blank
    return x

flexFill LayoutLeft w =
  RD.divClass "flexFillH" $ do  
    RD.divClass "fill" RD.blank
    wrapWidget w


flexFill LayoutBottom w = 
  RD.divClass "flexFillV" $ do
    x <- wrapWidget w
    RD.divClass "fill" RD.blank
    return x
    
flexFill LayoutTop w = 
  RD.divClass "flexFillV" $ do  
    RD.divClass "fill" RD.blank
    wrapWidget w

flexCenter::(RD.DomBuilder t m,MonadIO (R.PushM t))=>LayoutOrientation->m a->m a
flexCenter LayoutHorizontal w = 
  RD.divClass "flexFillH" $ do
    RD.divClass "fill" RD.blank
    x <- wrapWidget w
    RD.divClass "fill" RD.blank
    return x
    
flexCenter LayoutVertical w = 
  RD.divClass "flexFillV" $ do
    RD.divClass "fill" RD.blank
    x <- wrapWidget w
    RD.divClass "fill" RD.blank
    return x


infixl 2 ##
(##)::(RD.DomBuilder t m,MonadIO (R.PushM t))=>(m a->m a)->m a->m a
(##) = ($)

infix 1 #$
(#$)::RD.DomBuilder t m=>(m a->m a)->m a->m a
(#$) = ($)

