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
import qualified Clay.Flexbox as Flexbox
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.String (fromString)
import Data.Monoid ((<>))
import Reflex.Dom.Contrib.Layout.Types (toCssString,CssClasses,emptyCss,LayoutOrientation(..),LayoutDirection(..))

flexFillStyles :: Css
flexFillStyles = do
  ".flexFillH" <> ".flexFillV" ? do
    display flex
    alignItems stretch
    Flexbox.flex 1 0 auto
    ("display" -: "-webkit-flex")
    ("-webkit-align-items" -: "stretch")    
    ("-webkit-flex" -: "1 0 auto")
    ".fill-space" <? do
      Flexbox.flex 1 1 auto
      ("-webkit-flex" -: "1 1 auto")
    ".fill-content" <? do
      Flexbox.flex 1 0 auto
      ("-webkit-flex" -: "1 0 auto")
  ".flexFillH" ? do
    flexDirection row
    ("-webkit-flex-direction" -: "row")
  ".flexFillV" ? do
    flexDirection column
    ("-webkit-flex-direction" -: "column")    

numberFlexGrowOptions :: Int
numberFlexGrowOptions = 12

-- I would prefer all these "auto"s to be "0%" but that is buggy in safari.  

flexContainerStyle :: Css
flexContainerStyle = do
  display flex
  alignItems stretch
  Flexbox.flex 1 0 auto
  "display" -: "-webkit-flex"
  "-webkit-align-items" -: "stretch"  
  "-webkit-flex" -: "1 0 auto"  

flexGridStyles :: Css
flexGridStyles = do
  ".gl-flex-row" ? do
    flexDirection row
    "-webkit-flex-direction" -: "row"    
    flexContainerStyle
  ".gl-flex-col" ? do
    flexDirection column
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

flexRow'::(RD.DomBuilder t m)=>CssClasses->m a->m a
flexRow' classes = RD.divClass ("gl-flex-row " <> toCssString classes) 

flexCol'::(RD.DomBuilder t m)=>CssClasses->m a->m a
flexCol' classes = RD.divClass ("gl-flex-col "  <> toCssString classes)

flexItem'::(RD.DomBuilder t m)=>CssClasses->m a->m a
flexItem' classes = RD.divClass ("gl-flex-item " <> toCssString classes)

flexSizedItem'::(RD.DomBuilder t m)=>CssClasses->Int->m a->m a
flexSizedItem' classes n = let n' = Prelude.min n numberFlexGrowOptions in RD.divClass $ T.pack ("gl-flex-item-" ++ show n')  <> toCssString classes

flexRow::(RD.DomBuilder t m)=>m a->m a
flexRow = flexRow' emptyCss 

flexCol::(RD.DomBuilder t m)=>m a->m a
flexCol = flexCol' emptyCss

flexItem::(RD.DomBuilder t m)=>m a->m a
flexItem = flexItem' emptyCss

flexSizedItem::(RD.DomBuilder t m)=>Int->m a->m a
flexSizedItem = flexSizedItem' emptyCss


wrapWidget::RD.DomBuilder t m=>m a->m a
wrapWidget = RD.divClass "fill-content" 


flexFill::(RD.DomBuilder t m)=>LayoutDirection->m a->m a
flexFill LayoutRight w =
  RD.divClass "flexFillH" $ do
    x <- wrapWidget w
    RD.divClass "fill-space" RD.blank
    return x

flexFill LayoutLeft w =
  RD.divClass "flexFillH" $ do  
    RD.divClass "fill-space" RD.blank
    wrapWidget w


flexFill LayoutBottom w = 
  RD.divClass "flexFillV" $ do
    x <- wrapWidget w
    RD.divClass "fill-space" RD.blank
    return x
    
flexFill LayoutTop w = 
  RD.divClass "flexFillV" $ do  
    RD.divClass "fill-space" RD.blank
    wrapWidget w

flexCenter::(RD.DomBuilder t m)=>LayoutOrientation->m a->m a
flexCenter LayoutHorizontal w = 
  RD.divClass "flexFillH" $ do
    RD.divClass "fill-space" RD.blank
    x <- wrapWidget w
    RD.divClass "fill-space" RD.blank
    return x
    
flexCenter LayoutVertical w = 
  RD.divClass "flexFillV" $ do
    RD.divClass "fill-space" RD.blank
    x <- wrapWidget w
    RD.divClass "fill-space" RD.blank
    return x


infixl 2 ##
(##)::(RD.DomBuilder t m)=>(m a->m a)->m a->m a
(##) = ($)

infix 1 #$
(#$)::RD.DomBuilder t m=>(m a->m a)->m a->m a
(#$) = ($)

