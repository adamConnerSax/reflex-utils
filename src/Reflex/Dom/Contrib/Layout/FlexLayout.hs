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
       , flexFillR
       , flexFillL
       , flexHCenter
       , flexVCenter
       , flexFillD
       , flexFillU
       , flexRow
       , flexCol
       , flexItem
       , flexSizedItem
       ) where

--import Reflex.Dom.Contrib.StackedMonadWidget

import qualified Reflex as R
import qualified Reflex.Dom as RD

import Control.Monad.IO.Class (MonadIO)

import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Foldable (mapM_,foldl')

import Clay hiding (id)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.String (fromString)
import Data.Monoid ((<>))


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

numberFlexGrowOptions = 12::Int

-- I would prefer all these "auto"s to be "0%" but that is buggy in safari.  

flexContainerStyle = do
  "display" -: "flex"
  "display" -: "-webkit-flex"
  "align-items" -: "stretch"
  "-webkit-align-items" -: "stretch"  
  "flex" -: "1 0 auto"
  "-webkit-flex" -: "1 0 auto"  

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

flexCss = do
  flexFillStyles
  flexGridStyles

flexCssBS::B.ByteString
flexCssBS = B.concat . BL.toChunks . encodeUtf8  $ renderWith pretty []  flexCss

flexRow::(RD.DomBuilder t m,MonadIO (R.PushM t))=>m a->m a
flexRow = RD.divClass "gl-flex-row" 

flexCol::(RD.DomBuilder t m,MonadIO (R.PushM t))=>m a->m a
flexCol = RD.divClass "gl-flex-col" 

flexItem::(RD.DomBuilder t m,MonadIO (R.PushM t))=>m a->m a
flexItem = RD.divClass "gl-flex-item" 

flexSizedItem::(RD.DomBuilder t m,MonadIO (R.PushM t))=>Int->m a->m a
flexSizedItem n = let n' = Prelude.min n numberFlexGrowOptions in RD.divClass $ T.pack ("gl-flex-item-" ++ show n') 

wrapWidget::RD.DomBuilder t m=>m a->m a
wrapWidget = RD.divClass "" 


flexFillR::(RD.DomBuilder t m,MonadIO (R.PushM t))=>m a->m a
flexFillR w = 
  RD.divClass "flexFillH" $ do
    a <- wrapWidget w
    RD.divClass "fill" RD.blank
    return a

flexFillL::(RD.DomBuilder t m,MonadIO (R.PushM t))=>m a->m a
flexFillL w = 
  RD.divClass "flexFillH" $ do  
    RD.divClass "fill" RD.blank
    wrapWidget w

flexHCenter::(RD.DomBuilder t m,MonadIO (R.PushM t))=>m a->m a
flexHCenter w = 
  RD.divClass "flexFillH" $ do
    RD.divClass "fill" RD.blank
    a <- wrapWidget w
    RD.divClass "fill" RD.blank
    return a

flexVCenter::(RD.DomBuilder t m,MonadIO (R.PushM t))=>m a->m a
flexVCenter w = 
  RD.divClass "flexFillV" $ do
    RD.divClass "fill" RD.blank
    a <- wrapWidget w
    RD.divClass "fill" RD.blank
    return a

flexFillD::(RD.DomBuilder t m,MonadIO (R.PushM t))=>m a->m a
flexFillD w = 
  RD.divClass "flexFillV" $ do
    a <- wrapWidget w
    RD.divClass "fill" RD.blank
    return a

flexFillU::(RD.DomBuilder t m,MonadIO (R.PushM t))=>m a->m a
flexFillU w = 
  RD.divClass "flexFillV" $ do  
    RD.divClass "fill" RD.blank
    wrapWidget w



infixl 2 ##
(##)::(RD.DomBuilder t m,MonadIO (R.PushM t))=>(m a->m a)->m a->m a
(##) = ($)

infix 1 #$
(#$)::RD.DomBuilder t m=>(m a->m a)->m a->m a
(#$) = ($)

