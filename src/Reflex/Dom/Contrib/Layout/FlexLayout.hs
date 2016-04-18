{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Contrib.Layout.FlexLayout
       (
         flexCssBS
       , flexFillR
       , flexFillL
       , flexHCenter
       , flexVCenter
       , flexFillD
       , flexFillU
       , flexLayoutRowSimple
       , flexLayoutColSimple
       , flexLayoutItemSimple
       , flexLayoutRow
       , flexCol
       , flexLayoutCol
       , flexRow
       , flexLayoutRow'
       , flexCol'
       , flexLayoutCol'
       , flexRow'
       ) where


import qualified Reflex as R
import qualified Reflex.Dom as RD

import Control.Monad.IO.Class (MonadIO)

import qualified Data.Map as M

import Reflex.Dom.Contrib.Layout.Core
import Reflex.Dom.Contrib.Layout.Types
--import Reflex.Dom.Contrib.Layout.GridLayout

import Clay
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.String (fromString)

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
--    let flexRowStyle n = (fromString (".gl-flex-row-" ++ show n)) <? do ("flex" -: fromString (show n ++ " 0 auto"))
--    mapM_ flexRowStyle [1..numberFlexGrowOptions]
  let flexJustifyStyle s = (fromString (".gl-justify-" ++ s)) ? do { ("display" -: "flex"); ("display" -: "-webkit-flex"); ("justify-content" -: fromString s); ("-webkit-justify-content" -: fromString s) }
      flexJustifyStyles = ["flex-start","flex-end","center","space-between","space-around"]
  mapM_ flexJustifyStyle flexJustifyStyles

flexCss = do
  flexFillStyles
  flexGridStyles

flexCssBS::B.ByteString
flexCssBS = B.concat . BL.toChunks . encodeUtf8  $ renderWith pretty [] $ flexCss

wrapWidget::RD.MonadWidget t m=>m a->m a
wrapWidget = RD.divClass "" 

flexFillR::(RD.MonadWidget t m,MonadIO (R.PushM t))=>m a->m a
flexFillR w = do
  RD.divClass "flexFillH" $ do
    a <- wrapWidget w
    RD.divClass "fill" RD.blank
    return a

flexFillL::(RD.MonadWidget t m,MonadIO (R.PushM t))=>m a->m a
flexFillL w = do
  RD.divClass "flexFillH" $ do  
    RD.divClass "fill" RD.blank
    wrapWidget w

flexHCenter::(RD.MonadWidget t m,MonadIO (R.PushM t))=>m a->m a
flexHCenter w = do
  RD.divClass "flexFillH" $ do
    RD.divClass "fill" RD.blank
    a <- wrapWidget w
    RD.divClass "fill" RD.blank
    return a

flexVCenter::(RD.MonadWidget t m,MonadIO (R.PushM t))=>m a->m a
flexVCenter w = do
  RD.divClass "flexFillV" $ do
    RD.divClass "fill" RD.blank
    a <- wrapWidget w
    RD.divClass "fill" RD.blank
    return a

flexFillD::(RD.MonadWidget t m,MonadIO (R.PushM t))=>m a->m a
flexFillD w = do
  RD.divClass "flexFillV" $ do
    a <- wrapWidget w
    RD.divClass "fill" RD.blank
    return a

flexFillU::(RD.MonadWidget t m,MonadIO (R.PushM t))=>m a->m a
flexFillU w = do
  RD.divClass "flexFillV" $ do  
    RD.divClass "fill" RD.blank
    wrapWidget w

{-
flexLayoutRowSimple::(RD.MonadWidget t m,MonadIO (R.PushM t))=>m a->m a
flexLayoutRowSimple = RD.divClass "gl-flex-row"

flexLayoutColSimple::(RD.MonadWidget t m,MonadIO (R.PushM t))=>m a->m a
flexLayoutColSimple = RD.divClass "gl-flex-col"

flexLayoutItemSimple::(RD.MonadWidget t m,MonadIO (R.PushM t))=>m a->m a
flexLayoutItemSimple = RD.divClass "gl-flex-item-1"
-}

flexLayoutRowSimple::AddStyle a=>a->a
flexLayoutRowSimple = addStyle (CssClasses [CssClass "gl-flex-row"])

flexLayoutColSimple::AddStyle a=>a->a
flexLayoutColSimple = addStyle (CssClasses [CssClass "gl-flex-col"])

flexLayoutItemSimple::AddStyle a=>a->a
flexLayoutItemSimple = addStyle (CssClasses [CssClass "gl-flex-item-1"])


flexLayoutRowF::R.Reflex t=>LayoutF t
flexLayoutRowF _ lTree =
  let flexRowClasses = CssClasses [CssClass "gl-flex-row"]
  in addNewClassesToTreeTop flexRowClasses lTree

flexLayoutRowD::R.Reflex t=>[String]->LayoutDescription t
flexLayoutRowD tags = LayoutDescription flexLayoutRowF M.empty (["all","row","flexContainer"]++tags)

flexLayoutRow::RD.MonadWidget t m=>LayoutM t m a->LayoutM t m a
flexLayoutRow = addNewLayoutNode $ flexLayoutRowD []

flexLayoutRow'::RD.MonadWidget t m=>[String]->LayoutM t m a->LayoutM t m a 
flexLayoutRow' tags = addNewLayoutNode $ flexLayoutRowD tags

flexColF::R.Reflex t=>Int->LayoutF t
flexColF w _ lTree =
  let n = Prelude.max 1 $ Prelude.min w numberFlexGrowOptions
      flexColClasses = CssClasses [CssClass ("gl-flex-item-" ++ show n)]
  in addNewClassesToTreeTop flexColClasses lTree

flexColD::R.Reflex t=>[String]->Int->LayoutDescription t
flexColD tags w = LayoutDescription (flexColF w) M.empty (["all","col","flexItem"]++tags)

flexCol::RD.MonadWidget t m=>Int->LayoutM t m a->LayoutM t m a
flexCol w = addNewLayoutNode (flexColD [] w)

flexCol'::RD.MonadWidget t m=>[String]->Int->LayoutM t m a->LayoutM t m a
flexCol' tags w = addNewLayoutNode (flexColD tags w)

flexLayoutColF::R.Reflex t=>LayoutF t
flexLayoutColF _ lTree =
  let flexColClasses = CssClasses [CssClass "gl-flex-col"]
  in addNewClassesToTreeTop flexColClasses lTree

flexLayoutColD::R.Reflex t=>[String]->LayoutDescription t
flexLayoutColD tags = LayoutDescription flexLayoutColF M.empty (["all","col","flexContainer"] ++ tags)

flexLayoutCol::RD.MonadWidget t m=>LayoutM t m a->LayoutM t m a
flexLayoutCol = addNewLayoutNode (flexLayoutColD [])

flexLayoutCol'::RD.MonadWidget t m=>[String]->LayoutM t m a->LayoutM t m a
flexLayoutCol' tags = addNewLayoutNode (flexLayoutColD tags)


flexRowF::R.Reflex t=>Int->LayoutF t
flexRowF h _ lTree =
  let n = Prelude.max 1 $ Prelude.min h numberFlexGrowOptions
      flexRowClasses = CssClasses [CssClass ("gl-flex-item-" ++ show n)]
  in addNewClassesToTreeTop flexRowClasses lTree

flexRowD::R.Reflex t=>[String]->Int->LayoutDescription t
flexRowD tags h = LayoutDescription (flexRowF h) M.empty (["all","row","flexItem"]++tags)

flexRow::RD.MonadWidget t m=>Int->LayoutM t m a->LayoutM t m a
flexRow h = addNewLayoutNode (flexRowD [] h)

flexRow'::RD.MonadWidget t m=>[String]->Int->LayoutM t m a->LayoutM t m a
flexRow' tags h = addNewLayoutNode (flexRowD tags h)


{-
flexDirectionAttr::FlexDirection->String
flexDirectionAttr FlexRow = "row"
flexDirectionAttr FlexRowReverse = "row-reverse"
flexDirectionAttr FlexColumn = "column"
flexDirectionAttr FlexColumnReverse = "column-reverse"

flexWrapAttr::FlexWrap->String
flexWrapAttr FlexNoWrap = "no-wrap"
flexWrapAttr FlexDoWrap = "wrap"
flexWrapAttr FlexWrapReverse = "wrap-reverse"

flexLocationAttr::FlexLocation->String
flexLocationAttr FlexStart = "flex-start"
flexLocationAttr FlexEnd = "flex-end"
flexLocationAttr FlexCenter = "flex-center"

flexFillAttr::FlexFill->String
flexFillAttr FlexSpaceBetween = "space-between"
flexFillAttr FlexSpaceAround = "space-around"

flexJustifyAttr::FlexJustify->String
flexJustifyAttr FlexJustifyLocation l = flexLocationAttr l
flexJustifyAttr FlexJustifyFill f = flexFillAttr f

flexAlignItemsAttr::FlexAlignItems->String
flexAlignItemsAttr FlexAlignItemsLocation l = flexLocationAttr l
flexAlignItemsAttr FlexBaseline = "baseline"
flexAlignItemsAttr FlexItemStretch = "stretch"

flexAlignContentAttr::FlexAlignContent->String
flexAlignContentAttr FlexContentLocation l = flexLocationAttr l
flexAlignContentAttr FlexContentFill f = flexFillAttr f
flexAlignContentAttr FlexContentStretch = "stretch"

flexLengthAttr::FlexLength->String
flexLengthAttr Px n = show n ++ "px"
flexLengthAttr Rem x = show x ++ "rem"
flexLengthAttr Pct n = show n ++ "%"

flexBasisAttr::FlexBasis->String
flexBasisAttr FlexBasisLength l = flexLengthAttr l
flexBasisAttr FlexAuto = "auto"


flexContainerAttributes::FlexContainerConfig->M.Map String String
flexContainerAttributes fcc = ("style" =: "flex")
                              <> ("flex-flow" =: (flexDirectionAttr $ _fccDirection fcc ++ " || " ++
                                                  flexWrapAttr $ _fccWrap fcc))
                              <> ("flex-justify-content" =: (flexJustifyAttr $ _fccJustify fcc))
                              <> ("flex-align-items" =: (flexAlignItemsAttr $ _fccAlignItems fcc))
                              <> ("flex-align-content" =: (flexAlignContentAttr $ _fccAlignContent fcc))

flexItemAttributes::FlexItemConfig->M.Map String String
flexItemAttributes fic = maybe (M.empty) (\n->("order:
                                                            
-}

