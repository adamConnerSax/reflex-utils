{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Reflex.Dom.Contrib.Layout.OptimizedFlexLayout
  (
    (##)
  , (#$)
  , flexRow
  , flexCol
  , flexItem
  , flexSizedItem
  , flexFillH
  , flexFillV
  , flexFill
  , flexCenter
  , flexCssBS
  ) where

import Reflex.Dom.Contrib.Layout.Types (LISeq,LNode,OpenLNode(..),LayoutInstruction(..),
                                        LNodeConstraint(..),CssClass(..),CssClasses(..),
                                        LNodeType(..),
                                        lNodeToFunction,closeLNode,openNAddCss,
                                        LayoutOrientation(..),LayoutDirection(..))
import Reflex.Dom.Contrib.Layout.FlexLayout (flexCssBS,numberFlexGrowOptions) -- just to re-export

import qualified Reflex as R 
import qualified Reflex.Dom as RD 

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (State,gets,modify,execState)
import Data.Sequence ((><),(|>),Seq,empty,singleton)
import Data.Foldable (foldl')
import qualified Data.Text as T
import Data.Monoid ((<>))

{-
class AddLayout a where
  addLayout::LISeq->a->a

instance AddLayout LISeq where
  addLayout lis lis' = lis S.>< lis'

instance RD.DomBuilder t m=>AddLayout (m a) where
  addLayout = performLayouts
-}

type OFLC t m = (RD.DomBuilder t m, MonadIO (R.PushM t))

flexSimple::LNodeConstraint->T.Text->LISeq
flexSimple lc css = singleton $ LayoutInstruction lc (OpenLNode LDiv (CssClasses [CssClass css]))

flexRow::LISeq
flexRow = flexSimple ClosesLNode "gl-flex-row"

flexCol::LISeq
flexCol = flexSimple ClosesLNode "gl-flex-col"

flexItem::LISeq
flexItem = flexSimple InLNode "gl-flex-item"

flexSizedItem::Int->LISeq
flexSizedItem n = let n' = Prelude.min n numberFlexGrowOptions in flexSimple InLNode ("gl-flex-item-" <> T.pack (show n'))

flexFillH::LISeq
flexFillH = flexSimple ClosesLNode "flexFillH"

flexFillV::LISeq
flexFillV = flexSimple ClosesLNode "flexFillV"

wrapWidget::RD.DomBuilder t m=>m a->m a
wrapWidget = RD.divClass "" 

flexFill::OFLC t m=>LayoutDirection->m a->m a
flexFill LayoutRight w = 
  flexFillH #$ do
    a <- wrapWidget w
    RD.divClass "fill" RD.blank
    return a

flexFill LayoutLeft w = 
  flexFillH #$ do  
    RD.divClass "fill" RD.blank
    wrapWidget w

flexFill LayoutBottom w = 
  flexFillV #$ do
    a <- wrapWidget w
    RD.divClass "fill" RD.blank
    return a

flexFill LayoutTop w = 
  flexFillV #$ do  
    RD.divClass "fill" RD.blank
    wrapWidget w


flexCenter::OFLC t m=>LayoutOrientation->m a->m a
flexCenter LayoutHorizontal w = 
  flexFillH #$ do
    RD.divClass "fill" RD.blank
    a <- wrapWidget w
    RD.divClass "fill" RD.blank
    return a

flexCenter LayoutVertical w = 
  flexFillV #$ do
    RD.divClass "fill" RD.blank
    a <- wrapWidget w
    RD.divClass "fill" RD.blank
    return a


infixl 2 ##
(##)::LISeq->LISeq->LISeq
(##) = (><)

infix 1 #$
(#$)::RD.DomBuilder t m=>LISeq->m a->m a
(#$) = performLayout
  
performLayout::RD.DomBuilder t m=>LISeq->m a->m a
performLayout lis w = do
  let nodes = instructionsToNodes lis
      layoutF = foldl' (\f node -> f . lNodeToFunction node) id nodes
  layoutF w    

instructionsToNodes::LISeq->Seq LNode
instructionsToNodes lis = nbNodes . closeCurrent . flip execState emptyNBS $ mapM_ doOneInstruction lis

data NodeBuilderState = NodeBuilderState { nbONM::Maybe OpenLNode, nbNodes::Seq LNode } 

emptyNBS::NodeBuilderState
emptyNBS = NodeBuilderState Nothing empty

replaceON::Maybe OpenLNode->NodeBuilderState->NodeBuilderState
replaceON onM nbs = nbs { nbONM = onM }

addLNode::LNode->NodeBuilderState->NodeBuilderState
addLNode ln (NodeBuilderState onM nodes) = NodeBuilderState onM (nodes |> ln)

addAndReplace::LNode->Maybe OpenLNode->NodeBuilderState->NodeBuilderState
addAndReplace ln onM = addLNode ln . replaceON onM

closeCurrent::NodeBuilderState->NodeBuilderState
closeCurrent nbs@(NodeBuilderState onM nodes) =
  case onM of
    Nothing -> nbs
    Just on -> NodeBuilderState Nothing (nodes |> closeLNode on)

doOneInstruction::LayoutInstruction->State NodeBuilderState ()
doOneInstruction li@(LayoutInstruction lc oln) = do
  curONodeM <- gets nbONM
  case curONodeM of
    Nothing -> newNodeType li Nothing
    (Just on) -> if olnType on == olnType oln
                 then sameNodeType lc (olnCss oln) on
                 else newNodeType li (Just on)

sameNodeType::LNodeConstraint->CssClasses->OpenLNode->State NodeBuilderState ()
sameNodeType lc css oln = do
  let oln' = openNAddCss css oln
  modify $ case lc of
    OpensLNode ->  addAndReplace (closeLNode oln) (Just $ OpenLNode (olnType oln) css)
    InLNode -> replaceON (Just oln')
    ClosesLNode -> addAndReplace (closeLNode oln') Nothing

newNodeType::LayoutInstruction->Maybe OpenLNode->State NodeBuilderState ()
newNodeType (LayoutInstruction lc oln) _ = modify $ f . closeCurrent where
  f = case lc of
    ClosesLNode -> addLNode (closeLNode oln)
    _              -> replaceON (Just oln)




