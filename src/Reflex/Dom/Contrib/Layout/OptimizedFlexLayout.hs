module Reflex.Dom.Contrib.Layout.OptimizedFlexLayout
  (
    (##)
  , (#$)
  , optFlexRow
  , optFlexCol
  , optFlexItem
  , optFlexFillH
  , optFlexFillV
  ) where

import Reflex.Dom.Contrib.Layout.Types (LISeq,LNode,OpenLNode(..),LayoutInstruction(..),
                                        LNodeConstraint(..),CssClass(..),CssClasses(..),
                                        LNodeType(..),
                                        lNodeToFunction,closeLNode,openNAddCss)
import Reflex.Dom.Contrib.Layout.FlexLayout (flexCssBS) -- just to re-export

import Reflex.Dom (MonadWidget)

import Control.Monad.State (State,gets,modify,execState)
import Data.Sequence ((><),(|>),Seq,empty,singleton)
import Data.Foldable (mapM_,foldl')

{-
class AddLayout a where
  addLayout::LISeq->a->a

instance AddLayout LISeq where
  addLayout lis lis' = lis S.>< lis'

instance RD.MonadWidget t m=>AddLayout (m a) where
  addLayout = performLayouts
-}

optFlexSimple::LNodeConstraint->String->LISeq
optFlexSimple lc css = singleton $ LayoutInstruction lc (OpenLNode LDiv (CssClasses [CssClass css]))

optFlexRow::LISeq
optFlexRow = optFlexSimple ClosesLNode "gl-flex-row"

optFlexCol::LISeq
optFlexCol = optFlexSimple ClosesLNode "gl-flex-col"

optFlexItem::LISeq
optFlexItem = optFlexSimple InLNode "gl-flex-item"

optFlexFillH::LISeq
optFlexFillH = optFlexSimple ClosesLNode "flexFillH"

optFlexFillV::LISeq
optFlexFillV = optFlexSimple ClosesLNode "flexFillV"



infixl 2 ##
(##)::LISeq->LISeq->LISeq
(##) = (><)

infix 1 #$
(#$)::MonadWidget t m=>LISeq->m a->m a
(#$) = performLayout
  
performLayout::MonadWidget t m=>LISeq->m a->m a
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
    Just on -> NodeBuilderState Nothing (nodes |> (closeLNode on))

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
newNodeType (LayoutInstruction lc oln) mOln = modify $ f . closeCurrent where
  f = case lc of
    ClosesLNode -> addLNode (closeLNode oln)
    _              -> replaceON (Just oln)




