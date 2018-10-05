{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs        #-}
module Reflex.Dom.Contrib.Widgets.EditableCollection
  (
    simpleCollectionValueEditor
  , simpleHideDeletesCollectionEditor
  , simpleRemoveDeletesCollectionEditor
  , DisplayCollection (..)
  , EditableCollection(..)
  , hideDeletesCollectionEditor
  , hideDeletesCollectionEditorWR
  , removeDeletesCollectionEditor
  , removeDeletesCollectionEditorWR
  , selectEditValues
  , editWithDeleteButton
  , reappearingEditWithDeleteButton
  , addNewItemWidget
  , addNewItemWidgetModal
  , buttonNoSubmit
  , updateKeyLabelMap
  , validOnly
  ) where

-- These need to go so that the widget piece of reflex-utils can be its own lib
import           Reflex.Dom.Contrib.EventUtils (fanBool)
import           Reflex.Dom.Contrib.ReflexConstraints (MonadWidgetExtraC)
import qualified Reflex.Dom.Contrib.Widgets.WidgetResult as WR

import qualified Reflex.Dom.Contrib.Widgets.ModalEditor as ME
import qualified Reflex.Dom.Contrib.Widgets.SafeDropdown as SD

-- for the collection functions and constraints they need
import           Reflex.Collections.Collections as RC
import           Reflex.Collections.SelfEditingCollection as RC

import           Reflex.Dom        ((=:))                 
import qualified Reflex            as R
import qualified Reflex.Dom        as RD

import           Data.Traversable (sequenceA) 
import           Control.Lens      ((&),(.~), view)
import           Control.Monad (join)
import           Control.Monad.Fix (MonadFix)
import           Data.Maybe (isJust, isNothing)
import           Data.Monoid ((<>))
import           Data.Kind (Type)
import           Data.Bool (bool)
import           Data.Proxy (Proxy (..))
import           Data.Either (either)
import qualified Data.Key as K

import qualified Data.Map          as M
import qualified Data.Text         as T
import qualified Data.IntMap as IM
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as S
import qualified Data.Array as A
import qualified Data.Tree as T

data DisplayCollection t k where
  DisplayAll :: DisplayCollection t k
  DisplayEach :: R.Dynamic t (M.Map T.Text T.Text) -> (k -> T.Text) -> DisplayCollection t k 

-- | Create a collection editing widget for modifying values only.  No adds or deletes from the collection.
-- For supported types (Map, HashMap, IntMap, [], Seq, Array, Tree), we need a widget to edit an item as input.
-- Initial DiplayCollection argument specifies whether entire collection is displayed or a dropdown per element. 
simpleCollectionValueEditor :: forall t m f a. ( RD.DomBuilder t m
                                               , RD.PostBuild t m
                                               , R.Adjustable t m
                                               , R.MonadHold t m                                               
                                               , MonadFix m
                                               , RC.Mergeable f
                                               , EditableCollection f
                                               , Ord (Key f))
  => DisplayCollection t (Key f) -- use a dropdown or show entire collection
  -> (RC.Key f -> R.Dynamic t a -> m (R.Dynamic t (Maybe a))) -- display and edit existing
  -> R.Dynamic t (f a)
  -> m (R.Dynamic t (f a))
simpleCollectionValueEditor display editWidget fDyn =
  case display of
    DisplayAll -> 
      -- change (Key f -> Dynamic t a -> m (Dynamic t (Maybe a))) into (Key f -> Dynamic t a -> m (Dynamic t a))
      let editValueWidget k vDyn = do
            aDynM <- editWidget k vDyn
            R.buildDynamic (R.sample $ R.current vDyn) $ R.fmapMaybe id (R.updated aDynM)
      in editOnlyValues id editValueWidget fDyn
    DisplayEach attrsDyn labelKey -> do
      let editValueWidget k vDyn = R.fmapMaybe id . R.updated <$> editWidget k vDyn
      diffFaEv <- selectEditValues attrsDyn labelKey (const id) editValueWidget fDyn
      dynamicPlusEvent fDyn (R.attachWith (flip RC.applyDiff) (R.current fDyn) (fmap Just <$> diffFaEv))

-- | Create a collection editing widget--allowing value edits, deletion of entries, and addition of new entries.
-- For supported collections (Map, HashMap, IntMap, [] and Seq), we need two widgets as input.
-- One to display and edit an existing item and another to input a new item.
-- collection may be displayed one at a time, via dropdown, or all at once.
simpleHideDeletesCollectionEditor :: forall t m f a. ( RD.DomBuilder t m
                                                     , RD.PostBuild t m
                                                     , MonadWidgetExtraC t m
                                                     , R.Adjustable t m
                                                     , R.MonadHold t m
                                                     , MonadFix m
                                                     , RC.Mergeable f
                                                     , EditableCollection f
                                                     , Ord (Key f)
                                                     , Key f ~ Key (KeyValueSet f))
  => DisplayCollection t (Key f) -- use a dropdown or show entire collection
  -> (RC.Key f -> R.Dynamic t a -> m (R.Dynamic t (Maybe a))) -- display and edit existing
  -> m (R.Dynamic t (Maybe (NewItem f a))) -- input a new one
  -> R.Dynamic t (f a)
  -> m (R.Dynamic t (f a))
simpleHideDeletesCollectionEditor display editWidget newItemWidget fDyn =
  let editValueWidget k vDyn = R.fmapMaybe id . R.updated <$> editWidget k vDyn -- m (R.Event t (f a))
      pf = Proxy :: Proxy f
      editAndDeleteWidget k vDyn = do
        widgetVis <- R.holdDyn True $ True <$ R.updated vDyn
        editWithDeleteButton editValueWidget M.empty (buttonNoSubmit "-") widgetVis k vDyn
      editDeletableWidget = case display of
        DisplayAll -> flip ecListViewWithKey editAndDeleteWidget
        DisplayEach ddAttrs toText -> selectEditValues ddAttrs toText (updateKeyLabelMap (Proxy :: Proxy f) toText) editAndDeleteWidget
      addNewWidget = (addNewItemWidgetModal pf $ newKeyValueWidget pf (maybe (Left ("Invalid Input" :: T.Text)) Right) newItemWidget) . fmap RC.toKeyValueSet 
  in hideDeletesCollectionEditor editDeletableWidget addNewWidget id id fDyn


simpleRemoveDeletesCollectionEditor :: forall t m f a. ( RD.DomBuilder t m
                                                       , RD.PostBuild t m
                                                       , MonadWidgetExtraC t m
                                                       , R.Adjustable t m
                                                       , R.MonadHold t m
                                                       , MonadFix m
                                                       , RC.Mergeable f
                                                       , RC.FannableC f a
                                                       , RC.SequenceableWithEventC t f (Key (KeyValueSet f), Maybe a)
                                                       , Monoid (f a)
                                                       , EditableCollection f
                                                       , Ord (Key f)
                                                       , Key f ~ Key (KeyValueSet f))
  => DisplayCollection t (Key f) -- use a dropdown or show entire collection
  -> (RC.Key f -> R.Dynamic t a -> m (R.Event t (Key (KeyValueSet f), Maybe a))) -- display and edit existing
  -> m (R.Dynamic t (Maybe (NewItem f a))) -- input a new one
  -> R.Dynamic t (f a)
  -> m (R.Dynamic t (f a))
simpleRemoveDeletesCollectionEditor display editWidget newItemWidget fDyn =
  let editValueWidget k a aEv = R.holdDyn a aEv >>= editWidget k -- $  R.fmapMaybe id . R.updated <$> editWidget k vDyn -- m (R.Event t (f a))
      pf = Proxy :: Proxy f
      addNewWidget = addNewItemWidgetModal pf $ newKeyValueWidget pf (maybe (Left ("Invalid Input" :: T.Text)) Right) newItemWidget
      diffFromKVAndNew kva fa = RC.slUnion (Just <$> RC.toKeyValueSet fa) (Nothing <$ kva)
  in case display of
    DisplayAll -> WR.widgetResultToDynamic <$> removeDeletesCollectionEditorWR editValueWidget addNewWidget diffFromKVAndNew id id fDyn
    DisplayEach ddAttrs labelToText -> WR.widgetResultToDynamic <$> removeDeletesSelectCollectionEditorWR ddAttrs labelToText editWidget addNewWidget diffFromKVAndNew id id fDyn
    
-- | This class allows the collection editing functions to be polymorphic over the types supported by Reflex.Collections
class (KeyedCollection f, Diffable f) => EditableCollection (f :: Type -> Type) where
  type NewItem f b :: Type
  editOnlyValues :: (RD.Adjustable t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
    => (a -> b) -- to map the input type to the output type at widget build time. Often "id" or "Just" or "Right"
    -> (RC.Key f -> R.Dynamic t a -> m (R.Dynamic t b)) -- widget for editing one value, possibly with visible key
    -> R.Dynamic t (f a) -- input collection
    -> m (R.Dynamic t (f b))

  -- required for the full structure editor
  ecListViewWithKey :: (RD.Adjustable t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
    => R.Dynamic t (f v) -> (Key f -> R.Dynamic t v -> m (R.Event t a)) -> m (R.Event t (KeyValueSet f a))

  --
  ecListViewWithKeyShallowDiff :: (RD.Adjustable t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
    => Proxy f -> R.Event t (Diff f v) -> (Key f -> v -> R.Event t v -> m (R.Event t a)) -> m (R.Event t (KeyValueSet f a))

  -- required for the select version of the full structure editor
  ecSelectViewListWithKey :: (RD.Adjustable t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
    => R.Dynamic t (Key f)    -- Current selection key
    -> R.Dynamic t (f a)      -- Dynamic container of values
    -> (Key f -> R.Dynamic t a -> R.Dynamic t Bool -> m (R.Event t b)) -- create a widget from Dynamic value and Dynamic Bool (is widget selected?)
    -> m (R.Event t (KeyValueSet f b))

  -- required for either full structure editor in order to generalize over Key/Value collections like Map vs Value collections like []
  newKeyValueWidget :: (R.Reflex t, Applicative g, Monad m)
    => Proxy f
    -> (forall x. g x -> Either e x)
    -> m (R.Dynamic t (g (NewItem f b)))
    -> (R.Dynamic t (KeyValueSet f a) -> m (R.Dynamic t (Either e (Key (KeyValueSet f), b))))

  -- some containers (e.g., lists and sequences) "contract" when items are deleted and we need to keep track of this so edits are applied to the correct elements
  diffAfterDeletes :: Proxy f -> [Key (KeyValueSet f)] -> KeyValueSet f b -> KeyValueSet f b
  
    
instance Ord k => EditableCollection (M.Map k) where
  type NewItem (M.Map k) b = (k,b)
  editOnlyValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  ecListViewWithKeyShallowDiff = RC.listViewWithKeyShallowDiff
  ecSelectViewListWithKey = RC.selectViewListWithKey
  newKeyValueWidget _ nat = const . fmap (fmap nat)
  diffAfterDeletes _ _ = id

instance (Hashable k, Ord k) => EditableCollection (HM.HashMap k) where
  type NewItem (HM.HashMap k) b = (k,b)
  editOnlyValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  ecListViewWithKeyShallowDiff = RC.listViewWithKeyShallowDiff 
  ecSelectViewListWithKey = RC.selectViewListWithKey
  newKeyValueWidget _ nat = const . fmap (fmap nat)
  diffAfterDeletes _ _ = id

instance EditableCollection IM.IntMap where
  type NewItem IM.IntMap b = (Int,b)
  editOnlyValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  ecListViewWithKeyShallowDiff = RC.listViewWithKeyShallowDiff 
  ecSelectViewListWithKey = RC.selectViewListWithKey 
  newKeyValueWidget _ nat = const . fmap (fmap nat)
  diffAfterDeletes _ _ = id

instance EditableCollection [] where
  type NewItem [] b = b
  editOnlyValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  ecListViewWithKeyShallowDiff = RC.listViewWithKeyShallowDiff 
  ecSelectViewListWithKey = RC.selectViewListWithKey
  newKeyValueWidget _ nat inputgbW kvDyn = do
    newEitherbDyn <- fmap nat <$> inputgbW
    let newKeyDyn = (\im -> if IM.null im then 0 else (1 + (fst $ IM.findMax im))) <$> kvDyn
--    let newKeyDyn = length <$> fDyn 
    return $ R.zipDynWith (\ea eb -> (,) <$> ea <*> eb) (Right <$> newKeyDyn) newEitherbDyn
  diffAfterDeletes :: Proxy [] -> [Int] -> IM.IntMap b -> IM.IntMap b
  diffAfterDeletes _ deletedKeys oldDiff =
    let newKey n = n - (length . filter (< n) $ deletedKeys)
    in IM.foldrWithKey (\n x nm -> IM.insert (newKey n) x nm) IM.empty oldDiff 
      
instance EditableCollection S.Seq where
  type NewItem S.Seq b = b 
  editOnlyValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  ecListViewWithKeyShallowDiff = RC.listViewWithKeyShallowDiff 
  ecSelectViewListWithKey = RC.selectViewListWithKey
  newKeyValueWidget _ nat inputgbW kvDyn = do
    newEitherbDyn <- fmap nat <$> inputgbW
    let newKeyDyn = (\im -> if IM.null im then 0 else 1 + (fst $ IM.findMax im)) <$> kvDyn
--    let newKeyDyn = length <$> fDyn 
    return $ R.zipDynWith (\ea eb -> (,) <$> ea <*> eb) (Right <$> newKeyDyn) newEitherbDyn
  diffAfterDeletes :: Proxy S.Seq -> [Int] -> IM.IntMap b -> IM.IntMap b
  diffAfterDeletes _ deletedKeys oldDiff =
    let newKey n = n - (length . filter (< n) $ deletedKeys)
    in IM.foldrWithKey (\n x nm -> IM.insert (newKey n) x nm) IM.empty oldDiff 
  
instance (A.Ix k, Enum k, Bounded k) => EditableCollection (A.Array k) where
  type NewItem (A.Array k) b = (k,b) 
  editOnlyValues aTob widget initial = RC.simplifyDynMaybe aTob (flip RC.listWithKeyMaybe widget) initial
  ecListViewWithKey = RC.listViewWithKeyMaybe
  ecListViewWithKeyShallowDiff = RC.listViewWithKeyShallowDiffMaybe 
  ecSelectViewListWithKey = RC.selectViewListWithKeyMaybe  
  newKeyValueWidget _ _ _ = undefined -- can't add (or delete) in (Array k) collections
  diffAfterDeletes _ _ = id

instance EditableCollection T.Tree where
  type NewItem T.Tree b = b
  editOnlyValues aTob widget initial = RC.simplifyDynMaybe aTob (flip RC.listWithKeyMaybe widget) initial     
  ecListViewWithKey = RC.listViewWithKeyMaybe
  ecListViewWithKeyShallowDiff = RC.listViewWithKeyShallowDiffMaybe 
  ecSelectViewListWithKey = RC.selectViewListWithKeyMaybe  
  newKeyValueWidget _ _ _ = undefined -- add to where?  Delete all below?  For now undefined.
  diffAfterDeletes _ _ = undefined -- this might make sense but unimplemented for now

-- helper for the case when you want the output to update only on input change or a valid edit
-- e.g., validOnly (flip (editValues Right) widget) initial
validOnly :: (R.Reflex t, R.MonadHold t m, Traversable f, Applicative g)
  => (forall x. g x -> Maybe x) -> (R.Dynamic t (f a) -> m (R.Dynamic t (f (g a)))) -> R.Dynamic t (f a) -> m (R.Dynamic t (f a))
validOnly toMaybe editCollectionF initial = do
  edited <- editCollectionF initial
  let validEditEv = R.fmapMaybe toMaybe $ R.updated $ (fmap sequenceA) edited
  dynamicPlusEvent initial validEditEv
       
{-
There are 3 sources of change:
1. A change to the input (fDyn)
2. An edit or delete of an existing element (editDeleteDiffMaybeEv)
3. The addition of a new element (addDiffMaybeEv)
which must be reflected in 2 places:
1. The collection as it's rendered in the DOM via editDeleteWidget 
2. The output of this widget (curDyn)
And we must not feed edits or deletes back into the edit/Delete widget or we get a causality loop.
-}
hideDeletesCollectionEditorWR :: forall t m f a b. ( RD.Adjustable t m
                                        , RD.PostBuild t m
                                        , RD.MonadHold t m                 
                                        , MonadFix m
                                        , RD.DomBuilder t m
                                        , RC.Mergeable f
                                        , EditableCollection f)
  => (R.Dynamic t (f a) -> m (R.Event t (Diff f b))) -- edit/Delete widget.  Fires only on change. Something like Reflex.Collections.
  -> (R.Dynamic t (f b) -> m (R.Event t (KeyValueSet f b))) --  add item(s) widget.  Fires only on valid add.
  -> (f a -> f b) -- we need this to have a starting point for the output dynamics
  -> (f b -> f a) -- we need this to feed the changes back in to the collection functions
  -> R.Dynamic t (f a) -- input collection
  -> m (WR.WidgetResult t (f b))
hideDeletesCollectionEditorWR editDeleteWidget addWidget faTofb fbTofa fDyn = mdo
  editDeleteDiffMaybeEv <- editDeleteWidget (fbTofa <$> editDeleteInputDyn)
  addDiffMaybeEv <- fmap (fmap Just) <$> addWidget curDyn
  let inputFbBeh = faTofb <$> R.current fDyn 
      inputFbEv = faTofb <$> R.updated fDyn
      deletedKeysEv = fmap fst . RC.toKeyValueList . RC.slFilter isNothing <$> editDeleteDiffMaybeEv -- R.Event t [Key (Diff f)]
      shiftedEditDeleteDiffMaybeEv = R.attachWith (diffAfterDeletes (Proxy :: Proxy f)) (R.current deletedKeysDyn) editDeleteDiffMaybeEv
      editDeleteAddDiffEv = R.leftmost [shiftedEditDeleteDiffMaybeEv, addDiffMaybeEv] -- should this combine if same frame?
      addFEv = R.attachWith (flip applyDiff) (R.current curDyn) addDiffMaybeEv
      internalChangeEv = R.attachWith (flip applyDiff) (R.current curDyn) editDeleteAddDiffEv
      newFEv = R.leftmost [inputFbEv, internalChangeEv]
      updatedEditDeleteInputEv = R.leftmost [addFEv, inputFbEv]
  curDyn <- R.buildDynamic (R.sample inputFbBeh) newFEv -- always has current value
  deletedKeysDyn <- R.foldDyn (\mdk cdk -> maybe [] (cdk <>) mdk) [] $ R.leftmost [Just <$> deletedKeysEv, Nothing <$ updatedEditDeleteInputEv]
  editDeleteInputDyn <- R.buildDynamic (R.sample inputFbBeh) updatedEditDeleteInputEv -- updates to current on add or carries completely new input
  WR.buildWidgetResult (faTofb <$> fDyn) internalChangeEv 

hideDeletesCollectionEditor :: ( RD.Adjustable t m
                    , RD.PostBuild t m
                    , RD.MonadHold t m                 
                    , MonadFix m
                    , RD.DomBuilder t m
                    , RC.Mergeable f
                    , EditableCollection f)
  => (R.Dynamic t (f a) -> m (R.Event t (Diff f b))) -- edit/Delete widget.  Fires only on change. Something like Reflex.Collections.ListView.
  -> (R.Dynamic t (f b) -> m (R.Event t (KeyValueSet f b))) --  add item(s) widget.  Fires only on valid add.
  -> (f a -> f b) -- we need this to have a starting point for the output dynamics
  -> (f b -> f a) -- we need this to feed the changes back in to the collection functions
  -> R.Dynamic t (f a) -- input collection
  -> m (R.Dynamic t (f b))
hideDeletesCollectionEditor editDeleteWidget addWidget faTofb fbTofa fDyn =
  WR.widgetResultToDynamic <$>  hideDeletesCollectionEditorWR editDeleteWidget addWidget faTofb fbTofa fDyn 

removeDeletesCollectionEditorWR :: forall t m f k a b. ( RD.Adjustable t m
                                                       , RD.PostBuild t m
                                                       , RD.MonadHold t m                 
                                                       , MonadFix m
                                                       , Monoid (f a)
                                                       , RD.DomBuilder t m
                                                       , RC.Mergeable f
                                                       , RC.FannableC f a
                                                       , RC.SequenceableWithEventC t f (Key (KeyValueSet f), Maybe b)
                                                       , k ~ Key (KeyValueSet f)
                                                       , EditableCollection f)
  => (Key f -> a -> R.Event t a -> m (R.Event t (k, Maybe b))) -- edit/delete widget. Carries the KeyValueSet key. Fires only on internal change.  
  -> (R.Dynamic t (RC.KeyValueSet f b) -> m (R.Event t (RC.KeyValueSet f b))) --  add item(s) widget.  Fires only on valid add.
  -> (RC.KeyValueSet f b -> f a -> RC.Diff f a)
  -> (RC.Diff f a -> RC.Diff f b)
  -> (RC.Diff f b -> RC.Diff f a)
  -> R.Dynamic t (f a)
  -> m (WR.WidgetResult t (f b))
removeDeletesCollectionEditorWR itemWidget addWidget updateFromInput dfaTodfb dfbTodfa faDyn = do
  let proxyf = Proxy :: Proxy f
      updateDeletes x = dfbTodfa . remappedUpdateDeletes proxyf x 
      updateAll = remappedUpdateAll proxyf
  postBuild <- R.getPostBuild
  rec (kvbDyn, dfbEv) <- RC.selfEditingCollectionWithChanges dfaTodfb updateDeletes updateAll itemWidget (mempty :: f a)  dfaEv -- Dynamic t (KeyValueSet f b)
      dfbAddEv <- fmap (fmap Just) <$> addWidget kvbDyn -- Event t (Diff f b)
      let newInputFaEv = R.leftmost [R.updated faDyn, R.tag (R.current faDyn) postBuild]
          dfaNewInputEv = R.attachWith updateFromInput (R.current kvbDyn) newInputFaEv
          dfaEv = R.leftmost [dfaNewInputEv, dfbTodfa <$> dfbAddEv]
          curDyn = RC.fromCompleteKeyValueSet <$> kvbDyn
  return $ WR.unsafeBuildWidgetResult curDyn (() <$ R.leftmost [dfbEv, dfbAddEv])

removeDeletesCollectionEditor :: forall t m f k a b. ( RD.Adjustable t m
                                                     , RD.PostBuild t m
                                                     , RD.MonadHold t m                 
                                                     , MonadFix m
                                                     , Monoid (f a)
                                                     , RD.DomBuilder t m
                                                     , RC.Mergeable f
                                                     , RC.FannableC f a
                                                     , RC.SequenceableWithEventC t f (Key (KeyValueSet f), Maybe b)
                                                     , k ~ Key (KeyValueSet f)
                                                     , EditableCollection f)
  => (Key f -> a -> R.Event t a -> m (R.Event t (k, Maybe b))) -- edit/delete widget. Carries the KeyValueSet key. Fires only on internal change.  
  -> (R.Dynamic t (RC.KeyValueSet f b) -> m (R.Event t (RC.KeyValueSet f b))) --  add item(s) widget.  Fires only on valid add.
  -> (RC.KeyValueSet f b -> f a -> RC.Diff f a)
  -> (RC.Diff f a -> RC.Diff f b)
  -> (RC.Diff f b -> RC.Diff f a)
  -> R.Dynamic t (f a)
  -> m (R.Dynamic t (f b))
removeDeletesCollectionEditor itemWidget addWidget updateFromInput dfaTodfb dfbTodfa faDyn =
  WR.widgetResultToDynamic <$>  removeDeletesCollectionEditorWR itemWidget addWidget updateFromInput dfaTodfb dfbTodfa faDyn 


removeDeletesSelectCollectionEditorWR ::  forall t m f k a b. ( RD.Adjustable t m
                                                              , RD.PostBuild t m
                                                              , RD.MonadHold t m                 
                                                              , MonadFix m
                                                              , Monoid (f a)
                                                              , RD.DomBuilder t m
                                                              , RC.Mergeable f
                                                              , RC.FannableC f a
                                                              , RC.SequenceableWithEventC t f (Key (KeyValueSet f), Maybe b)
                                                              , k ~ Key (KeyValueSet f)
                                                              , Key f ~ Key (KeyValueSet f)
                                                              , Ord (Key f)
                                                              , EditableCollection f)
  => R.Dynamic t (M.Map T.Text T.Text)
  -> (k -> T.Text)
  -> (k -> R.Dynamic t a -> m (R.Event t (k, Maybe b))) -- edit/delete widget. Carries the KeyValueSet key. Fires only on internal change.  
  -> (R.Dynamic t (RC.KeyValueSet f b) -> m (R.Event t (RC.KeyValueSet f b))) --  add item(s) widget.  Fires only on valid add.
  -> (RC.KeyValueSet f b -> f a -> RC.Diff f a)
  -> (RC.Diff f a -> RC.Diff f b)
  -> (RC.Diff f b -> RC.Diff f a)
  -> R.Dynamic t (f a)
  -> m (WR.WidgetResult t (f b))
removeDeletesSelectCollectionEditorWR ddAttrsDyn keyToLabel itemWidget addWidget updateFromInput dfaTodfb dfbTodfa faDyn = do
  let proxyf = Proxy :: Proxy f
      updateDeletes x = dfbTodfa . remappedUpdateDeletes proxyf x 
      updateAll = remappedUpdateAll proxyf
      dynamicallyVisible visDyn w = let visAttrsDyn = bool hiddenCSS visibleCSS <$> visDyn in RD.elDynAttr "div" visAttrsDyn w
      keyLabelMapInDyn = M.fromList . fmap (\(k, _) -> (k, keyToLabel k)) . toKeyValueList <$> faDyn
  postBuild <- R.getPostBuild
  attrsDyn <- R.foldDyn (<>) ("size" RD.=: "1") (R.updated ddAttrsDyn)
  rec let keyLabelMapUpdateEv pf = R.attachWith (flip $ updateKeyLabelMap pf keyToLabel) (R.current keyLabelMapDyn) $ R.leftmost [dfbEv, dfbAddEv]
          safeDropdownConfig = SD.SafeDropdownConfig R.never attrsDyn
      keyLabelMapDyn <- dynamicPlusEvent keyLabelMapInDyn $ keyLabelMapUpdateEv proxyf
      sdd <- SD.safeDropdown Nothing keyLabelMapDyn safeDropdownConfig
      sddNullEv <- R.updated <$> R.holdUniqDyn (isNothing <$> view SD.safeDropdown_value sdd) -- why not (M.null <$> keyLabelMap)?
      let (notNullEv, nullEv) = fanBool sddNullEv
          nullWidgetEv = return (R.constDyn RC.slEmpty, R.never) <$ nullEv
          safeKeyEv = R.tagPromptlyDyn (head . M.keys <$> keyLabelMapDyn) notNullEv
          selWidget safeKey = do
            selDyn <- R.holdDyn safeKey (R.fmapMaybe id $ view SD.safeDropdown_change sdd)
            RC.selectSelfEditingCollectionWithChanges dfaTodfb updateDeletes updateAll dynamicallyVisible selDyn itemWidget (mempty :: f a) dfaEv
      (kvbDynDyn, dfbDynEv) <- R.splitDynPure <$> (RD.widgetHold (return (R.constDyn RC.slEmpty, R.never)) $ R.leftmost [nullWidgetEv, selWidget <$> safeKeyEv]) -- Event t (Diff f b)
      let kvbDyn = join kvbDynDyn
          dfbEv = R.switchPromptlyDyn dfbDynEv
      dfbAddEv <- fmap (fmap Just) <$> addWidget kvbDyn
      let newInputFaEv = R.leftmost [R.updated faDyn, R.tag (R.current faDyn) postBuild]
          dfaNewInputEv = R.attachWith updateFromInput (R.current kvbDyn) newInputFaEv
          dfaEv = R.leftmost [dfaNewInputEv, dfbTodfa <$> dfbAddEv]
          curDyn = RC.fromCompleteKeyValueSet <$> kvbDyn
  return $ WR.unsafeBuildWidgetResult curDyn (() <$ R.leftmost [dfbEv, dfbAddEv])
      

remapKVS :: Diffable f => Proxy f -> RC.KeyValueSet f (Key (KeyValueSet f), Maybe b) -> RC.Diff f b
remapKVS _ kvsPair =
  let singleton k a = RC.fromKeyValueList $ [(k,a)] 
  in K.foldrWithKey (\_ (k,mb) df -> RC.slUnion df (singleton k mb)) RC.slEmpty kvsPair
  
remappedUpdateDeletes :: Diffable f => Proxy f -> RC.KeyValueSet f b -> RC.KeyValueSet f (Key (KeyValueSet f), Maybe b) -> RC.Diff f b
remappedUpdateDeletes pf _ x = RC.slFilter isNothing (remapKVS pf x) 

remappedUpdateAll :: Diffable f => Proxy f -> RC.KeyValueSet f b -> RC.KeyValueSet f (Key (KeyValueSet f), Maybe b) -> RC.Diff f b
remappedUpdateAll pf kvb x = RC.slUnion (remapKVS pf x) (Just <$> kvb) -- left biased union

-- This can be plugged into edit Structure in place of ecListViewWithKey to get a selection with drodown in place of the entire list
selectEditValues :: ( RD.Adjustable t m
                    , RD.PostBuild t m
                    , RD.MonadHold t m                 
                    , MonadFix m
                    , RD.DomBuilder t m
                    , RC.Diffable f
                    , Ord (RC.Key f)
                    , EditableCollection f) -- for dropdown label
  => R.Dynamic t (M.Map T.Text T.Text) -- dropdown attrs.
  -> (RC.Key f -> T.Text)
  -> (RC.KeyValueSet f b -> M.Map (Key f) T.Text -> M.Map (Key f) T.Text) -- function to adjust dropdown labels 
  -> (Key f -> R.Dynamic t a -> m (R.Event t b)) -- single element edit widget 
  -> R.Dynamic t (f a)
  -> m (R.Event t (RC.KeyValueSet f b))
selectEditValues ddAttrsDyn keyToLabel updateLabelMap elemWidget fDyn = mdo
  attrsDyn <- R.foldDyn (<>) ("size" RD.=: "1") (R.updated ddAttrsDyn)
  let safeDropdownConfig = SD.SafeDropdownConfig R.never attrsDyn
      keyLabelMapInDyn = M.fromList . fmap (\(k, _) -> (k, keyToLabel k)) . toKeyValueList <$> fDyn
      keyLabelMapUpdateEv = R.attachWith (flip updateLabelMap) (R.current keyLabelMapDyn) diffBEv
  keyLabelMapDyn <- dynamicPlusEvent keyLabelMapInDyn keyLabelMapUpdateEv
  sdd <- SD.safeDropdown Nothing keyLabelMapDyn safeDropdownConfig
  sddNullEv <- R.updated <$> R.holdUniqDyn (isNothing <$> view SD.safeDropdown_value sdd) -- why not (M.null <$> keyLabelMap)?
  let (notNullEv, nullEv) = fanBool sddNullEv
      nullWidgetEv = return R.never <$ nullEv
      safeKeyEv = R.tagPromptlyDyn (head . M.keys <$> keyLabelMapDyn) notNullEv
      dynamicallyVisible visDyn w = let visAttrsDyn = bool hiddenCSS visibleCSS <$> visDyn in RD.elDynAttr "div" visAttrsDyn w
      selWidget safeKey = do
        selDyn <- R.holdDyn safeKey (R.fmapMaybe id $ view SD.safeDropdown_change sdd)
        ecSelectViewListWithKey selDyn fDyn (\k vDyn visDyn -> dynamicallyVisible visDyn $ elemWidget k vDyn)
  diffBEv <- R.switchPromptlyDyn <$> (RD.widgetHold (return R.never) $ R.leftmost [nullWidgetEv, selWidget <$> safeKeyEv]) -- Event t (Diff f b)
  return diffBEv

updateKeyLabelMap ::  (RC.Diffable f
                     , RC.Key f ~ RC.Key (RC.KeyValueSet f)
                     , Ord (Key (KeyValueSet f)))
  => Proxy f -> (Key f -> T.Text) -> RC.Diff f a -> M.Map (Key f) T.Text -> M.Map (Key f) T.Text
updateKeyLabelMap _ keyToLabel d x =
  let (deletes, updatesOrAdds) = M.partition isNothing . M.fromList . RC.toKeyValueList $ d
  in M.union (M.difference x deletes) (M.mapWithKey (\k _ -> keyToLabel k) updatesOrAdds)


reappearingEditWithDeleteButton :: ( R.Reflex t
                                   , R.MonadHold t m
                                   , R.PostBuild t m
                                   , RD.DomBuilder t m
                                   , MonadFix m)
  => (k -> R.Dynamic t a -> m (R.Event t b)) -- widget to edit one element, fires only on valid change
  -> M.Map T.Text T.Text -- attrs for the div surrounding the widget with button
  -> m (R.Event t ()) -- delete button widget
  -> k -- key in collection
  -> R.Dynamic t a -- input element
  -> m (R.Event t (Maybe b)) -- 'Just b' if changed, 'Nothing' if deleted.                                   
reappearingEditWithDeleteButton editWidget attrs delButton k vDyn = do
  widgetVisDyn <- R.holdDyn True $ True <$ R.updated vDyn
  editWithDeleteButton editWidget attrs delButton widgetVisDyn k vDyn 
                                
editWithDeleteButton :: ( R.Reflex t
                        , R.MonadHold t m
                        , R.PostBuild t m
                        , RD.DomBuilder t m
                        , MonadFix m)
  => (k -> R.Dynamic t a -> m (R.Event t b)) -- widget to edit one element, fires only on valid change
  -> M.Map T.Text T.Text -- attrs for the div surrounding the widget with button
  -> m (R.Event t ()) -- delete button widget
  -> R.Dynamic t Bool -- visibility of widget
  -> k -- key in collection
  -> R.Dynamic t a -- input element
  -> m (R.Event t (Maybe b)) -- 'Just b' if changed, 'Nothing' if deleted. Only fire on edits.
editWithDeleteButton editWidget attrs delButton visibleDyn k vDyn = mdo
  postBuild <- R.getPostBuild
  let widgetAttrsDyn = (\x -> attrs <> if x then visibleCSS else hiddenCSS) <$> visibleDyn'
  (visibleDyn', outEv') <- RD.elDynAttr "div" widgetAttrsDyn $ do
    editedEv <- RD.el "span" $ editWidget k vDyn
    delButtonEv <- RD.el "span" $ delButton
    let outEv = R.leftmost [Just <$> editedEv, Nothing <$ delButtonEv]
    visDyn <- R.holdDyn True $ R.leftmost [R.updated visibleDyn, True <$ editedEv, False <$ delButtonEv]
    return (visDyn, outEv)
  return $ leftWhenNotRight outEv' $ R.leftmost [() <$ R.updated vDyn, postBuild] -- this leftWhenNotRight is crucial. ??

newItemEditorConfig :: (Show e, R.Reflex t) => ME.ModalEditorConfig t e a
newItemEditorConfig = RD.def
                      & ME.modalEditor_updateOutput .~ ME.OnOk
                      & ME.modalEditor_closeOnOk .~ True
                      & ME.modalEditor_openButton .~ const (ME.ButtonConfig "Add" M.empty (Just "fa fa-plus"))
                      & ME.modalEditor_XButton .~ Nothing
                      & ME.modalEditor_OkButton .~ flip (ME.disableAndDisplayIfError (T.pack . show)) (ME.ButtonConfig "OK" M.empty (Just "fa fa-check"))
                      & ME.modalEditor_CancelButton .~ const (ME.ButtonConfig "Cancel" M.empty (Just "fa fa-window-close"))

addNewItemWidgetModal :: ( R.Reflex t
                         , RD.DomBuilder t m
                         , RD.PostBuild t m
                         , RD.MonadHold t m
                         , MonadFix m
                         , MonadWidgetExtraC t m
                         , Diffable f
                         , Show e
                         , Monoid e)
  => Proxy f
  -> (R.Dynamic t (KeyValueSet f a) -> m (R.Dynamic t (Either e (Key (KeyValueSet f),b)))) -- widget to edit an entry and, given the input Key/Value set, return the proper key. Left for invalid value.
  -> R.Dynamic t (KeyValueSet f a)
  -> m (R.Event t (KeyValueSet f b))
addNewItemWidgetModal _ editPairW kvDyn = do
  let modalEditW = const $ editPairW kvDyn
--      blankInput = R.constDyn $ Left mempty
      pairEvToKeyValueSetEv pairEv = fromKeyValueList . pure <$> pairEv -- here, fromKeyValueList :: [(Key f, v)] -> KeyValueSet f v 
  newPairEv <- ME.modalEditor_change <$> ME.modalEditorEither modalEditW (R.constDyn $ Left mempty) newItemEditorConfig
  return $ pairEvToKeyValueSetEv newPairEv

addNewItemWidget :: ( R.Reflex t
                    , RD.DomBuilder t m
                    , RD.PostBuild t m
                    , RD.MonadHold t m
                    , MonadFix m
                    , MonadWidgetExtraC t m
                    , Diffable f
                    , Show e
                    , Monoid e)
  => Proxy f
  -> (R.Dynamic t (KeyValueSet f a) -> m (R.Dynamic t (Either e (Key (KeyValueSet f),b)))) -- widget to edit an entry and, given the input collection, return the proper key. Left for invalid value.
  -> R.Dynamic t (KeyValueSet f a)
  -> m (R.Event t (KeyValueSet f b))
addNewItemWidget _ editPairW kvDyn = do
  let pairEvToKeyValueSetEv pairEv = fromKeyValueList . pure <$> pairEv -- here, fromKeyValueList :: [(Key f, v)] -> KeyValueSet f v 
  newPairEv <- R.fmapMaybe (either (const Nothing) Just) . R.updated <$> editPairW kvDyn
  return $ pairEvToKeyValueSetEv newPairEv --leftWhenNotRight newPairEv (R.updated fDyn)

-- one possible button widget
buttonNoSubmit :: RD.DomBuilder t m => T.Text -> m (R.Event t ())
buttonNoSubmit t = (RD.domEvent RD.Click . fst) <$> RD.elAttr' "button" ("type" RD.=: "button") (RD.text t)
    
hiddenCSS :: M.Map T.Text T.Text
hiddenCSS  = "style" =: "display: none !important"
visibleCSS :: M.Map T.Text T.Text
visibleCSS = "style" =: "display: inline"

dynamicPlusEvent :: (R.Reflex t, RD.MonadHold t m) => R.Dynamic t a -> R.Event t a -> m (R.Dynamic t a)
dynamicPlusEvent aDyn aEv = R.buildDynamic (R.sample . R.current $ aDyn) $ R.leftmost [R.updated aDyn, aEv]

leftWhenNotRight :: R.Reflex t => R.Event t a -> R.Event t b -> R.Event t a
leftWhenNotRight leftEv rightEv = R.fmapMaybe id $ R.leftmost [Nothing <$ rightEv, Just <$> leftEv]

rightWhenNotLeft :: R.Reflex t => R.Event t a -> R.Event t b -> R.Event t b
rightWhenNotLeft leftEv rightEv = R.fmapMaybe id $ R.leftmost [Nothing <$ leftEv, Just <$> rightEv]

-- make a version using shallow diff so we can be more efficient in handling of adds?
{-
collectionEditorWR' :: forall t m f a b. ( RD.Adjustable t m
                                         , RD.PostBuild t m
                                         , RD.MonadHold t m                 
                                         , MonadFix m
                                         , RD.DomBuilder t m
                                         , RC.Mergeable f
                                         , EditableCollection f
                                         , RC.MapLike (Diff f))                 
  => (R.Event t (Diff f a) -> m (R.Event t (Diff f (Maybe b)))) -- edit/Delete widget.
  -> (R.Dynamic t (f a) -> m (R.Event t (Diff f b))) --  add item widget.  Fires only on valid add.
  -> (f a -> f b) -- we need this to have a starting point for the output dynamics
  -> (Diff f (Maybe b) -> Diff f (Maybe a)) -- we need this to feed the changes back in to the collection functions
  -> R.Dynamic t (f a) -- input collection
  -> m (WR.WidgetResult t (f b))
collectionEditorWR' editDeleteWidget addWidget faTofb dfMbTodfMa faDyn = mdo
  -- we start with all the events, splitting the edits into deletes, which are not tracked inside the editDelete widget,
  -- and edits, which are. 
  editDeleteDiffFMbEv <- editDeleteWidget editDeleteInputEv
  addDiffFbEv <- addWidget fDyn
  let inputFbEv = faTofb <$> R.updated faDyn
      deleteOnlyDiffFMbEv = RC.mlFilter isNothing <$> editDeleteDiffFMbEv
      editOnlyDiffFMbEv =  RC.mlFilter isJust <$> editDeleteDiffFMbEv
      inputFaDiffEv = R.attachWith RC.diff (fbTofa <$> R.current curFbDyn) (R.updated faDyn)
  -- we construct a dyn with the current dynamic value of Fb. We need to track deletes since the keys from
  -- the edit/delete widget may not match the output keys because of deletes (e.g., [])
      deletedKeysEv = fmap fst . RC.toKeyValueList $ deleteOnlyDiffFMbEv -- R.Event t [Key (Diff f)]
  -- deleted keys since last widget input update    
  deletedKeysDyn <- R.foldDyn (\mdk cdk -> maybe [] (cdk <>) mdk) [] $ R.leftmost [Just <$> deletedKeysEv, Nothing <$ editDeleteInputEv]
  let shiftedEditDeleteDiffFMbEv = R.attachWith (diffAfterDeletes (Proxy :: Proxy f)) (R.current deletedKeysDyn) editDeleteDiffMaybeEv
      editDeleteFbEv = R.attachWith (flip R.applyDiff) (R.current curFbDyn) shiftedEditDeleteDiffFMbEv
      addFbEv = R.attachWith (flip RC.applyDiff) (R.current curFbDyn) (fmap Just <$> addDiffFb)
  let inputFbBeh = faTofb <$> R.current faDyn      
  curFbDyn <- R.buildDynamic (R.sample inputFbBeh) $ R.leftmost [editDeleteFbEv, addFbEv, inputFbEv]
  -- we construct a dyn to reflect the current internal state of the collection *as the edit/delete widget knows it*
  -- which is without deletes until an add or new input updates the widget.  And 
  let editsOnlyFbEv <- R.attachWith (flip RC.applyDiff) (R.current editDeleteStateFbDyn) editsOnlyFbEv      
  editDeleteStateFbDyn <- R.buildDynamic (R.sample inputFBeh) $ R.leftmost [editsOnlyFbEv, addFbEv, inputFbEv]
  -- now we construct the event used to update the edit/delete widget
  -- this is tricky since the widget takes a diffs *relative to what it knows (adds, edits but not deletes)*
  -- first we track deletes since the internal state has been updated


      
      

      editDeleteAddDiffEv = R.leftmost [shiftedEditDeleteDiffMaybeEv, addDiffMaybeEv] -- should this combine if same frame?
      internalChangeEv = R.attachWith (flip applyDiff) (R.current curDyn) editDeleteAddDiffEv
      editDiffFbEv = R.attachWith (flip applyDiff) (R.current curDyn) $ (RC.mlFilter isJust editDeleteDiffMaybeEv)
      newFEv = R.leftmost [inputFbEv, internalChangeEv]
      


  
  WR.buildWidgetResult curDyn internalChangeEv 
-}
