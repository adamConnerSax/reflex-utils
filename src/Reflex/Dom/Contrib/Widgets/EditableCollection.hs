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
module Reflex.Dom.Contrib.Widgets.EditableCollection
  (
    simpleCollectionValueEditor
  , simpleCollectionEditor
  , DisplayCollection (..)
  , EditableCollection(..)
  , collectionEditor
  , selectEditValues
  , editWithDeleteButton
  , addNewItemWidget
  , buttonNoSubmit
  , updateKeyLabelMap
  , validOnly
  ) where

--import Reflex.Dom.Contrib.DynamicUtils (dynAsEv, dynPlusEvent)
import           Reflex.Dom.Contrib.EventUtils (fanBool)
import           Reflex.Dom.Contrib.ReflexConstraints (MonadWidgetExtraC)
import           Reflex.Dom.Contrib.Widgets.WidgetResult (dynamicToWidgetResult)

import qualified Reflex.Dom.Contrib.Widgets.ModalEditor as ME
import qualified Reflex.Dom.Contrib.Widgets.SafeDropdown as SD

-- for the collection functions and constraints they need
import           Reflex.Collections.Collections as RC

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
--import           Data.Default (Default (def)) 

import qualified Data.Map          as M
import qualified Data.Text         as T
import qualified Data.IntMap as IM
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as S
import qualified Data.Array as A
import qualified Data.Tree as T

data DisplayCollection t k = DisplayAll | DisplayEach (R.Dynamic t (M.Map T.Text T.Text)) (k -> T.Text) 

dynamicPlusEvent :: (R.Reflex t, RD.MonadHold t m) => R.Dynamic t a -> R.Event t a -> m (R.Dynamic t a)
dynamicPlusEvent aDyn aEv = R.buildDynamic (R.sample . R.current $ aDyn) $ R.leftmost [R.updated aDyn, aEv]

-- | Create a collection editing widget for modifying values only.  No adds or deletes from the collection.
-- For supported types (Map, HashMap, IntMap, [], Seq, Array, Tree), we need a widget to edit an item as input.
-- Initial DiplayCollection argument specifies whether entire collection is displayed or a dropdown per element. 
simpleCollectionValueEditor :: forall t m f a. ( RD.DomBuilder t m
                                               , RD.PostBuild t m
                                               , R.Adjustable t m
                                               , R.MonadHold t m                                               
                                               , MonadFix m
                                               , RC.Mergeable f (Maybe a)
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
            dynamicPlusEvent vDyn $ R.fmapMaybe id (R.updated aDynM)
      in editOnlyValues id editValueWidget fDyn
    DisplayEach attrsDyn labelKey -> do
      let editValueWidget k vDyn = R.fmapMaybe id . R.updated <$> editWidget k vDyn
      diffFaEv <- selectEditValues attrsDyn labelKey (const id) editValueWidget fDyn
      dynamicPlusEvent fDyn (R.attachWith (flip RC.applyDiff) (R.current fDyn) (fmap Just <$> diffFaEv))

-- | Create a collection editing widget--allowing value edits, deletion of entries, and addition of new entries.
-- For supported collections (Map, HashMap, IntMap, [] and Seq), we need two widgets as input.
-- One to display and edit an existing item and another to input a new item.
-- collection may be displayed one at a time, via dropdown, or all at once.
simpleCollectionEditor :: forall t m f a. ( RD.DomBuilder t m
                                          , RD.PostBuild t m
                                          , MonadWidgetExtraC t m
                                          , R.Adjustable t m
                                          , R.MonadHold t m
                                          , MonadFix m
                                          , RC.Mergeable f (Maybe a)
                                          , EditableCollection f
                                          , Ord (Key f)
                                          , Key f ~ Key (Diff f))
  => DisplayCollection t (Key f) -- use a dropdown or show entire collection
  -> (RC.Key f -> R.Dynamic t a -> m (R.Dynamic t (Maybe a))) -- display and edit existing
  -> m (R.Dynamic t (Maybe (NewItem f a))) -- input a new one
  -> R.Dynamic t (f a)
  -> m (R.Dynamic t (f a))
simpleCollectionEditor display editWidget newItemWidget fDyn =
  let editValueWidget k vDyn = R.fmapMaybe id . R.updated <$> editWidget k vDyn -- m (R.Event t (f a))
      editAndDeleteWidget = editWithDeleteButton editValueWidget M.empty (buttonNoSubmit "-") (R.constDyn True)
      editDeletableWidget = case display of
        DisplayAll -> flip ecListViewWithKey editAndDeleteWidget
        DisplayEach ddAttrs toText -> selectEditValues ddAttrs toText (updateKeyLabelMap (Proxy :: Proxy f)) editAndDeleteWidget
      addNewWidget = addNewItemWidget $ newKeyValueWidget (maybe (Left ("Invalid Input" :: T.Text)) Right) newItemWidget 
  in collectionEditor editDeletableWidget addNewWidget id id fDyn

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
    => R.Dynamic t (f v) -> (Key f -> R.Dynamic t v -> m (R.Event t a)) -> m (R.Event t (Diff f a))

  -- required for the select version of the full structure editor
  ecSelectViewListWithKey :: (RD.Adjustable t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
    => R.Dynamic t (Key f)    -- Current selection key
    -> R.Dynamic t (f a)      -- Dynamic container of values
    -> (Key f -> R.Dynamic t a -> R.Dynamic t Bool -> m (R.Event t b)) -- create a widget from Dynamic value and Dynamic Bool (is widget selected?)
    -> m (R.Event t (Diff f b))

  -- required for either full structure editor in order to generalize over Key/Value collections like Map vs Value collections like []
  newKeyValueWidget :: (R.Reflex t, Applicative g, Monad m)
    => (forall x. g x -> Either e x) -> m (R.Dynamic t (g (NewItem f b))) -> (R.Dynamic t (f a) -> m (R.Dynamic t (Either e (Key (Diff f), b))))
    
instance Ord k => EditableCollection (M.Map k) where
  type NewItem (M.Map k) b = (k,b)
  editOnlyValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  ecSelectViewListWithKey = RC.selectViewListWithKey
  newKeyValueWidget nat = const . fmap (fmap nat) 

instance (Hashable k, Ord k) => EditableCollection (HM.HashMap k) where
  type NewItem (HM.HashMap k) b = (k,b)
  editOnlyValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  ecSelectViewListWithKey = RC.selectViewListWithKey  
  newKeyValueWidget nat = const . fmap (fmap nat)

instance EditableCollection IM.IntMap where
  type NewItem IM.IntMap b = (Int,b)
  editOnlyValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  ecSelectViewListWithKey = RC.selectViewListWithKey  
  newKeyValueWidget nat = const . fmap (fmap nat)

instance EditableCollection [] where
  type NewItem [] b = b
  editOnlyValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  ecSelectViewListWithKey = RC.selectViewListWithKey  
  newKeyValueWidget nat inputgbW fDyn = do
    newEitherbDyn <- fmap nat <$> inputgbW 
    return $ R.zipDynWith (\ea eb -> (,) <$> ea <*> eb) (Right . length <$> fDyn) newEitherbDyn
            
instance EditableCollection S.Seq where
  type NewItem S.Seq b = b 
  editOnlyValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  ecSelectViewListWithKey = RC.selectViewListWithKey  
  newKeyValueWidget nat inputgbW fDyn = do
    newEitherbDyn <- fmap nat <$> inputgbW 
    return $ R.zipDynWith (\ea eb -> (,) <$> ea <*> eb) (Right . S.length <$> fDyn) newEitherbDyn
  
instance (A.Ix k, Enum k, Bounded k) => EditableCollection (A.Array k) where
  type NewItem (A.Array k) b = (k,b) 
  editOnlyValues aTob widget initial = RC.simplifyDynMaybe aTob (flip RC.listWithKeyMaybe widget) initial
  ecListViewWithKey = RC.listViewWithKeyMaybe
  ecSelectViewListWithKey = RC.selectViewListWithKeyMaybe  
  newKeyValueWidget _ _ = undefined -- can't add (or delete) in (Array k) collections

instance EditableCollection T.Tree where
  type NewItem T.Tree b = b
  editOnlyValues aTob widget initial = RC.simplifyDynMaybe aTob (flip RC.listWithKeyMaybe widget) initial     
  ecListViewWithKey = RC.listViewWithKeyMaybe
  ecSelectViewListWithKey = RC.selectViewListWithKeyMaybe  
  newKeyValueWidget _ _ = undefined -- add to where?  Delete all below?  For now undefined.

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
collectionEditor :: ( RD.Adjustable t m
                    , RD.PostBuild t m
                    , RD.MonadHold t m                 
                    , MonadFix m
                    , RD.DomBuilder t m
                    , RC.Mergeable f (Maybe b))                 
  => (R.Dynamic t (f a) -> m (R.Event t (Diff f (Maybe b)))) -- edit/Delete widget.  Fires only on change. Something like Reflex.Collections.ListView.
  -> (R.Dynamic t (f a) -> m (R.Event t (Diff f b))) --  add item widget.  Fires only on valid add.
  -> (f a -> f b) -- we need this to have a starting point for the output dynamics
  -> (f b -> f a) -- we need this to feed the changes back in to the collection functions
  -> R.Dynamic t (f a) -- input collection
  -> m (R.Dynamic t (f b))
collectionEditor editDeleteWidget addWidget faTofb fbTofa fDyn = mdo
  editDeleteDiffMaybeEv <- editDeleteWidget (fbTofa <$> editDeleteInputDyn)
  addDiffMaybeEv <- fmap (fmap Just) <$> addWidget fDyn
  let inputFbBeh = faTofb <$> R.current fDyn 
      inputFbEv = faTofb <$> R.updated fDyn
      editDeleteAddDiffEv = R.leftmost [editDeleteDiffMaybeEv, addDiffMaybeEv] -- should this combine if same frame?
      newFEv = R.leftmost [inputFbEv, R.attachWith (flip applyDiff) (R.current curDyn) editDeleteAddDiffEv]
      newCollectionInputEv = R.leftmost [() <$ R.updated fDyn, () <$ addDiffMaybeEv]
  curDyn <- R.buildDynamic (R.sample inputFbBeh) newFEv -- always has current value
  editDeleteInputDyn <- R.buildDynamic (R.sample inputFbBeh) (R.tagPromptlyDyn curDyn newCollectionInputEv) -- updates to current value (in this frame) on adds or new input
  return curDyn

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
  -> (RC.Diff f b -> M.Map (Key f) T.Text -> M.Map (Key f) T.Text) -- function to adjust dropdown labels 
  -> (Key f -> R.Dynamic t a -> m (R.Event t b)) -- single element edit widget 
  -> R.Dynamic t (f a)
  -> m (R.Event t (RC.Diff f b))
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

updateKeyLabelMap :: (RC.Diffable f, RC.Key f ~ RC.Key (RC.Diff f), Ord (Key (Diff f))) => Proxy f -> RC.Diff f (Maybe a) -> M.Map (Key f) b -> M.Map (Key f) b
updateKeyLabelMap _ d x =
  let mapOfDeletes = M.filter isNothing . M.fromList . RC.toKeyValueList $ d
  in M.difference x mapOfDeletes
    
-- add a delete action to a widget that edits the (key/)value.  
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
  -> m (R.Event t (Maybe b)) -- 'Just b' if changed, 'Nothing' if deleted.
editWithDeleteButton editWidget attrs delButton visibleDyn k vDyn = mdo
  let widgetAttrs = (\x -> attrs <> if x then visibleCSS else hiddenCSS) <$> visibleDyn'
  (visibleDyn', outEv') <- RD.elDynAttr "div" widgetAttrs $ do
    editedEv <- RD.el "span" $ editWidget k vDyn
    delButtonEv <- RD.el "span" $ delButton
    let outEv = R.leftmost [Just <$> editedEv, Nothing <$ delButtonEv]  
    visDyn <- R.buildDynamic (R.sample $ R.current visibleDyn) $ (isJust <$> outEv) 
    return (visDyn, outEv)
  return outEv'

newItemEditorConfig :: (Show e, R.Reflex t) => ME.ModalEditorConfig t e a
newItemEditorConfig = RD.def
                      & ME.modalEditor_updateOutput .~ ME.OnOk
                      & ME.modalEditor_closeOnOk .~ True
                      & ME.modalEditor_openButton .~ const (ME.ButtonConfig "Add" M.empty (Just "fa fa-plus"))
                      & ME.modalEditor_XButton .~ Nothing
                      & ME.modalEditor_OkButton .~ flip (ME.disableAndDisplayIfError (T.pack . show)) (ME.ButtonConfig "OK" M.empty (Just "fa fa-check"))
                      & ME.modalEditor_CancelButton .~ const (ME.ButtonConfig "Cancel" M.empty (Just "fa fa-window-close"))

addNewItemWidget :: ( R.Reflex t
                    , RD.DomBuilder t m
                    , RD.PostBuild t m
                    , RD.MonadHold t m
                    , MonadFix m
                    , MonadWidgetExtraC t m
                    , Diffable f
                    , Show e
                    , Monoid e)
  => (R.Dynamic t (f a) -> m (R.Dynamic t (Either e (Key (Diff f),b)))) -- widget to edit an entry and, given the input collection, return the proper key. Left for invalid value.
  -> R.Dynamic t (f a)
  -> m (R.Event t (Diff f b))
addNewItemWidget editPairW mapDyn = mdo
  let modalEditW = const $ dynamicToWidgetResult <$> editPairW mapDyn
      blankInput = R.constDyn $ Left mempty
      pairEvToDiffMaybeEv pairEv = fromKeyValueList . pure <$> pairEv
  newPairEv <- ME.modalEditor_change <$> ME.modalEditorEither modalEditW blankInput newItemEditorConfig
  return $ pairEvToDiffMaybeEv newPairEv


-- one possible button widget
buttonNoSubmit :: RD.DomBuilder t m=>T.Text -> m (R.Event t ())
buttonNoSubmit t = (RD.domEvent RD.Click . fst) <$> RD.elAttr' "button" ("type" RD.=: "button") (RD.text t)
    
hiddenCSS :: M.Map T.Text T.Text
hiddenCSS  = "style" =: "display: none !important"
visibleCSS :: M.Map T.Text T.Text
visibleCSS = "style" =: "display: inline"

{-
editDeletable ::  ( RD.Adjustable t m
                  , RD.PostBuild t m
                  , RD.MonadHold t m
                  , MonadFix m
                  , RC.Mergeable f (Maybe b)
                  , EditableCollection f)
  => (f a -> f b)
  -> (RC.Key f -> R.Dynamic t a -> m (R.Event t (Maybe b))) -- function to edit (Just b) or Delete (Nothing)
  -> R.Dynamic t (f a) -- input collection
  -> m (R.Dynamic t (f b))
editDeletable faTofb widget fDyn = do
  editDeleteDiffEv <- ecListViewWithKey fDyn widget
  let newFEv = R.attachWith (flip applyDiff) (R.current $ fmap faTofb fDyn) editDeleteDiffEv 
  R.buildDynamic (R.sample . R.current $ fmap faTofb fDyn) newFEv     
-}
