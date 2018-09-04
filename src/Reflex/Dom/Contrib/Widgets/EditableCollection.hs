{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Reflex.Dom.Contrib.Widgets.EditableCollection
  (
    EditableCollection(..)
  , editDeletable
  , editStructure
  , editWithDeleteButton
  , newItemWidget
  , buttonNoSubmit
  , validOnly
  ) where

--import Reflex.Dom.Contrib.DynamicUtils (dynAsEv, dynPlusEvent)
--import Reflex.Dom.Contrib.EventUtils (fanBool)
import           Reflex.Dom.Contrib.ReflexConstraints (MonadWidgetExtraC)
import           Reflex.Dom.Contrib.Widgets.WidgetResult (dynamicToWidgetResult)
import qualified Reflex.Dom.Contrib.Widgets.ModalEditor as ME

-- for the collection functions and constraints they need
import           Reflex.Collections.Collections as RC

import           Reflex            (Dynamic, Event, Reflex, attachWithMaybe,
                                    leftmost, never)
import           Reflex.Dom        ((=:))                 
import           Reflex.Dynamic    (constDyn, current, tagPromptlyDyn, updated)
--import Reflex.Dom (widgetHold,dropdown,DropdownConfig(..))
import qualified Reflex            as R
import qualified Reflex.Dom        as RD

import           Data.Traversable (sequenceA) 
import           Control.Lens      (makeLenses,(&),(.~))
import           Control.Monad (join)
import           Control.Monad.Fix (MonadFix)
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Kind (Type)
--import           Data.Default

import qualified Data.Map          as M
import qualified Data.Text         as T
import qualified Data.IntMap as IM
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as S
import qualified Data.Array as A
import qualified Data.Tree as T



class (KeyedCollection f, Diffable f) => EditableCollection (f :: Type -> Type) where
  editValues :: (RD.Adjustable t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
    => (a -> b) -- to map the input type to the output type. Often "Just" or "Right"
    -> ((RC.Key f -> R.Dynamic t a -> m (R.Dynamic t b))) -- widget for editing one value, possibly with visible key
    -> Dynamic t (f a) -- input collection
    -> m (R.Dynamic t (f b))

  ecListViewWithKey :: (RD.Adjustable t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
    => R.Dynamic t (f v) -> (Key f -> R.Dynamic t v -> m (R.Event t a)) -> m (R.Event t (Diff f a))

  newKey :: KeyedCollection f => Dynamic t (f a) -> Dynamic t (Maybe (Key f)) -- if the key is determined by the collection
    
instance Ord k => EditableCollection (M.Map k) where
  editValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  newKey _ = R.constDyn Nothing

instance (Hashable k, Ord k) => EditableCollection (HM.HashMap k) where
  editValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  newKey _ = R.constDyn Nothing

instance EditableCollection IM.IntMap where
  editValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  newKey _ = R.constDyn Nothing
  
instance EditableCollection [] where
  editValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  newKey = fmap (Just . length) 
  
instance EditableCollection S.Seq where
  editValues _ widget initial = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget
  ecListViewWithKey = RC.listViewWithKey
  newKey = fmap (Just . S.length) 
  
instance (A.Ix k, Enum k, Bounded k) => EditableCollection (A.Array k) where
  editValues aTob widget initial = RC.simplifyDynMaybe aTob (flip RC.listWithKeyMaybe widget) initial
  ecListViewWithKey = RC.listViewWithKeyMaybe

instance EditableCollection T.Tree where
  editValues aTob widget initial = RC.simplifyDynMaybe aTob (flip RC.listWithKeyMaybe widget) initial     
  ecListViewWithKey = RC.listViewWithKeyMaybe
  
-- helper for the case when you want the output to update only on input change or a valid edit
-- e.g., validOnly (flip (editValues Right) widget) initial
validOnly :: (Reflex t, R.MonadHold t m, Traversable f, Applicative g)
  => (forall x. g x -> Maybe x) -> (Dynamic t (f a) -> m (Dynamic t (f (g a)))) -> Dynamic t (f a) -> m (Dynamic t (f a))
validOnly toMaybe editCollectionF initial = do
  edited <- editCollectionF initial
  let validEditEv = R.fmapMaybe toMaybe $ updated $ (fmap sequenceA) edited
  R.buildDynamic (R.sample $ R.current initial) $ R.leftmost [R.updated initial, validEditEv] 

editDeletable ::  ( RD.Adjustable t m
                  , RD.PostBuild t m
                  , RD.MonadHold t m
                  , MonadFix m
                  , RC.Mergeable f (Maybe b)
                  , EditableCollection f)
  => (f a -> f b)
  -> (RC.Key f -> R.Dynamic t a -> m (R.Event t (Maybe b))) -- function to edit (Just b) or Delete (Nothing)
  -> Dynamic t (f a) -- input collection
  -> m (R.Dynamic t (f b))
editDeletable faTofb widget fDyn = do
  diffEv <- ecListViewWithKey fDyn widget
  let newFEv = R.attachWith (flip applyDiff) (R.current $ fmap faTofb fDyn) diffEv 
  R.buildDynamic (R.sample . R.current $ fmap faTofb fDyn) newFEv     



{-
There are 3 sources of change:
1. A change to the input (fDyn)
2. An edit or delete of an existing element (editDeleteDiffMaybeEv)
3. The addition of a new element (addDiffMaybeEv)
which must be reflected in 2 places:
1. The collection as its rendered in the dom via editDeleteWidget 
2. The output of this widget (curDyn)
-}
editStructure :: ( RD.Adjustable t m
                 , RD.PostBuild t m
                 , RD.MonadHold t m                 
                 , MonadFix m
                 , RD.DomBuilder t m
                 , RC.Mergeable f (Maybe b)
                 , EditableCollection f)                 
  => (R.Dynamic t (f a) -> m (R.Event t (Diff f (Maybe b)))) -- edit/Delete
  -> (R.Dynamic t (f a) -> m (R.Event t (Diff f b))) --  function to make add item widget.  Only fires on valid add
  -> (R.Dynamic t (f a) -> Dynamic t (M.Map T.Text T.Text)) -- attrs can depend on collection
  -> (f a -> f b) -- we need this to have a starting point for the output dynamics
  -> (f b -> f a) -- we need this to feed the changes back in to the collection functions
  -> Dynamic t (f a) -- input collection
  -> m (R.Dynamic t (f b))
editStructure editDeleteWidget addWidget attrsF faTofb fbTofa fDyn = RD.elDynAttr "div" (attrsF fDyn) $ mdo
  editDeleteDiffMaybeEv <- editDeleteWidget (fbTofa <$> editDeleteInputDyn)
  addDiffMaybeEv <- fmap (fmap Just) <$> addWidget fDyn
  let inputfbEv = faTofb <$> R.updated fDyn
      diffEv = R.leftmost [editDeleteDiffMaybeEv, addDiffMaybeEv] -- should this combine if same frame?
      newFEv = R.leftmost [inputfbEv, R.attachWith (flip applyDiff) (R.current curDyn) diffEv]
      newWidgetInputEv = leftmost [() <$ R.updated fDyn, () <$ addDiffMaybeEv]
  curDyn <- R.buildDynamic (R.sample . R.current $ faTofb <$> fDyn) newFEv -- always has current value
  editDeleteInputDyn <- R.buildDynamic (R.sample . R.current $ faTofb <$> fDyn) (R.tagDyn curDyn newWidgetInputEv) -- updates to current value (in this frame) on adds or new input
  return curDyn
  
-- add a delete action to a widget that edits the (key/)value.  
editWithDeleteButton :: ( R.Reflex t
                        , R.MonadHold t m
                        , R.PostBuild t m
                        , RD.DomBuilder t m
                        , MonadFix m)
  => (k -> R.Dynamic t a -> m (R.Event t b)) -- widget to edit one element, fires only on valid change
  -> M.Map T.Text T.Text -- attrs for the div surrounding the widget with button
  -> m (Event t ()) -- delete button widget
  -> R.Dynamic t Bool -- visibility of widget
  -> k -- key in collection
  -> R.Dynamic t a -- input element
  -> m (R.Event t (Maybe b)) -- 'Just b' if changed, 'Nothing' if deleted.
editWithDeleteButton editWidget attrs delButton visibleDyn k vDyn = mdo
  let widgetAttrs = (\x -> attrs <> if x then visibleCSS else hiddenCSS) <$> visibleDyn'
  (visibleDyn', outEv) <- RD.elDynAttr "div" widgetAttrs $ do
    editedEv <- RD.el "span" $ editWidget k vDyn
    delButtonEv <- RD.el "span" $ delButton
    let outEv = R.leftmost [Just <$> editedEv, Nothing <$ delButtonEv]  
    visDyn <- R.buildDynamic (R.sample $ current visibleDyn) $ (isJust <$> outEv) 
    return (visDyn, outEv)
  return outEv


newItemEditorConfig :: (Show e, R.Reflex t) => ME.ModalEditorConfig t e a
newItemEditorConfig = RD.def
                      & ME.modalEditor_updateOutput .~ ME.OnOk
                      & ME.modalEditor_closeOnOk .~ True
                      & ME.modalEditor_openButton .~ const (ME.ButtonConfig "Add" M.empty (Just "fa fa-plus"))
                      & ME.modalEditor_XButton .~ Nothing
                      & ME.modalEditor_OkButton .~ flip (ME.disableAndDisplayIfError (T.pack . show)) (ME.ButtonConfig "OK" M.empty (Just "fa fa-check"))
                      & ME.modalEditor_CancelButton .~ const (ME.ButtonConfig "Cancel" M.empty (Just "fa fa-window-close"))

newItemWidget :: ( R.Reflex t
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
newItemWidget editPairW mapDyn = mdo
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

  --NB: I see why tagPromtlyDyn is required for pairWidgetEv (so when the new one is drawn, it sees the updated map) but not quite why
-- that doesn't lead to a cycle.
-- Should we do something better with the WidgetResults in updateMapDyn?  Get a Map k (Event) and then mergeMap?
buildLBAddDelete :: ()
  => (k -> Dynamic t v -> m (Dynamic t (Deletable v)))
  -> 
  -> Dynamic t (f v)
  -> m (Dynamic t (f v))
buildLBAddDelete (MapLike to from diffMapF) (MapElemWidgets eW nWF) mFN fvfa = makeForm $ fCol $ do
  let eW' k v0 vEv = R.holdDyn v0 vEv >>= editAndDeleteElemWidget eW (R.constDyn True) k
      mapDyn0 = widgetResultToDynamic $ fmap fValMapToMap . getCompose $ to <$> fvfa
      mapDynAV0 = fmap AccSuccess <$> mapDyn0
      diffMap' avOld new = case avOld of
        AccSuccess old -> diffMapF old new
        AccFailure _   -> Just <$> new
  newInputMapEv <- dynAsEv mapDyn0
  rec updateMapDyn <- fItem $ LHF.listWithKeyShallowDiffLHFMap lhfEmptyMap diffMapEv eW' -- Dynamic t (Map k (WidgetResult t (Maybe (FValidation v))))
      insertDiffEv <- fRow $ newItemWidget nWF mapDyn
      let mapAfterInsertEv = R.attachWith (flip LHF.lhfMapApplyDiff) (R.current mapDyn) (fmap (fmap AccSuccess) <$> insertDiffEv)
          newInputDiffEv = R.attachWith diffMap' (R.current $ sequenceA <$> mapDyn) newInputMapEv --newInputMapEv -- Event t (Map k (Maybe v))
          diffMapEv = R.leftmost [newInputDiffEv, insertDiffEv]
          mapEditsFVEv = R.updated . join $ LHF.distributeLHFMapOverDynPure . fmap widgetResultToDynamic <$> updateMapDyn -- Event t (Map k (Maybe (FValidation v))) ??
          editedMapEv = R.attachWith (flip LHF.lhfMapApplyDiff) (R.current mapDyn) mapEditsFVEv -- Event t (Map k (FValidation v))
      mapDyn <- dynPlusEvent mapDynAV0 editedMapEv
  wr <- fmap (fmap from . sequenceA) <$> (buildWidgetResult mapDynAV0 $ R.leftmost [mapAfterInsertEv, editedMapEv])
  return $ Compose wr


editAndDeleteElemWidget :: (Reflex t)
  => (k -> Dynamic t v -> m (AccValidation v))
  -> (Dynamic t k -> m (AccValidation k))
  -> R.Dynamic t Bool
  -> k
  -> Dynamic t v
  -> Dynamic t (AccValidation (k, v))
editAndDeleteElemWidget eW kW visibleDyn k vDyn = mdo
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> visibleDyn'
  (visibleDyn', outWR') <- RD.elDynAttr "div" widgetAttrs . fRow $ do
    resWR <- getCompose <$> (fItem $ eW k vDyn) -- WidgetResult t (FValidation v)
    delButtonEv <- fItem $ fFill LayoutLeft $ buttonNoSubmit' "-"
    visDyn <-  dynPlusEvent visibleDyn $ R.leftmost
               [
                 False <$ delButtonEv -- delete button pressed, so hide
               , True <$ (R.updated $ widgetResultToDynamic $ resWR) --resEv -- value updated so make sure it's visible (in case of re-use of deleted key)
               ]
    outWR <- buildWidgetResult (Just . AccSuccess <$> vDyn) $ R.leftmost [Just <$> updatedWidgetResult resWR, Nothing <$ delButtonEv]
    return (visDyn,outWR)
  return outWR'



newItemEditorConfig :: R.Reflex t => ME.ModalEditorConfig t FormErrors a
newItemEditorConfig = RD.def
                      & ME.modalEditor_updateOutput .~ MW.OnOk
                      & ME.modalEditor_closeOnOk .~ True
                      & ME.modalEditor_openButton .~ const (MW.ButtonConfig "Add" M.empty (Just "fa fa-plus"))
                      & ME.modalEditor_XButton .~ Nothing
                      & ME.modalEditor_OkButton .~ flip (MW.disableAndDisplayIfError printFormErrors) (MW.ButtonConfig "OK" M.empty (Just "fa fa-check"))
                      & ME.modalEditor_CancelButton .~ const (MW.ButtonConfig "Cancel" M.empty (Just "fa fa-window-close"))

newItemWidget :: ContainerForm t m g k v
  => (R.Dynamic t (g (FValidation v)) -> FRW t m (k,v))
  -> R.Dynamic t (g (FValidation v))
  -> FR t m (R.Event t (g (Maybe v)))
newItemWidget editPairW mapDyn = mdo
  let modalEditW = const $ fmap avToEither . getCompose <$> editPairW mapDyn
      blankInput = R.constDyn $ Left [FNothing]
      pairEvToDiffEv pairEv = fmap Just . uncurry lhfMapSingleton <$> pairEv
  newPairEv <- MW.modalEditor_change <$> MW.modalEditorEither modalEditW blankInput newItemEditorConfig
  return $ pairEvToDiffEv newPairEv


-- something using selectView
buildSelectViewer :: ( FormInstanceC t m
                     , Traversable g
                     , LHFMap g
                     , LHFMapKey g ~ k
                     , VFormBuilderBoth t m k v)
  => LabelStrategy k v
  -> MapElemWidgets g t m k v
  -> LBBuildF' g t m k v
buildSelectViewer labelStrategy (MapElemWidgets eW nWF) mFN dgvIn = fCol $ mdo
  -- chooser widget with dropdown
  (editedMapEv, deleteDiffEv) <- fRow $ do
    (maybeSelDyn, editedMapEv') <- fRow $ selectWidget labelStrategy eW dgvForDD

  -- we make the button invisible if the container is empty
    let isEmptyContainerDyn = LHF.lhfMapNull <$> widgetResultToDynamic gvWR
        delButtonAttrs = bool visibleCSS hiddenCSS <$> isEmptyContainerDyn
    deleteDiffEv' <- fItem $ RD.elDynAttr "div" delButtonAttrs $ do
      deleteButtonEv <- fCol $ buttonNoSubmit' "Delete"
      let deleteEv = R.fmapMaybe id $ R.tag (R.current maybeSelDyn) deleteButtonEv  -- Event t k, only fires if there is a current selection
      return $ flip lhfMapSingleton Nothing <$> deleteEv -- Event t (k, Nothing)
    return (editedMapEv', deleteDiffEv')

  insertDiffEv <- fItem $ newItemWidget nWF (fmap AccSuccess <$> widgetResultToDynamic gvWR)
  let insertDeleteDiffEv = R.leftmost [insertDiffEv, deleteDiffEv]
      mapAfterInsertDeleteEv = R.attachWith (flip LHF.lhfMapApplyDiff) (currentWidgetResult gvWR) insertDeleteDiffEv

  dgvForDD <- dynPlusEvent dgvIn mapAfterInsertDeleteEv -- dropdown causes edits so don't feed them back in.
  gvWR <- buildWidgetResult dgvIn $ R.leftmost [editedMapEv, mapAfterInsertDeleteEv] -- authoritative value for (g v)
  return $ Compose $ AccSuccess <$> gvWR

selectWidget :: ( FormInstanceC t m
                , Traversable g
                , LHFMap g
                , LHFMapKey g ~ k
                , VFormBuilderBoth t m k v
                )
  => LabelStrategy k v
  -> ElemWidget t m k v
  -> R.Dynamic t (g v)
  -> FR t m (R.Dynamic t (Maybe k), R.Event t (g v))
selectWidget labelStrategy eW dgv = do
  let keyLabelMap = labelLHFMap labelStrategy <$> dgv
      config = SD.SafeDropdownConfig R.never $ R.constDyn ("size" =: "1")
  sdd <- fItem $ SD.safeDropdown Nothing keyLabelMap config
  sddNullEv <- R.holdUniqDyn (isNothing <$> view SD.safeDropdown_value sdd) >>= dynAsEv -- does this need dynAsEv?
  let (notNullEv, nullEv) = fanBool sddNullEv
      nullWidgetEv = return R.never <$ nullEv
      safeKeyEv = R.tagPromptlyDyn (head . LHF.lhfMapKeys <$> keyLabelMap) notNullEv
      selWidget safeKey = do
        selDyn <- R.holdDyn safeKey (R.fmapMaybe id $ view SD.safeDropdown_change sdd)
        LHF.selectViewListWithKeyLHFMap selDyn dgv (toSVLWKWidget eW)  -- NB: this map doesn't need updating from edits or deletes

  editEvDyn <- fItem $ RD.widgetHold (return R.never) $ R.leftmost [nullWidgetEv, selWidget <$> safeKeyEv]
  mapEditEvBeh <- R.hold R.never (R.updated editEvDyn)
  let mapEditEv = leftWhenNotRight (R.switch mapEditEvBeh) (R.updated dgv) -- updated inputs are not edits.  But those events are mixed in because dgv is input to selectWidget
      editDiffEv = fmap avToMaybe . uncurry lhfMapSingleton <$> mapEditEv
      editedMapEv = R.attachWith (flip LHF.lhfMapApplyDiff) (R.current dgv) editDiffEv  -- has edits; these don't get fed back in.
  return (view SD.safeDropdown_value sdd, editedMapEv)

buildLBWithSelect :: ContainerForm t m g k v
  => LabelStrategy k v
  -> MapLike f g v
  -> MapElemWidgets g t m k v
  -> Maybe FieldName
  -> FormValue t (f v)
  -> Form t m (f v)
buildLBWithSelect labelStrategy (MapLike to from _) widgets mFN fvfa =  makeForm $ do
  let mapDyn0 = widgetResultToDynamic $ fmap fValMapToMap . getCompose $ to <$> fvfa
  fmap from <$> buildSelectViewer labelStrategy widgets mFN mapDyn0

buildLBWithSelectEditOnly :: ContainerForm t m g k v
  => LabelStrategy k v
  -> MapLike f g v
  -> MapElemWidgets g t m k v
  -> Maybe FieldName
  -> FormValue t (f v)
  -> Form t m (f v)
buildLBWithSelectEditOnly labelStrategy (MapLike to from _) (MapElemWidgets eW _)  mFN fvfv =  makeForm $ do
  let dgv = widgetResultToDynamic $ fmap fValMapToMap . getCompose $ to <$> fvfv
  (_, editedMapEv) <- selectWidget labelStrategy eW dgv
  res <- buildWidgetResult dgv editedMapEv
--  res <- R.buildDynamic (R.sample $ R.current dgv) $ R.leftmost [R.updated dgv, editedMapEv]
  return . Compose $ AccSuccess . from <$> res
-}
