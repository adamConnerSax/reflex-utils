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
  ) where

--import Reflex.Dom.Contrib.DynamicUtils (dynAsEv, dynPlusEvent)
--import Reflex.Dom.Contrib.EventUtils (fanBool)
--import Reflex.Dom.Contrib.Widgets.WidgetResult (WrappedWidgetResult, unsafeBuildWrappedWidgetResult)
import qualified Reflex.Dom.Contrib.Widgets.ModalEditor as ME

-- for the collection functions and constraints they need
import           Reflex.Collections.Collections as RC

import           Reflex            (Dynamic, Event, Reflex, attachWithMaybe,
                                    leftmost, never)
import           Reflex.Dynamic    (constDyn, current, tagPromptlyDyn, updated)
--import Reflex.Dom (widgetHold,dropdown,DropdownConfig(..))
import qualified Reflex            as R
import qualified Reflex.Dom        as RD

import           Data.Traversable (sequenceA) 
import           Control.Lens      (makeLenses)
import           Control.Monad (join)
import           Control.Monad.Fix (MonadFix)
--import           Data.Default

import qualified Data.Map          as M
import qualified Data.Text         as T
import qualified Data.IntMap as IM
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as S
import qualified Data.Array as A
import qualified Data.Tree as T

import           Data.Kind (Type)

class (KeyedCollection f, Diffable f) => EditableCollection (f :: Type -> Type) where
  editValues :: (RD.Adjustable t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
    => (a -> b) -- to map the input type to the output type. Often "Just" or "Right"
    -> ((RC.Key f -> R.Dynamic t a -> m (R.Dynamic t b))) -- widget for editing one value, possibly with visible key
    -> Dynamic t (f a) -- input collection
    -> m (R.Dynamic t (f b)) 

  editDeletable :: (RD.Adjustable t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
    => (a -> b) -- to map the input type to the output type. Often "Just" or "Right"
    -> ((RC.Key f -> R.Dynamic t a -> m (R.Dynamic t b))) -- widget for editing one value, possibly with visible key
    -> Dynamic t (f a) -- input collection
    -> m (R.Event t (Diff f b)) -- edits and deletes

{-
  editStructure :: (RD.Adjustable t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
    => ((RC.Key f -> R.Dynamic t a -> m (R.Dynamic t b))) -- widget for editing one value, possibly with visible key
    -> (R.Dynamic t (f a) -> m (R.Event t (RC.Diff b))) -- widget for adding an item or items. Might respond to changes in the input collection.
    -> ((RC.Key f -> R.Dynamic t a -> m (R.Dynamic t b)) -> (Dynamic t (f a) -> m (R.Event t (RC.Diff b))))  -- widget taking single item widget and adding delete 
    -> Dynamic t (f a)
    -> m (Dynamic t (f b))
-}

instance Ord k => EditableCollection (M.Map k) where
  editValues _ initial widget = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget

instance (Hashable k, Ord k) => EditableCollection (HM.HashMap k) where
  editValues _ initial widget = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget

instance EditableCollection IM.IntMap where
  editValues _ initial widget = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget

instance EditableCollection [] where
  editValues _ initial widget = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget

instance EditableCollection S.Seq where
  editValues _ initial widget = join . fmap distributeOverDynPure <$> RC.listWithKey initial widget

instance (A.Ix k, Enum k, Bounded k) => EditableCollection (A.Array k) where
  editValues aTob initial widget = RC.simplifyDynMaybe aTob (flip RC.listWithKeyMaybe widget) initial

instance EditableCollection T.Tree where
  editValues aTob initial widget = RC.simplifyDynMaybe aTob (flip RC.listWithKeyMaybe widget) initial     

-- helper for the case when you want the output to update only on input change or a valid edit
-- e.g., validOnly (flip (editValues Right) widget) initial
validOnly :: (Reflex t, R.MonadHold t m, Traversable f, Applicative g)
  => (forall x. g x -> Maybe x) -> (Dynamic t (f a) -> m (Dynamic t (f (g a)))) -> Dynamic t (f a) -> m (Dynamic t (f a))
validOnly toMaybe editCollectionF initial = do
  edited <- editCollectionF initial
  let validEditEv = R.fmapMaybe toMaybe $ updated $ (fmap sequenceA) edited
  R.buildDynamic (R.sample $ R.current initial) $ R.leftmost [R.updated initial, validEditEv] 


editDeletableGeneral :: ( RD.Adjustable t m
                        , RD.PostBuild t m
                        , RD.MonadHold t m
                        , MonadFix m)
  
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
