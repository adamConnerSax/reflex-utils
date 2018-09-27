{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Module with machinery for building reflex-dom forms/dynamic editors from containers.
--
-- This module also contains FormBuilder instances for various basic container types (Map, [], IntMap, Sequence, HashMap, Tree, Array)
-- These are based on mapping the container to something map-like (Map, IntMap, HashMap)
module Reflex.Dom.Contrib.FormBuilder.Instances.Containers
  (
    -- * Container form builders
    formCollectionEditor
  , formCollectionValueEditor
  , DisplayCollection (..)
  ) where


import qualified Reflex.Collections.Collections                 as RC
import           Reflex.Dom.Contrib.Widgets.EditableCollection  (DisplayCollection (..))
import qualified Reflex.Dom.Contrib.Widgets.EditableCollection  as EC
import qualified Reflex.Dom.Contrib.Widgets.WidgetResult        as WR

import qualified DataBuilder                                    as B
import           Reflex.Dom.Contrib.FormBuilder.Builder         (FieldName,
                                                                 Form,
                                                                 FormBuilder (buildForm),
                                                                 FormValidator,
                                                                 FormValue,
                                                                 VFormBuilderC,
                                                                 avToMaybe,
                                                                 buildVForm,
                                                                 constFormValue,
                                                                 dynamicToFormValue,
                                                                 fFill, fItem,
                                                                 fRow,
                                                                 formValueNothing,
                                                                 makeForm,
                                                                 toReadOnly,
                                                                 unF,
                                                                 validateForm)
import           Reflex.Dom.Contrib.FormBuilder.DynValidation   (FValidation, FormError (FInvalid, FNothing),
                                                                 FormErrors,
                                                                 avToEither,
                                                                 maybeToFV,
                                                                 mergeAccValidation)
import           Reflex.Dom.Contrib.FormBuilder.Instances.Basic (FormInstanceC)
import           Reflex.Dom.Contrib.Layout.Types                (LayoutDirection (..))
import           Reflex.Dom.Contrib.ReflexConstraints           (MonadWidgetExtraC)


-- reflex imports
import qualified Reflex                                         as R
import qualified Reflex.Dom                                     as RD
import qualified Reflex.Dom.Contrib.Widgets.Common              as RDC

import           Control.Monad                                  (join)
import           Control.Monad.Fix                              (MonadFix)
import           Data.Functor.Compose                           (Compose (Compose, getCompose))
import           Data.Proxy                                     (Proxy (..))
import qualified Data.Text                                      as T
import           Data.Validation                                (AccValidation (..))
import           Text.Read                                      (readMaybe)

-- imports only to make instances
import qualified Data.Array                                     as A
import           Data.Hashable                                  (Hashable)
import qualified Data.HashMap.Lazy                              as HML
import qualified Data.IntMap                                    as IM
import qualified Data.Map                                       as M
import qualified Data.Sequence                                  as Seq
import qualified Data.Tree                                      as Tree

type VFormBuilderBoth t m a b = (VFormBuilderC t m a, VFormBuilderC t m b)

-- simple widgets for default container instances
-- the instances are simple enough (using formCollectionEditor) so it'd be easy to sub in specific widgets if you need them.
showKeyEditVal :: (FormInstanceC t m, VFormBuilderBoth t m k v) => k -> R.Dynamic t v -> Form t m v
showKeyEditVal k vDyn = makeForm $ do
  let showKey x = toReadOnly $ buildVForm Nothing (constFormValue x)
  fRow $ do
    _<- fItem . fFill LayoutRight . unF $ showKey k
    fItem . fFill LayoutLeft . unF $ buildVForm Nothing (dynamicToFormValue vDyn)

hideKeyEditVal :: (FormInstanceC t m, VFormBuilderC t m v) => k -> R.Dynamic t v -> Form t m v
hideKeyEditVal _ vDyn = makeForm $ do
  fItem . unF $ buildVForm Nothing (dynamicToFormValue vDyn)

newItemWidget :: (EC.EditableCollection f, FormInstanceC t m, VFormBuilderC t m (EC.NewItem f a))
  => Proxy f -> Proxy a -> Form t m (EC.NewItem f a)
newItemWidget _ _ = buildVForm Nothing formValueNothing
--

instance (Ord k, B.Validatable FValidation k, B.Validatable FValidation a) => B.Validatable FValidation (M.Map k a) where
  validator = M.traverseWithKey (\k a -> mergeAccValidation (B.validator a <$ B.validator k))

instance (Ord k, FormInstanceC t m, VFormBuilderBoth t m k a) => FormBuilder t m (M.Map k a) where
  buildForm va mFN mFV =
    let newItemW =  newItemWidget (Proxy :: Proxy (M.Map k)) (Proxy :: Proxy a)
    in validateForm va $ formCollectionEditor EC.DisplayAll showKeyEditVal newItemW mFV

instance (Ord k, Hashable k, B.Validatable FValidation k, B.Validatable FValidation a) => B.Validatable FValidation (HML.HashMap k a) where
  validator = HML.traverseWithKey (\k a -> mergeAccValidation (B.validator a <$ B.validator k))

instance (Ord k, Hashable k, FormInstanceC t m, VFormBuilderBoth t m k a) => FormBuilder t m (HML.HashMap k a) where
  buildForm va mFN hmFV =
    let newItemW =  newItemWidget (Proxy :: Proxy (HML.HashMap k)) (Proxy :: Proxy a)
    in validateForm va $ formCollectionEditor EC.DisplayAll showKeyEditVal newItemW hmFV

instance B.Validatable FValidation a => B.Validatable FValidation (IM.IntMap a) where
  validator = traverse B.validator

instance (FormInstanceC t m, VFormBuilderBoth t m Int a) => FormBuilder t m (IM.IntMap a) where
  buildForm va mFN imFV =
    let newItemW =  newItemWidget (Proxy :: Proxy IM.IntMap) (Proxy :: Proxy a)
    in validateForm va $ formCollectionEditor EC.DisplayAll showKeyEditVal newItemW imFV

instance B.Validatable FValidation a => B.Validatable FValidation [a] where
  validator = traverse B.validator

instance (FormInstanceC t m, VFormBuilderC t m a) => FormBuilder t m [a] where
  buildForm va mFN lFV =
    let newItemW =  newItemWidget (Proxy :: Proxy []) (Proxy :: Proxy a)
    in validateForm va $ formCollectionEditor EC.DisplayAll hideKeyEditVal newItemW lFV

instance B.Validatable FValidation a => B.Validatable FValidation (Seq.Seq a) where
  validator = traverse B.validator

instance (FormInstanceC t m, VFormBuilderBoth t m Int a) => FormBuilder t m (Seq.Seq a) where
  buildForm va mFN sFV =
    let newItemW = newItemWidget (Proxy :: Proxy Seq.Seq) (Proxy :: Proxy a)
    in validateForm va $ formCollectionEditor EC.DisplayAll hideKeyEditVal newItemW sFV

instance (A.Ix k, B.Validatable FValidation a) => B.Validatable FValidation (A.Array k a) where
  validator = traverse B.validator

instance (A.Ix k, Enum k, Bounded k, FormInstanceC t m, VFormBuilderBoth t m k a) => FormBuilder t m (A.Array k a) where
  buildForm va mFN aFV = validateForm va $ formCollectionValueEditor EC.DisplayAll showKeyEditVal aFV

instance B.Validatable FValidation a => B.Validatable FValidation (Tree.Tree a) where
  validator = traverse B.validator

instance (FormInstanceC t m, VFormBuilderC t m a) => FormBuilder t m (Tree.Tree a) where
  buildForm va mFN tFV = validateForm va $ formCollectionValueEditor EC.DisplayAll hideKeyEditVal tFV


-- I don't love the widgetHold here, so
-- TODO: try and fix the widgetHold via the Maybe version of things?
-- but it's also not so bad because the eitherDyn means we will only redo everything when we change from Invalid to Valid inputs.
-- and that would require a redraw of everything anyway!
-- TODO: Re-write in an event based way so we can honor the promise of WidgetResult
-- might require a re-write in EditableCollection as well.
formCollectionEditor :: forall t m f a. ( RD.DomBuilder t m
                                        , RD.PostBuild t m
                                        , FormInstanceC t m
                                        , R.Adjustable t m
                                        , R.MonadHold t m
                                        , MonadFix m
                                        , RC.Mergeable f
                                        , EC.EditableCollection f
                                        , Ord (RC.Key f)
                                        , Monoid (f a)
                                        , RC.Key f ~ RC.Key (RC.KeyValueSet f))
  => EC.DisplayCollection t (RC.Key f) -- use a dropdown or show entire collection
  -> (RC.Key f -> R.Dynamic t a -> Form t m a) -- display and edit existing
  -> Form t m (EC.NewItem f a) -- input a new one
  -> FormValue t (f a)
  -> Form t m (f a)
formCollectionEditor display editWidget newItemWidget fvFa = makeForm $ do
  postBuild <- RD.getPostBuild
  let editWidget' k vDyn = R.fmapMaybe avToMaybe . WR.updatedWidgetResult . getCompose <$> unF (editWidget k vDyn) -- m (R.Event t a)
      editAndDeleteWidget = EC.reappearingEditWithDeleteButton editWidget' M.empty (EC.buttonNoSubmit "-")
      editDeletableWidget = case display of
        EC.DisplayAll -> flip EC.ecListViewWithKey editAndDeleteWidget
        EC.DisplayEach ddAttrs toText -> EC.selectEditValues ddAttrs toText (EC.updateKeyLabelMap (Proxy :: Proxy f)) editAndDeleteWidget
      pf = Proxy :: Proxy f
      addNewWidget = (EC.addNewItemWidgetModal pf $ EC.newKeyValueWidget pf avToEither (WR.widgetResultToDynamic . getCompose <$> unF newItemWidget)) . fmap RC.toKeyValueSet
      collWidget fDyn = fmap AccSuccess <$> EC.collectionEditorWR editDeletableWidget addNewWidget id id fDyn
      invalidWidget = return . WR.dynamicToWidgetResult . fmap AccFailure
  davFa <-  R.eitherDyn . fmap avToEither . WR.widgetResultToDynamic . getCompose $ fvFa
  let (errsDynEv, fDynEv) = R.fanEither $ R.leftmost [R.updated davFa, R.tag (R.current davFa) postBuild]
      isFNothing formErrs = if formErrs == [FNothing] then Just () else Nothing
  fNothingEv <- R.fmapMaybe isFNothing . R.updated . join <$> R.holdDyn (R.constDyn $ [FNothing]) errsDynEv -- add empty container case
  let fNothingFaDynEv = R.constDyn mempty <$ fNothingEv
      initialWidget = invalidWidget $ R.constDyn [FInvalid "Initial Widget"]
      updatedWidgetEv = R.leftmost [collWidget <$> fNothingFaDynEv, invalidWidget <$> errsDynEv, collWidget <$> fDynEv]
  Compose . WR.dynamicWidgetResultToWidgetResult  <$> (RD.widgetHold initialWidget updatedWidgetEv)


-- simpler widget for cases where we can't edit the structure.  Like Array and, for now, Tree.
formCollectionValueEditor :: forall t m f a. ( RD.DomBuilder t m
                                             , RD.PostBuild t m
                                             , FormInstanceC t m
                                             , R.Adjustable t m
                                             , R.MonadHold t m
                                             , MonadFix m
                                             , RC.Mergeable f
                                             , EC.EditableCollection f
                                             , Ord (RC.Key f))
  => EC.DisplayCollection t (RC.Key f) -- use a dropdown or show entire collection
  -> (RC.Key f -> R.Dynamic t a -> Form t m a) -- display and edit existing
  -> FormValue t (f a)
  -> Form t m (f a)
formCollectionValueEditor display editWidget fvFa = makeForm $ do
  postBuild <- RD.getPostBuild
  let editWidget' k vDyn = WR.widgetResultToDynamic . fmap avToMaybe . getCompose <$> unF (editWidget k vDyn) -- m (R.Dynamic t (Maybe a))
      collWidget fDyn = fmap AccSuccess <$> EC.simpleCollectionValueEditor display editWidget' fDyn
      invalidWidget = return . fmap AccFailure
  davFa <- R.eitherDyn . fmap avToEither . WR.widgetResultToDynamic . getCompose $ fvFa
  let (errsDynEv, fDynEv) = R.fanEither $ R.leftmost [R.updated davFa, R.tag (R.current davFa) postBuild]
  Compose . WR.dynamicToWidgetResult . join <$> (RD.widgetHold (invalidWidget $ R.constDyn [FInvalid "Initial Widget"]) $ R.leftmost [invalidWidget <$> errsDynEv, collWidget <$> fDynEv])


