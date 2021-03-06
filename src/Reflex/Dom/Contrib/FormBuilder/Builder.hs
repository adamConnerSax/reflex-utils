{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.Dom.Contrib.FormBuilder.Builder
       (
         module Reflex.Dom.Contrib.FormBuilder.DynValidation
       , module Reflex.Dom.Contrib.FormBuilder.Configuration
       , module Reflex.Dom.Contrib.FormBuilder.FormEditor
       , BuildForm
       , BuildEditor
       , buildFormToEditor
       , dynamicForm
       , dynamicFormOfDynamic
       , dynamicFormOfDynMaybe
       , dynamicFormOfFormValue
       , formWithSubmitAction
       , observeDynamic
       , observeWidget
       , observeFlow
       , fgvToForm
       , formToFGV
       , FormValidator
       , validateForm
       , validateEditor
       , FormBuilder(..)
       , gBuildFormValidated
       , gBuildEditorValidated
       , gBuildForm
       , buildFMDWrappedList
       , makeSubformData
       , SubformArgs (SubformArgs)
       , prismToSubformData
       , editorFromSubForms
       , actOnDBWidget
       , joinDynOfFormValues
       , FMDWrapped
       , buildVForm
       , editField
       , editValidatedField
       , readOnlyField
       , noFormField
       , labelForm
       , makeForm
       , unF
       , toReadOnly
       , VFormBuilderC
       , CollapsibleInitialState(..)
       , runForm
       , module ReflexExport
       , module BExport
       , liftF
       , liftE
       , liftTransform
       , liftRAction
       , liftAction
       , switchingForm
       , fieldSet
       , fRowDynAttr
       , fColDynAttr
       , attrs0
       , titleAttr
       , cssClassAttr
       , fAttrs
       ) where


import           Reflex.Dom.Contrib.Layout.Types              (CssClasses (..),
                                                               IsCssClass (..),
                                                               LayoutOrientation (..))

import           Reflex.Dom.Contrib.Widgets.WidgetResult      (WidgetResult,
                                                               constWidgetResult,
                                                               dynamicToWidgetResult,
                                                               dynamicWidgetResultToWidgetResult,
                                                               updatedWidgetResult,
                                                               widgetResultToDynamic,
                                                               wrDyn)

import           Reflex.Dom.Contrib.FormBuilder.Configuration
import           Reflex.Dom.Contrib.FormBuilder.DynValidation
import           Reflex.Dom.Contrib.FormBuilder.FormEditor

--import qualified Data.Dependent.Map                           as DM
import qualified Data.Dependent.Map                           as DM
import           DataBuilder                                  as BExport (FieldName,
                                                                          GBuilder (..),
                                                                          MDWrapped (..))
import qualified DataBuilder                                  as B
import           Generics.SOP.DMapUtilities                   (npSequenceViaDMap)

import           Reflex                                       as ReflexExport (PushM)
import qualified Reflex                                       as R
import qualified Reflex.Dom                                   as RD

import           Control.Lens                                 (Prism', has,
                                                               preview, review,
                                                               view)
import           Control.Monad.Fix                            (MonadFix)
import           Control.Monad.Morph
import           Control.Monad.Reader                         (MonadReader (..),
                                                               runReaderT)
import           Data.Functor.Compose                         (Compose (..))
import           Data.Functor.Identity                        (Identity)
import qualified Data.Map                                     as M
import           Data.Maybe                                   (fromMaybe,
                                                               isJust)
import           Data.Monoid                                  ((<>))
import qualified Data.Text                                    as T
import           Data.Validation                              (AccValidation (..))
--import           Language.Haskell.TH


-- Form is necessary because it has functor and applicative that are different from that of SFRW
-- NB: Form is *not* a Monad. So we do all the monadic widget building in (FRW t m a) and then wrap with makeForm
-- NB: the Form type is now in Editor but re-exported from here.
-- type Form t m a = Compose (FR t m) (FormValue t) a

type BuildForm t m a = FormValidator a -> Maybe FieldName -> FormValue t a -> Form t m a
type BuildEditor t m a = FormValidator a -> Maybe FieldName -> FormEditor t m a a

-- adapter for BuildForm (in Containers)
buildFormToEditor :: BuildForm t m a -> BuildEditor t m a
buildFormToEditor bf v = Editor . bf v

makeForm :: FRW t m a -> Form t m a
makeForm = Compose

unF :: Form t m a -> FRW t m a
unF = getCompose

fgvToForm :: Functor m => B.FGV (FR t m) (WidgetResult t) FValidation a -> Form t m a
fgvToForm = makeForm . fmap Compose . B.unFGV

formToFGV :: Functor m => Form t m a -> B.FGV (FR t m) (WidgetResult t) FValidation a
formToFGV = B.FGV . fmap getCompose . unF

type FormValidator a = B.Validator FValidation a

validateForm :: (Functor m, R.Reflex t) => FormValidator a -> Form t m a -> Form t m a
validateForm va = makeForm . fmap Compose . B.unFGV . B.validateFGV va . B.FGV . fmap getCompose . unF

validateEditor :: (R.Reflex t, Functor m) => FormValidator b -> FormEditor t m a b -> FormEditor t m a b
validateEditor v de = Editor $ validateForm v . runEditor de

formValueToGBuildInput :: R.Reflex t => FormValue t a -> B.GV (WidgetResult t) FValidation a
formValueToGBuildInput = B.GV . getCompose

distributeDMapOverWidgetResult :: forall t k. (R.Reflex t, DM.GCompare k) => DM.DMap k (WidgetResult t) -> WidgetResult t (DM.DMap k Identity)
distributeDMapOverWidgetResult = dynamicToWidgetResult . R.distributeDMapOverDynPure . DM.map (view wrDyn)

class (RD.DomBuilder t m, R.MonadHold t m, RD.PostBuild t m) => FormBuilder t m a where
  buildForm :: FormValidator a -> Maybe FieldName -> FormValue t a -> Form t m a
  default buildForm :: (B.GBuilderCS (FR t m) (WidgetResult t) FValidation a)
    => FormValidator a -> Maybe FieldName -> FormValue t a -> Form t m a
  buildForm va mFN = makeForm . fmap Compose . B.unFGV . B.gBuildValidatedCS (npSequenceViaDMap distributeDMapOverWidgetResult) va mFN . formValueToGBuildInput


-- helper function for using the genericBuilder in an instance rather than as the instance.  Useful for additional layout, etc.
gBuildFormValidated::( RD.DomBuilder t m
                     , RD.MonadHold t m
                     , RD.PostBuild t m
                     , B.GBuilderCS (FR t m) (WidgetResult t) FValidation a)
  => FormValidator a -> Maybe FieldName -> FormValue t a -> Form t m a
gBuildFormValidated va mFN = fgvToForm  . B.gBuildValidatedCS (npSequenceViaDMap distributeDMapOverWidgetResult) va mFN . formValueToGBuildInput

gBuildForm :: ( RD.DomBuilder t m
              , RD.MonadHold t m
              , RD.PostBuild t m
              , B.Validatable FValidation a
              , B.GBuilderCS (FR t m) (WidgetResult t) FValidation a)
  => Maybe FieldName -> FormValue t a -> Form t m a
gBuildForm = gBuildFormValidated B.validator


gBuildEditorValidated :: ( RD.DomBuilder t m
                         , RD.MonadHold t m
                         , RD.PostBuild t m
                         , B.GBuilderCS (FR t m) (WidgetResult t) FValidation a)
  => FormValidator a -> Maybe FieldName -> FormEditor t m a a
gBuildEditorValidated va mFN = Editor $ gBuildFormValidated va mFN

type VFormBuilderC t m a = (FormBuilder t m a, B.Validatable FValidation a)

buildVForm :: VFormBuilderC t m a => Maybe FieldName ->FormValue t a -> Form t m a
buildVForm = buildForm B.validator

editField :: VFormBuilderC t m a => Maybe FieldName -> FormEditor t m a a
editField mFN = Editor $ buildVForm mFN

editValidatedField :: FormBuilder t m a => FormValidator a -> Maybe FieldName -> FormEditor t m a a
editValidatedField v mFN = Editor $ buildForm v mFN

noFormField :: (Applicative m, R.Reflex t) => FormEditor t m a a
noFormField = Editor $ Compose . pure

readOnlyField :: VFormBuilderC t m a => Maybe FieldName -> FormEditor t m a a
readOnlyField mFN =  Editor $ toReadOnly . buildVForm mFN


labelForm :: Monad m => T.Text -> T.Text -> T.Text -> CssClasses -> Form t m a -> Form t m a
labelForm label title placeHolder classes =
  let labelCfg = LabelConfig label ("class" RD.=: toCssString classes)
      inputCfg = InputElementConfig (Just placeHolder) (Just title) (Just labelCfg)
  in liftF (setInputConfig inputCfg)

-- utilities to use the sum-split facilities from dataBuilder

buildFMDWrappedList::( RD.DomBuilder t m
                     , RD.MonadHold t m
                     , RD.PostBuild t m
                     , B.Generic a
                     , B.HasDatatypeInfo a
                     , B.All2 (B.And (B.Builder (FR t m) (WidgetResult t) FValidation) (B.Validatable FValidation)) (B.Code a))
  => Maybe FieldName -> FormValue t a -> [FMDWrapped t m a]
buildFMDWrappedList mFN = B.buildMDWrappedList mFN . formValueToGBuildInput


makeSubformData :: (R.Reflex t, Functor m)
  => Maybe FieldName -> (a -> Bool) -> FormEditor t m a a -> B.ConName -> FormValue t a -> FMDWrapped t m a
makeSubformData mFN isThis subFormEditor name fva =
  let w =  B.FGV . fmap getCompose . unF . runEditor subFormEditor . Compose . B.unGV
      gva = formValueToGBuildInput fva
  in B.makeMDWrapped mFN isThis w name gva

data SubformArgs a b = SubformArgs (FormValidator b) B.ConName (Prism' a b)

prismToSubformData :: (R.Reflex t, Functor m, FormBuilder t m b)
  => Maybe FieldName -> SubformArgs a b -> FormValue t a -> FMDWrapped t m a
prismToSubformData mFN (SubformArgs vb name p) =
  let mappedEditor = Editor $ fmap (review p) . buildForm vb mFN . maybeMapFormValue (preview p)
  in makeSubformData mFN (has p) mappedEditor name

editorFromSubForms ::  (R.Reflex t, Functor m, B.Buildable (FR t m) (WidgetResult t) FValidation)
  => FormValidator a -> [FormValue t a -> FMDWrapped t m a] -> FormEditor t m a a
editorFromSubForms v subForms = Editor $ \fva -> validateForm v $ makeForm $ fmap Compose . B.unFGV . B.bSum $ fmap ($ fva) subForms

actOnDBWidget :: Functor m => (FRW t m a -> FRW t m a) -> B.FGV (FR t m) (WidgetResult t) FValidation a -> B.FGV (FR t m) (WidgetResult t) FValidation a
actOnDBWidget f = B.FGV . fmap getCompose . f . fmap Compose . B.unFGV

toReadOnly :: Monad m => Form t m a -> Form t m a
toReadOnly form = makeForm . local setToObserve $ unF form

-- We need this, but why?
instance (RD.DomBuilder t m, FormBuilder t m a) => B.Builder (FR t m) (WidgetResult t) FValidation a where
  buildValidated va mFN = B.FGV . fmap getCompose . unF . buildForm va mFN . Compose . B.unGV


runForm :: Monad m => FormConfiguration t m -> Form t m a -> m (FormValue t a)
runForm cfg sfra = runForm' cfg sfra return

runForm' :: Monad m => FormConfiguration t m -> Form t m a -> (FormValue t a -> m b) -> m b
runForm' cfg fra f = runReaderT (fWrapper $ unF fra >>= lift . f) cfg

joinDynOfFormValues :: R.Reflex t => R.Dynamic t (FormValue t a) -> FormValue t a
joinDynOfFormValues = Compose . dynamicWidgetResultToWidgetResult . fmap getCompose

switchingForm :: (RD.DomBuilder t m, R.MonadHold t m) => (a -> Form t m b) -> a -> R.Event t a -> Form t m b
switchingForm widgetGetter widgetHolder0 newWidgetHolderEv = makeForm $ do
  cfg <- ask
  let f = runForm cfg . widgetGetter
  lift $ joinDynOfFormValues <$> RD.widgetHold (f widgetHolder0) (fmap f newWidgetHolderEv)

dynamicForm :: (RD.DomBuilder t m, VFormBuilderC t m a) => FormConfiguration t m -> Maybe a -> m (FormValue t a)
dynamicForm cfg ma = runForm cfg $ buildVForm Nothing (Compose $ constWidgetResult $ maybeToFV ma)

dynamicFormOfFormValue :: (RD.DomBuilder t m, VFormBuilderC t m a) => FormConfiguration t m -> FormValue t a -> m (FormValue t a)
dynamicFormOfFormValue cfg fva = runForm cfg $ buildVForm Nothing fva

dynamicFormOfDynamic :: (RD.DomBuilder t m, VFormBuilderC t m a) => FormConfiguration t m -> R.Dynamic t a -> m (FormValue t a)
dynamicFormOfDynamic cfg da = runForm cfg $ buildVForm Nothing (Compose . dynamicToWidgetResult . fmap AccSuccess $ da)

dynamicFormOfDynMaybe :: (RD.DomBuilder t m, VFormBuilderC t m a) => FormConfiguration t m -> DynMaybe t a -> m (FormValue t a)
dynamicFormOfDynMaybe cfg dma = runForm cfg $ buildVForm Nothing (Compose . dynamicToWidgetResult . fmap maybeToFV . getCompose $ dma)

--TODO: is attachPromptlyDynWithMaybe the right thing here?
formWithSubmitAction :: ( RD.DomBuilder t m
                        , VFormBuilderC t m a)
  => FormConfiguration t m
  -> Maybe a -- initial values
  -> m (RD.Event t ()) -- submit control
  -> m (RD.Event t a)
formWithSubmitAction cfg ma submitWidget = do
  let f fra = do
        submitEv <- submitWidget
        return $ RD.attachPromptlyDynWithMaybe const (widgetResultToDynamic $ avToMaybe <$> getCompose fra) submitEv -- fires when control does but only if form entries are valid
  runForm' cfg (buildVForm Nothing (Compose $ constWidgetResult $ maybeToFV ma)) f


observeDynamic :: (RD.DomBuilder t m, VFormBuilderC t m a) => FormConfiguration t m -> R.Dynamic t a -> m (FormValue t a)
observeDynamic cfg aDyn = dynamicFormOfDynamic (setToObserve cfg) aDyn

observeWidget :: (RD.DomBuilder t m, VFormBuilderC t m a) => FormConfiguration t m -> m a -> m (FormValue t a)
observeWidget cfg wa =
  runForm (setToObserve cfg) . makeForm $ lift wa >>= unF . buildVForm Nothing . constFormValue

observeFlow :: ( RD.DomBuilder t m
               , R.MonadHold t m
               , MonadFix m
               , VFormBuilderC t m a
               , VFormBuilderC t m b)
  => FormConfiguration t m -> (a -> m b) -> a -> m (FormValue t b)
observeFlow cfg flow initialA =
  runForm cfg . makeForm  $ do
    let initialWidget = flow initialA
        obF = observeWidget cfg
    fva <- unF $ buildVForm Nothing (constFormValue initialA) -- FormValue t a
    dwb <- lift $ R.foldDynMaybe (\ma _ -> flow <$> ma) initialWidget (avToMaybe <$> updatedWidgetResult (getCompose fva)) -- Dynamic t (m b)
    lift $ joinDynOfFormValues <$> RD.widgetHold (obF initialWidget) (obF <$> R.updated dwb)

liftF :: FLayoutF t m -> Form t m a -> Form t m a
liftF f = makeForm . f . unF

liftE :: FLayoutF t m -> Editor x (Form t m) a b -> Editor x (Form t m) a b
liftE f = transformEditor id (liftF f) -- Editor $ liftF f . runEditor e

liftTransform :: Monad m => (forall b. m b -> m b) -> Form t m a -> Form t m a
liftTransform f = liftF (liftLF f)

liftRAction :: Monad m => FR t m b -> Form t m a -> Form t m a
liftRAction ac form = makeForm $ ac >> unF form

liftAction :: Monad m => m b -> Form t m a -> Form t m a
liftAction ac = liftRAction (lift ac)

fRowDynAttr :: Monad m => DynAttrs t -> FLayoutF t m
fRowDynAttr attrsDyn = fItem . fDynamicDiv attrsDyn . fOrient LayoutHorizontal

fColDynAttr :: Monad m => DynAttrs t -> FLayoutF t m
fColDynAttr attrsDyn =   fItem . fDynamicDiv attrsDyn . fOrient LayoutHorizontal

fieldSet :: RD.DomBuilder t m => T.Text -> FLayoutF t m
fieldSet legendText ra = RD.el "fieldset" $ do
    lift $ RD.el "legend" $ RD.text legendText
    ra

attrs0 :: R.Reflex t => DynAttrs t
attrs0 = R.constDyn mempty

titleAttr :: T.Text -> M.Map T.Text T.Text
titleAttr x = M.fromList [("title",x),("placeholder",x)]

cssClassAttr :: CssClasses -> M.Map T.Text T.Text
cssClassAttr x = "class" RD.=: toCssString x

fAttrs :: (RD.MonadHold t m, RD.DomBuilder t m, MonadFix m)
  => FormValue t a
  -> Maybe FieldName
  -> Maybe T.Text
  -> FR t m (R.Dynamic t (M.Map T.Text T.Text))
fAttrs fra mFN mTypeS = fAttrs' fra mFN mTypeS (CssClasses [])

fAttrs' :: (RD.MonadHold t m, R.Reflex t, MonadFix m)
  => FormValue t a
  -> Maybe FieldName
  -> Maybe T.Text
  -> CssClasses
  -> FR t m (R.Dynamic t (M.Map T.Text T.Text))
fAttrs' fra mFN mTypeS fixedCss = do
  validClasses <- validDataClasses
  invalidClasses <- invalidDataClasses
  let title = componentTitle mFN mTypeS
      validAttrs = titleAttr title <> cssClassAttr (validClasses <> fixedCss)
      invalidAttrs = titleAttr title <> cssClassAttr (invalidClasses <> fixedCss)
      f (AccSuccess _) = True
      f (AccFailure _) = False
      validDyn = widgetResultToDynamic $ f <$> getCompose fra
  return . RD.ffor validDyn $ \x -> if x then validAttrs else invalidAttrs

componentTitle :: Maybe FieldName -> Maybe T.Text -> T.Text
componentTitle mFN mType =
  let fnS = maybe "" T.pack  mFN
      tnS = fromMaybe "" mType
  in if isJust mFN && isJust mType then fnS <> "::" <> tnS else fnS <> tnS


instance (RD.DomBuilder t m, R.MonadHold t m, RD.PostBuild t m) => B.Buildable (FR t m) (WidgetResult t) FValidation where
  bFail msg = B.FGV . fmap getCompose $ do
    failF <- failureF . _builderFunctions <$> ask
    failF $ T.pack msg

-- NB: This could use a rewrite given buildDynamic and WidgetResult
  bSum mwWidgets = B.FGV . fmap getCompose $ do
    postbuild <- RD.getPostBuild
    let mapHasDefault = R.fmapMaybe (\x -> if x then Just () else Nothing)
        mapValue      = fmap Compose . B.unFGV
        f (MDWrapped isConWR (conName, mFN) fgvWidget) =
          let isConDyn = widgetResultToDynamic isConWR
          in (conName, T.pack <$> mFN, mapHasDefault (R.leftmost [R.updated isConDyn, R.tag (R.current isConDyn) postbuild]), mapValue fgvWidget)
        constrList = f <$> mwWidgets
    sF <- sumF . _builderFunctions <$> ask
    sF constrList


type FMDWrapped t m a = B.MDWrapped (FR t m) (WidgetResult t) FValidation a


{-
deriveSFRowBuilder::Name -> Q [Dec]
deriveSFRowBuilder typeName =
  [d|instance SimpleFormC e t m=>Builder (SimpleFormR e t m) $(conT typeName) where
       buildA md Nothing  = liftF (itemL . layoutHoriz) ($(B.handleNothingL typeName) md)
       buildA md (Just x) = liftF (itemL . layoutHoriz) ($(B.handleJustL typeName) md x)|]


deriveSFColBuilder::Name -> Q [Dec]
deriveSFColBuilder typeName =
  [d|instance SimpleFormC e t m=>Builder (SimpleFormR e t m) $(conT typeName) where
       buildA md Nothing  = liftF (itemL . layoutVert) ($(B.handleNothingL typeName) md)
       buildA md (Just x) = liftF (itemL . layoutVert) ($(B.handleJustL typeName) md x)|]
-}
