{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module Reflex.Dom.Contrib.FormBuilder.Builder
       (
         module Reflex.Dom.Contrib.FormBuilder.DynValidation
       , module Reflex.Dom.Contrib.FormBuilder.Configuration
       , dynamicForm
       , formWithSubmitAction
       , observeDynamic
       , observeWidget
       , observeFlow
       , Form
       , fgvToForm
       , formToFGV
       , FormValidator
       , validateForm
       , FormBuilder(..)
       , FMDWrapped
       , buildForm'
       , makeForm
       , unF
       , toReadOnly
       , VFormBuilderC
--     , FormC
       , CollapsibleInitialState(..)
       , runForm
       , module ReflexExport
       , module BExport
       , module GSOP
       , liftF
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
--       , deriveSFRowBuilder
--       , deriveSFColBuilder
       ) where


import           Reflex.Dom.Contrib.Layout.Types             (CssClass,
                                                              CssClasses (..),
                                                              IsCssClass (..),
                                                              LayoutOrientation (..),
                                                              emptyCss)

import           Reflex.Dom.Contrib.DynamicUtils (dynAsEv,traceDynAsEv)
import           Reflex.Dom.Contrib.FormBuilder.Configuration
import           Reflex.Dom.Contrib.FormBuilder.DynValidation

import           DataBuilder                                 as BExport (Builder (..),
                                                                         FieldName,
                                                                         GBuilder (..),
                                                                         MDWrapped (..),
                                                                         buildAFromConList)
import qualified DataBuilder                                 as B
import           DataBuilder.GenericSOP                      as GSOP (Generic, HasDatatypeInfo,
                                                                      deriveGeneric)

import           Reflex                                      as ReflexExport (PushM)
import qualified Reflex                                      as R
import qualified Reflex.Dom                                  as RD

import           Control.Arrow                               ((&&&))
import           Control.Lens                                (view)
import           Control.Monad                               (join)
import           Control.Monad.Fix                           (MonadFix)
import           Control.Monad.Morph
import           Control.Monad.Reader                        (MonadReader (..),
                                                              runReaderT)
import           Data.Functor.Compose                        (Compose (..))
import           Data.These                                  (These(..))
import           Data.Align                                  (align)
import qualified Data.Map                                    as M
import           Data.Maybe                                  (fromMaybe, isJust,fromJust)
import           Data.Monoid                                 ((<>))
import qualified Data.Text                                   as T
import           Data.Validation                             (AccValidation (..))
import           Language.Haskell.TH




-- This is necessary because this functor and applicative are different from that of SFRW
type Form t m a = Compose (FR t m) (DynValidation t) a

--type FormC t m = (RD.DomBuilder t m, R.MonadHold t m -})

makeForm::FRW t m a -> Form t m a
makeForm = Compose

unF::Form t m a->FRW t m a
unF = getCompose

fgvToForm::Functor m=>B.FGV (FR t m) (R.Dynamic t) FValidation a -> Form t m a
fgvToForm = makeForm . fmap DynValidation . B.unFGV

formToFGV::Functor m=>Form t m a -> B.FGV (FR t m) (R.Dynamic t) FValidation a
formToFGV = B.FGV . fmap unDynValidation . unF

--type FormBuilderC t m a = (B.Builder (SFR t m) (R.Dynamic t) (FValidation) a, B.Validatable (FValidation) a)

type FormValidator a = B.Validator (FValidation) a

validateForm::(Functor m, R.Reflex t)=>FormValidator a->Form t m a->Form t m a
validateForm va = makeForm . fmap DynValidation . B.unFGV . B.validateFGV va . B.FGV . fmap unDynValidation . unF


class (RD.DomBuilder t m, R.MonadHold t m, RD.PostBuild t m) => FormBuilder t m a where
  buildForm::FormValidator a->Maybe FieldName->Maybe (R.Dynamic t a)->Form t m a
  default buildForm::(GBuilder (FR t m) (R.Dynamic t) (FValidation) a)
                   =>FormValidator a->Maybe FieldName->Maybe (R.Dynamic t a)->Form t m a
  buildForm va mFN = makeForm . fmap DynValidation . B.unFGV . gBuildValidated va mFN

type VFormBuilderC t m a = (FormBuilder t m a, B.Validatable (FValidation) a)

buildForm'::VFormBuilderC t m a=>Maybe FieldName->Maybe (R.Dynamic t a)->Form t m a
buildForm' = buildForm B.validator

toReadOnly::Monad m=>Form t m a -> Form t m a
toReadOnly form = makeForm . local setToObserve $ unF form 

instance (RD.DomBuilder t m, FormBuilder t m a)=>B.Builder (FR t m) (R.Dynamic t) (FValidation) a where
  buildValidated va mFN = B.FGV . fmap unDynValidation . unF . buildForm va mFN

runForm::Monad m=>FormConfiguration t m->Form t m a->m (DynValidation t a)
runForm cfg sfra = runForm' cfg sfra return

runForm'::Monad m=>FormConfiguration t m->Form t m a->(DynValidation t a->m b)->m b
runForm' cfg fra f = runReaderT (fWrapper $ unF fra >>= lift . f) cfg

switchingForm::(RD.DomBuilder t m, R.MonadHold t m)=>(a->Form t m b)->a->R.Event t a->Form t m b
switchingForm widgetGetter widgetHolder0 newWidgetHolderEv = makeForm $ do
  cfg <- ask
  let f = runForm cfg . widgetGetter
  lift $ joinDynOfDynValidation <$> RD.widgetHold (f widgetHolder0) (fmap f newWidgetHolderEv)


dynamicForm::(RD.DomBuilder t m, VFormBuilderC t m a)=>FormConfiguration t m->Maybe a->m (DynValidation t a)
dynamicForm cfg ma = runForm cfg $ buildForm' Nothing (R.constDyn <$> ma)


--TODO: is attachPromptlyDynWithMaybe the right thing here?
formWithSubmitAction::(RD.DomBuilder t m, VFormBuilderC t m a)
                    =>FormConfiguration t m
                    -> Maybe a -- initial values
                    -> m (RD.Event t ()) -- submit control
                    -> m (RD.Event t a)
formWithSubmitAction cfg ma submitWidget = do
  let f dva = do
        submitEv <- submitWidget
        return $ RD.attachPromptlyDynWithMaybe const (avToMaybe <$> (unDynValidation dva)) submitEv -- fires when control does but only if form entries are valid
  runForm' cfg (buildForm' Nothing (R.constDyn <$> ma)) f


observeDynamic::(RD.DomBuilder t m, VFormBuilderC t m a)=>FormConfiguration t m->R.Dynamic t a->m (DynValidation t a)
observeDynamic cfg aDyn = do
  aEv <- traceDynAsEv (const "observeDynamic") aDyn
  aDyn' <- R.holdDyn Nothing (Just <$> aEv) 
  runForm (setToObserve cfg) $ buildForm' Nothing (Just (fromJust <$> aDyn')) 


observeWidget::(RD.DomBuilder t m ,VFormBuilderC t m a)=>FormConfiguration t m->m a->m (DynValidation t a)
observeWidget cfg wa =
  runForm (setToObserve cfg) . makeForm $ lift wa >>= unF . buildForm' Nothing . Just . R.constDyn


observeFlow::(RD.DomBuilder t m
             , R.MonadHold t m
             , MonadFix m
             , VFormBuilderC t m a
             , VFormBuilderC t m b)
           =>FormConfiguration t m->(a->m b)->a->m (DynValidation t b)
observeFlow cfg flow initialA =
  runForm cfg . makeForm  $ do
    let initialWidget = flow initialA
        obF = observeWidget cfg
    dva <- (unF $ buildForm' Nothing (Just $ R.constDyn initialA)) -- DynValidation t a
    dwb <- lift $ R.foldDynMaybe (\ma _ -> flow <$> ma) initialWidget (avToMaybe <$> R.updated (unDynValidation dva)) -- Dynamic t (m b)
    lift $ joinDynOfDynValidation <$> RD.widgetHold (obF initialWidget) (obF <$> R.updated dwb)

liftF::FLayoutF t m->Form t m a->Form t m a
liftF f = makeForm . f . unF

liftTransform::Monad m=>(forall b.m b->m b)->Form t m a->Form t m a
liftTransform f = liftF (liftLF f)

liftRAction::Monad m=>FR t m b->Form t m a->Form t m a
liftRAction ac form = makeForm $ ac >> unF form

liftAction::Monad m=>m b->Form t m a->Form t m a
liftAction ac = liftRAction (lift ac)

fRowDynAttr::Monad m=>DynAttrs t->FLayoutF t m
fRowDynAttr attrsDyn = fItem . fDynamicDiv attrsDyn . fOrient LayoutHorizontal

fColDynAttr::Monad m=>DynAttrs t->FLayoutF t m
fColDynAttr attrsDyn =   fItem . fDynamicDiv attrsDyn . fOrient LayoutHorizontal

fieldSet::RD.DomBuilder t m=>T.Text->FLayoutF t m
fieldSet legendText ra = RD.el "fieldset" $ do
    lift $ RD.el "legend" $ RD.text legendText
    ra

attrs0::R.Reflex t=>DynAttrs t
attrs0 = R.constDyn mempty

titleAttr::T.Text->M.Map T.Text T.Text
titleAttr x = M.fromList [("title",x),("placeholder",x)]

cssClassAttr::CssClasses->M.Map T.Text T.Text
cssClassAttr x = "class" RD.=: toCssString x

fAttrs::(RD.MonadHold t m, RD.DomBuilder t m,MonadFix m)
  =>DynValidation t a
  ->Maybe FieldName
  ->Maybe T.Text
  ->FR t m (R.Dynamic t (M.Map T.Text T.Text))
fAttrs mDyn mFN mTypeS = fAttrs' mDyn mFN mTypeS (CssClasses [])

fAttrs'::(RD.MonadHold t m, R.Reflex t,MonadFix m)
  =>DynValidation t a
  ->Maybe FieldName
  ->Maybe T.Text
  ->CssClasses
  ->FR t m (R.Dynamic t (M.Map T.Text T.Text))
fAttrs' mDyn mFN mTypeS fixedCss = do
  validClasses <- validDataClasses
  invalidClasses <- invalidDataClasses
  let title = componentTitle mFN mTypeS
      validAttrs = titleAttr title <> cssClassAttr (validClasses <> fixedCss)
      invalidAttrs = titleAttr title <> cssClassAttr (invalidClasses <> fixedCss)
      f (AccSuccess _) = True
      f (AccFailure _) = False
      validDyn = f <$> (unDynValidation mDyn)
  return . RD.ffor validDyn $ \x -> if x then validAttrs else invalidAttrs

componentTitle::Maybe FieldName->Maybe T.Text->T.Text
componentTitle mFN mType =
  let fnS = maybe "" T.pack  mFN
      tnS = fromMaybe "" mType
  in if isJust mFN && isJust mType then fnS <> "::" <> tnS else fnS <> tnS


instance (RD.DomBuilder t m, R.MonadHold t m, RD.PostBuild t m)=> B.Buildable (FR t m) (R.Dynamic t) (FValidation) where
  bFail msg = B.FGV . fmap unDynValidation $ do
    failF <- failureF . _builderFunctions <$> ask
    failF $ T.pack msg

  bSum mwWidgets = B.FGV . fmap unDynValidation $ do
    let constrList = map ((fst . B.metadata) &&& (fmap DynValidation . B.unFGV . B.value)) mwWidgets
        defCon = case filter B.hasDefault mwWidgets of
          [] -> Nothing
          (x:_) -> Just . fst $ B.metadata x
    sF <- sumF . _builderFunctions <$> ask
    sF constrList defCon

  bDistributeList = R.distributeListOverDynPure

  bCollapse dynFGV = B.FGV $ do
    postbuild <- RD.getPostBuild
    let uncomposed = B.unFGV <$> dynFGV
        newWidgetEv = R.updated uncomposed
        startingWidgetEv = R.tag (R.current uncomposed) postbuild
    join <$> RD.widgetHold (return $ R.constDyn $ AccFailure [FNothing]) (R.leftmost [newWidgetEv, startingWidgetEv])

{-
  bCollapseAndSum mdwListDyn = B.FGV $ do    -- FIXME, empty list case
    postbuild <- getPostBuild
    cF <- chooserF . _builderFunctions <$> ask
    let toKeyValue mdw = (fst . metadata $ mdw, mdw)
        mdwMapDyn = M.fromList . toKeyValue <$> mdwListDyn
        conNamesDyn = R.uniqDyn $ M.keys <$> mdwMapDyn -- should never change.  An invariant I wish I could express in the types
        mInputSelDyn = R.uniqDyn $ headMaybe . fst . filter (hasDefault . snd) . toList <$> mdwMapDyn  -- Dynamic t (Maybe ConName)
        inputSelDyn = R.zipDynWith (\mi cns -> fromMaybe (head cns) mi) mInputSelDyn conNamesDyn
        chooserEv = cF conNamesDyn <$> R.updated inputSelDyn -- Event t (FRW t m B.ConName)
    vselDyn <- RD.widgetHold (return $ unDynValidation dynValidationNothing) (chooserEv) -- Dynamic t (FValidation B.ConName)
    let hiddenCSS  = "style" RD.=: "display: none"
        visibleCSS = "style" RD.=: "display: inline"
        dAttrs cn = accValidation (const hiddenCss) (==cn) <$> visDyn -- Dynamic t (Map Text Text)
        diffOnlyKeyChanges olds news = flip M.mapMaybe (align olds news) $ \case
          This _ -> Just Nothing
          These _ _ -> Nothing
          That new -> Just $ Just new
    rec sentVals :: Dynamic t (Map B.ConName (FMDWrapped t m a)) <- foldDyn applyMap Map.empty changeVals
        let changeVals :: Event t (Map B.ConName (Maybe (FMDWrapped t m a)))
            changeVals = RD.attachWith diffOnlyKeyChanges (R.current sentVals) $ R.leftmost
                         [ R.updated mdwMapDyn
                         , R.tag (current mdwMapDyn) postBuild 
                         ]
        

-}


    

type FMDWrapped t m a = B.MDWrapped (FR t m) (R.Dynamic t) (FValidation) a


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
