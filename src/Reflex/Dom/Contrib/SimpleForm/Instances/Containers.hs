{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TupleSections         #-}
module Reflex.Dom.Contrib.SimpleForm.Instances.Containers () where

-- All the basic (primitive types, tuples, etc.) are in here
import Reflex.Dom.Contrib.ReflexConstraints (MonadWidgetExtraC)
import Reflex.Dom.Contrib.SimpleForm.Instances.Basic (SimpleFormInstanceC,VBuilderC)
import Reflex.Dom.Contrib.SimpleForm.Instances.Extras (buildValidatedDynamic)
import Reflex.Dom.Contrib.SimpleForm.Builder
import Reflex.Dom.Contrib.SimpleForm.DynValidation (accValidation)
import Reflex.Dom.Contrib.Layout.Types (LayoutOrientation(..))

-- reflex imports
import qualified Reflex as R 
import qualified Reflex.Dom as RD
import Reflex.Dom ((=:))

import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (lift)
import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad.Morph (hoist)
import Control.Lens (view)
import qualified Data.Text as T
import Data.Validation (AccValidation(..))

-- imports only to make instances
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe,catMaybes,fromJust)
import Data.Monoid ((<>))
-- my libs
import qualified DataBuilder as B


-- Container instances
-- Editing/Appendability requires that the container be isomorphic to something traversable, but traversable in the (key,value) pairs for maps.
-- and the ability to make an empty traversable and insert new items/pairs.
-- Deletability requires some way of knowing which element to delete *in the traversable container rather than the original*.
-- If this uses an index, it might require some state as the traversable container is rendered.

-- I'd prefer these as classes but I can't make all the types work.  I end up with open type families and injectivity issues.
-- So, instead, I carry the dictionaries around as arguments.  That works too.
data CRepI (fa :: *) (gb :: *) = CRepI { toRep::fa -> gb, fromRep::gb -> fa }

idRep::CRepI fa fa
idRep = CRepI id id

data SFAppendableI (fa :: * ) (g :: * -> *) (b :: *) = SFAppendableI
  {
    cRep::CRepI fa (g b)
  , emptyT::g b
  , insertB::b->fa->fa
  , sizeFa::fa->Int
  }

toT::SFAppendableI fa g b->(fa -> g b)
toT = toRep . cRep

fromT::SFAppendableI fa g b->(g b -> fa)
fromT = fromRep . cRep

data SFDeletableI (g :: * -> *) (b :: *) (k :: *) (s :: *) = SFDeletableI
  {
    getKey::b -> s -> k
  , initialS::s
  , updateS::s->s
  , delete::k -> g b -> g b
  }

-- This helps insure the types in Appendable and Deletable line up for any particular instance
data SFAdjustableI fa g b k s= SFAdjustableI { sfAI::SFAppendableI fa g b, sfDI::SFDeletableI g b k s }


buildAdjustableContainer::(SimpleFormInstanceC t m, VBuilderC t m b,Traversable g)
                          =>SFAdjustableI fa g b k s->B.Validator (DynValidation t) fa->Maybe FieldName->Maybe fa->SimpleFormR t m fa
buildAdjustableContainer sfAdj va mFN mfa = B.validateFV va . makeSimpleFormR  $ do
  fType <- getFormType
  case fType of
    ObserveOnly ->  buildReadOnlyContainer (cRep . sfAI $ sfAdj) mFN mfa
    Interactive ->  buildSFContainer (sfAI sfAdj) (buildDeletable (sfDI sfAdj)) mFN mfa

{-
-- default behavior of button in a form is to submit the form
-- This creates a button but without that default
buttonNoSubmit :: forall t m .RD.DomBuilder t m => T.Text -> m (RD.Event t ())
buttonNoSubmit t = do
  let pd::RD.EventSpec (RD.DomBuilderSpace m) RD.EventResult -> RD.EventSpec (RD.DomBuilderSpace m) RD.EventResult
      pd x =  RD.addEventSpecFlags (Proxy::Proxy (RD.DomBuilderSpace m)) RD.Click (const RD.preventDefault) x
      pdCfg::RD.ElementConfig RD.EventResult t m -> RD.ElementConfig RD.EventResult t m
      pdCfg x = RD.elementConfig_eventSpec %~ pd $ x
  (e, _) <- RD.element "button" (pdCfg def) $ RD.text t
  return $ RD.domEvent RD.Click e

clickableLabel::RD.DomBuilder t m=>T.Text -> m (RD.Event t ())
clickableLabel t = do
  (e,_) <- RD.element "label" def $ RD.text t
  return $ RD.domEvent RD.Click e

checkBoxClicker::(RD.PostBuild t m, RD.DomBuilder t m) => T.Text -> m (RD.Event t ())
checkBoxClicker _ = fmap void (RD._checkbox_change <$> RD.checkbox False def)
-}

buttonNoSubmit'::RD.DomBuilder t m=>T.Text -> m (RD.Event t ())
buttonNoSubmit' t = (RD.domEvent RD.Click . fst) <$> RD.elAttr' "button" ("type" RD.=: "button") (RD.text t)
  
  
containerActionButton::(RD.PostBuild t m, RD.DomBuilder t m)=>T.Text -> m (RD.Event t ())
containerActionButton = buttonNoSubmit'

buildReadOnlyContainer::(SimpleFormInstanceC t m, VBuilderC t m b,Traversable g)=>CRepI fa (g b)->BuildF t m fa
buildReadOnlyContainer = buildTraversableSFA  


listAppend::a->[a]->[a]
listAppend a as = as ++ [a]

listDeleteAt::Int->[a]->[a]
listDeleteAt n as = take n as ++ drop (n+1) as

listSFA::SFAppendableI [a] [] a
listSFA = SFAppendableI idRep [] listAppend L.length

listSFD::SFDeletableI [] a Int Int
listSFD = SFDeletableI (\_ n->n) 0 (+1) listDeleteAt

instance (SimpleFormInstanceC t m,VBuilderC t m a)=>B.Builder (SFR t m) (DynValidation t) [a] where
  buildValidated = buildAdjustableContainer (SFAdjustableI listSFA listSFD)


mapSFA::Ord k=>SFAppendableI (M.Map k v) (M.Map k) (k,v)
mapSFA = SFAppendableI (CRepI (M.mapWithKey (\k v->(k,v))) (\m->M.fromList $ snd <$> M.toList m)) mempty (\(k,x) m->M.insert k x m) M.size

mapSFD::Ord k=>SFDeletableI (M.Map k) (k,v) k ()
mapSFD = SFDeletableI (\(k,_) _ ->k) () id M.delete


instance (R.Reflex t,B.Validatable (DynValidation t) a, B.Validatable (DynValidation t) b)=>B.Validatable (DynValidation t) (a,b) where
  validate (a,b) =
    let g::B.Validatable (DynValidation t) c=>c->R.Dynamic t (AccValidation SimpleFormErrors c)
        g = unDynValidation . B.validate
        f ava avb = (,) <$> ava <*> avb
    in DynValidation $ f <$> g a <*> g b
    
instance (SimpleFormInstanceC t m, Ord k, VBuilderC t m k, VBuilderC t m a,Show k)=>B.Builder (SFR t m) (DynValidation t) (M.Map k a) where
--  buildValidated = buildAdjustableContainer (SFAdjustableI mapSFA mapSFD)
  buildValidated va mFN ma = B.validateFV va . makeSimpleFormR $ buildLBEditableMap mFN ma
  
intMapSFA::SFAppendableI (IM.IntMap v) IM.IntMap (IM.Key,v)
intMapSFA = SFAppendableI (CRepI (IM.mapWithKey (\k v->(k,v))) (\m->IM.fromList $ snd <$> IM.toList m)) mempty (\(k,x) m->IM.insert k x m) IM.size

intMapSFD::SFDeletableI IM.IntMap (IM.Key,v) IM.Key ()
intMapSFD = SFDeletableI (\(k,_) _ ->k) () id IM.delete

instance (SimpleFormInstanceC t m, VBuilderC t m IM.Key, VBuilderC t m a)=>B.Builder (SFR t m) (DynValidation t) (IM.IntMap a) where
  buildValidated = buildAdjustableContainer (SFAdjustableI intMapSFA intMapSFD)

seqSFA::SFAppendableI (Seq.Seq a) Seq.Seq a
seqSFA = SFAppendableI idRep Seq.empty (flip (Seq.|>)) Seq.length

seqSFD::SFDeletableI Seq.Seq a Int Int
seqSFD = SFDeletableI (\_ index->index) 0 (+1) (\n bs -> Seq.take n bs Seq.>< Seq.drop (n+1) bs)

instance (SimpleFormInstanceC t m,VBuilderC t m a)=>B.Builder (SFR t m) (DynValidation t) (Seq.Seq a) where
  buildValidated = buildAdjustableContainer (SFAdjustableI seqSFA seqSFD)

-- we transform to a map since Set is not Traversable and we want map-like semantics on the as. We could fix this with lens Traversables maybe?
setSFA::Ord a=>SFAppendableI (S.Set a) (M.Map a) a
setSFA = SFAppendableI (CRepI (\s ->M.fromList $ (\x->(x,x)) <$> S.toList s) (\m->S.fromList $ snd <$> M.toList m)) M.empty S.insert S.size

setSFD::Ord a=>SFDeletableI (M.Map a) a a ()
setSFD = SFDeletableI const () id M.delete

instance (SimpleFormInstanceC t m, Ord a, VBuilderC t m a)=>B.Builder (SFR t m) (DynValidation t) (S.Set a) where
  buildValidated = buildAdjustableContainer (SFAdjustableI setSFA setSFD)

hashMapSFA::(Eq k,Hashable k)=>SFAppendableI (HML.HashMap k v) (HML.HashMap k) (k,v)
hashMapSFA = SFAppendableI (CRepI (HML.mapWithKey (\k v->(k,v))) (\m->HML.fromList $ snd <$> HML.toList m)) mempty (\(k,x) m->HML.insert k x m) HML.size

hashMapSFD::(Eq k, Hashable k)=>SFDeletableI (HML.HashMap k) (k,v) k ()
hashMapSFD = SFDeletableI (\(k,_) _ ->k) () id HML.delete


instance (SimpleFormInstanceC t m, Eq k, Hashable k, VBuilderC t m k, VBuilderC t m v)
         =>B.Builder (SFR t m) (DynValidation t) (HML.HashMap k v) where
  buildValidated = buildAdjustableContainer (SFAdjustableI hashMapSFA hashMapSFD)


-- we transform to a HashMap since Set is not Traversable and we want map-like semantics on the as. We could fix this with lens Traversables maybe?
hashSetSFA::(Eq a,Hashable a)=>SFAppendableI (HS.HashSet a) (HML.HashMap a) a
hashSetSFA = SFAppendableI (CRepI (\hs ->HML.fromList $ (\x->(x,x)) <$> HS.toList hs) (\hm->HS.fromList $ snd <$> HML.toList hm)) HML.empty HS.insert HS.size

hashSetSFD::(Eq a, Hashable a)=>SFDeletableI (HML.HashMap a) a a ()
hashSetSFD = SFDeletableI const () id HML.delete

instance (SimpleFormInstanceC t m, Eq a, Hashable a, VBuilderC t m a)=>B.Builder (SFR t m) (DynValidation t) (HS.HashSet a) where
  buildValidated = buildAdjustableContainer (SFAdjustableI hashSetSFA hashSetSFD)


-- the various container builder components
type BuildF t m a = Maybe FieldName->Maybe a->SFRW t m a

-- This feels like a lot of machinery just to get removables...but it does work...
newtype SSFR s t m a = SSFR { unSSFR::StateT s (SFR t m) (DynValidation t a) }

instance (R.Reflex t, R.MonadHold t m)=>Functor (SSFR s t m) where
--  fmap f ssfra = SSFR $ unSSFR ssfra >>= lift . lift . R.mapDyn (fmap f)
  fmap f ssfra = SSFR $ fmap f <$> unSSFR ssfra 

instance (R.Reflex t, R.MonadHold t m)=>Applicative (SSFR s t m) where
  pure x = SSFR $ return $ pure x
  ssfrF <*> ssfrA = SSFR $ do
    dmF <- unSSFR ssfrF
    dmA <- unSSFR ssfrA
    return $ dmF <*> dmA

liftLF'::Monad m=>(forall b.m b->m b)->StateT s m a -> StateT s m a
liftLF' = hoist 

-- utilities for collapsing
widgetToggle::(RD.DomBuilder t m, R.MonadHold t m)=>Bool->R.Event t Bool -> m a -> m a -> m (R.Dynamic t a)
widgetToggle startCond condEv trueW falseW = do
  let condW b = if b then trueW else falseW
  RD.widgetHold (condW startCond) (condW <$> condEv)

widgetToggleDyn::(RD.DomBuilder t m, R.MonadHold t m)=>Bool->R.Event t Bool -> m (R.Dynamic t a) -> m (R.Dynamic t a) -> m (R.Dynamic t a)
widgetToggleDyn startCond condEv trueW falseW = join <$> widgetToggle startCond condEv trueW falseW

-- unstyled, for use within other instances which will deal with the styling.
buildTraversableSFA'::(SimpleFormInstanceC t m,VBuilderC t m b,Traversable g)=>CRepI fa (g b)->BuildF t m b->BuildF t m fa
buildTraversableSFA' crI buildOne _ mfa =
  case mfa of
    Just fa -> unSF $ fromRep crI <$> traverse (liftF sfRow . makeSimpleFormR . buildOne Nothing . Just) (toRep crI fa)
    Nothing -> return dynValidationNothing

-- styled, in case we ever want an editable container without add/remove

buildTraversableSFA::forall t m b g fa.(SimpleFormInstanceC t m, VBuilderC t m b,Traversable g)=>CRepI fa (g b)->BuildF t m fa 
buildTraversableSFA crI md mfa = do
  validClasses <- validDataClasses
  let attrsDyn = R.constDyn $ cssClassAttr validClasses :: R.Dynamic t (M.Map T.Text T.Text)
  sfColDynAttr attrsDyn $ sfCollapsible "" CollapsibleStartsOpen $ buildTraversableSFA' crI (\x -> sfItemL . unSF . B.buildA x) md mfa

-- TODO: need a new version to do widgetHold over whole thing if collapsible depends on size
buildSFContainer::(SimpleFormInstanceC t m, VBuilderC t m b, Traversable g)=>SFAppendableI fa g b->BuildF t m (g b)->BuildF t m fa
buildSFContainer aI buildTr mFN mfa = mdo
  attrsDyn <- sfAttrs dmfa mFN Nothing
  let initial = Just $ maybe (emptyT aI) (toT aI) mfa 
  dmfa <- sfColDynAttr attrsDyn $ sfCollapsible "" CollapsibleStartsOpen $ mdo
    dmfa' <- unSF $ fromT aI <$> (makeSimpleFormR $ joinDynOfDynValidation <$> RD.widgetHold (buildTr mFN initial) newSFREv)
    let udmfa' = unDynValidation dmfa'
        sizeDM = fmap (sizeFa aI) dmfa'
        newSizeEv = R.updated . R.uniqDyn $ unDynValidation sizeDM
        resizedFaEv = R.attachPromptlyDynWithMaybe (\mFa ms -> accValidation (const Nothing) (const $ avToMaybe mFa) ms) udmfa' newSizeEv
          
    addEv <- sfRow $  do
      let emptyB = unSF $ B.buildA Nothing Nothing
      -- we have to clear it once it's used. For now we replace it with a new one.
      dmb <- sfItemL $ joinDynOfDynValidation <$> RD.widgetHold emptyB (emptyB <$ newFaEv) 
      clickEv <-  sfCenter LayoutVertical . sfItemR . lift $ containerActionButton "+" 
      return $ R.attachPromptlyDynWithMaybe (\a b -> avToMaybe a) (unDynValidation dmb) clickEv -- only fires if button is clicked when mb is a Just.
        
    let insert vfa' b = avToMaybe $ insertB aI <$> AccSuccess b <*> vfa'  
        newFaEv = R.attachPromptlyDynWithMaybe insert udmfa' addEv -- Event t (tr a), only fires if (Maybe fa) is not Nothing
        newSFREv = fmap (buildTr mFN . Just . toT aI) (R.leftmost [newFaEv,resizedFaEv]) -- Event t (SFRW t m (g b))
    return dmfa'
  return dmfa


buildSFContainer'::(SimpleFormInstanceC t m,VBuilderC t m b,Traversable g)=>SFAppendableI fa g b->BuildF t m (g b)->BuildF t m fa
buildSFContainer' aI buildTr mFN mfa = mdo
  attrsDyn <- sfAttrs dmfa mFN Nothing
  let initial = Just $ maybe (emptyT aI) (toT aI) mfa 
  dmfa <- sfColDynAttr attrsDyn $ mdo
    dmfa' <- unSF $ fromT aI <$> (makeSimpleFormR $ joinDynOfDynValidation <$> RD.widgetHold (buildTr mFN initial) newSFREv)
    let udmfa' = unDynValidation dmfa'
        sizeDM = fmap (sizeFa aI) dmfa'
        newSizeEv = R.updated . R.uniqDyn $ unDynValidation sizeDM
        resizedFaEv = R.attachPromptlyDynWithMaybe (\mFa ms -> accValidation (const Nothing) (const $ avToMaybe mFa) ms) udmfa' newSizeEv
          
    addEv <- sfRow $  do
      let emptyB = unSF $ B.buildA Nothing Nothing
      -- we have to clear it once it's used. For now we replace it with a new one.
      dmb <- sfItemL $ joinDynOfDynValidation <$> RD.widgetHold emptyB (emptyB <$ newFaEv) 
      clickEv <-  sfCenter LayoutVertical . sfItemR . lift $ containerActionButton "+" 
      return $ R.attachPromptlyDynWithMaybe (\a b -> avToMaybe a) (unDynValidation dmb)  clickEv -- only fires if button is clicked when mb is a Just.
        
    let insert mfa' b = avToMaybe $ insertB aI <$> AccSuccess b <*> mfa'  
        newFaEv = R.attachPromptlyDynWithMaybe insert udmfa' addEv -- Event t (tr a), only fires if (Maybe fa) is not Nothing
        newSFREv = fmap (buildTr mFN . Just . toT aI) (R.leftmost [newFaEv,resizedFaEv]) -- Event t (SFRW t m (g b))
    return dmfa'
  return dmfa

buildOneDeletable::(SimpleFormInstanceC t m, VBuilderC t m b)
                   =>SFDeletableI g b k s->Maybe FieldName->Maybe b->StateT ([R.Event t k],s) (SFR t m) (DynValidation t b)
buildOneDeletable dI mFN ma = liftLF' sfRow $ do     
    (evs,curS) <- get
    dma <- lift . sfItemL . unSF $ B.buildA mFN ma
    ev  <- lift . sfCenter LayoutVertical . sfItemR . lift $ containerActionButton "-" 
    let ev' = R.attachPromptlyDynWithMaybe (\va' _ -> getKey dI <$> (avToMaybe va') <*> Just curS) (unDynValidation dma) ev
    put (ev':evs,updateS dI curS)
    return dma


buildDeletable::(SimpleFormInstanceC t m, VBuilderC t m b, Traversable g)=>SFDeletableI g b k s->BuildF t m (g b)
buildDeletable dI _ mgb = 
  case mgb of
    Nothing -> return dynValidationNothing
    Just gb -> mdo
      let f gb' = do
            (dmgb',(evs,_)) <- runStateT (unSSFR $ traverse (SSFR . liftLF' sfRow  . buildOneDeletable dI Nothing . Just) gb') ([],initialS dI)
            return  (dmgb',R.leftmost evs)
      (ddmgb,dEv) <- R.splitDynPure <$> RD.widgetHold (f gb) (f <$> newgbEv)
      let dmgb = join (unDynValidation <$> ddmgb)
          newgbEv = R.attachPromptlyDynWithMaybe (\mgb' key-> delete dI key <$> (avToMaybe mgb')) dmgb (R.switchPromptlyDyn dEv)
      return $ DynValidation dmgb


-- List Based
-- buildListBasedContainer::(SimpleFormInstanceC t m, VBuilderC t m b, Traversable g)=>SFAppendableI fa g b->BuildF t m (g b)->BuildF t m fa

--data LBCItemState = Visible | Hidden 

{-
data LBCWidgetState k v = LBCWidgetState { lbcSelection::Maybe k, lbcMap::M.Map (Maybe k) (Maybe v) }
data LBCWidgetUpdate k v = ChangeSelection (Maybe k) | ChangeValue k v | DeleteItem k | AddItem k v | EditKey k k

lbcUpdate::(Eq k,Ord k)=>LBCWidgetUpdate k v->LBCWidgetState k v->LBCWidgetState k v
lbcUpdate (ChangeSelection mKey) (LBCWidgetState s m) = let s' = if M.member mKey m then mKey else s in LBCWidgetState s' m
lbcUpdate (ChangeValue key val) (LBCWidgetState s m) = let m'=M.adjust (const $ Just val) (Just key) m in LBCWidgetState s m'
lbcUpdate (DeleteItem key) (LBCWidgetState s m) = let (m',s') = (M.delete (Just key) m',if s==(Just key) then Nothing else s) in LBCWidgetState s' m'
lbcUpdate (AddItem key val) (LBCWidgetState s m) = let m' = M.insert (Just key) (Just val) m in LBCWidgetState s m' 
lbcUpdate (EditKey oldKey newKey) (LBCWidgetState s m) =
  let s' = if s == Just oldKey then Just newKey else s
      m' = maybe m (\x -> M.delete (Just oldKey) $ M.insert (Just newKey) x m ) (M.lookup (Just oldKey) m)
  in LBCWidgetState s' m'
-}

newtype LBCMap k v = LBCMap { lbcMap::M.Map (Maybe k) (Maybe v) }
newtype LBCSelection k = LBCSelection { lbcSelection::Maybe k }

data LBCSelectionUpdate k = SelectionUpdate (Maybe k) 
updateSelection::LBCSelectionUpdate k->LBCSelection k->LBCSelection k
updateSelection (SelectionUpdate mk) _ = LBCSelection mk -- this no longer checks if mk is in map so we need to make sure

data LBCMapUpdate k v = MapChangeValue k v
updateMap::Ord k=>LBCMapUpdate k v->LBCMap k v->LBCMap k v
updateMap (MapChangeValue k v) (LBCMap m) = LBCMap $ M.adjust (const $ Just v) (Just k) m


editOne::(SimpleFormInstanceC t m, VBuilderC t m v)=>R.Dynamic t v->R.Dynamic t Bool->SFR t m (R.Event t v)
editOne valDyn selDyn = do
  let widgetAttrs = (\x -> if x then visibleCSS else hiddenCSS) <$> selDyn
  resDynAV <- RD.elDynAttr "div" widgetAttrs $ unDynValidation <$> (unSF $ buildValidatedDynamic B.validate Nothing (Just valDyn)) -- Dynamic t (AccValidation) val
  let resDyn = accValidation (const Nothing) Just <$> resDynAV -- Dynamic t (Maybe v)
  return . R.fmapMaybe id $ R.updated resDyn 

hiddenCSS::M.Map T.Text T.Text
hiddenCSS  = "style" =: "display: none"
visibleCSS::M.Map T.Text T.Text
visibleCSS = "style" =: "display: inline"


buildLBEditableMap::(SimpleFormInstanceC t m, VBuilderC t m v,Ord k,Show k)=>BuildF t m (M.Map k v)  
buildLBEditableMap mFN mMap = sfRow $ mdo
  let map0 = fromMaybe M.empty mMap
      mk0 = if M.null map0 then Nothing else Just . head $ M.keys map0
      labelF k _ = T.pack $ show k
      editedToCI (Nothing,_) = Nothing -- shouldn't happen
      editedToCI (Just k, v) = Just $ MapChangeValue k v
      editF Nothing _ _ = return R.never
      editF (Just _) valDyn selDyn = editOne (fromJust <$> valDyn) selDyn -- will be okay but relies on getting LBCState manipulation right
  lbcMapDyn <- R.foldDyn updateMap (LBCMap $ toMaybeMap map0) mapUpdateEv
  lbcSelDyn <- R.foldDyn updateSelection (LBCSelection mk0) selUpdateEv
  let selectionDyn = lbcSelection <$> lbcSelDyn
      mapDyn = lbcMap <$> lbcMapDyn 
  selectEv <- sfCol $ selectableKeyList 10 labelF lbcMapDyn
  editedEv <- sfCol $ R.fmapMaybe editedToCI <$> RD.selectViewListWithKey selectionDyn mapDyn editF
  let selUpdateEv = R.leftmost [selectEv]
      mapUpdateEv = R.leftmost [editedEv]
-- TODO: this (the fromMaybeMap) is expensive since it rebuilds the map on all updates
  return . DynValidation $ (AccSuccess . fromMaybeMap <$> mapDyn)

toMaybeMap::Ord k=>M.Map k v->M.Map (Maybe k) (Maybe v)
toMaybeMap = M.insert Nothing Nothing . M.fromList . fmap (\(k,v)->(Just k,Just v)) . M.toList 

fromMaybeMap::Ord k=>M.Map (Maybe k) (Maybe v) -> M.Map k v
fromMaybeMap = M.fromList . catMaybes . fmap (\(mk, mv)-> (,) <$> mk <*> mv) . M.toList 

selectableKeyList::(RD.DomBuilder t m, MonadFix m, R.MonadHold t m, RD.PostBuild t m, Ord k, Show k)
  =>Int -- max size for dropdown
  ->(k->v->T.Text) -- function to label the map entries
  ->R.Dynamic t (LBCMap k v)  -- the map
  ->m (R.Event t (LBCSelectionUpdate k)) 
selectableKeyList maxDDSize labelF curMapDyn = do
  let mDyn = lbcMap <$> curMapDyn
      sizeF n = min maxDDSize (n+1)
      sizeDyn = M.size <$> mDyn
      ddAttrs = ddAttrsDyn sizeF sizeDyn
      labelF' Nothing _ = ""
      labelF' _ Nothing = ""
      labelF' (Just k) (Just v) = labelF k v
      ddMap = R.traceDyn "ddMap: " $ M.mapWithKey labelF' <$> mDyn
      config = RD.DropdownConfig R.never ddAttrs
  fmap SelectionUpdate . R.traceEvent "selectEv: " . RD._dropdown_change <$> RD.dropdown Nothing ddMap config    
  
ddAttrsDyn::R.Reflex t=>(Int->Int)->R.Dynamic t Int->R.Dynamic t RD.AttributeMap
ddAttrsDyn sizeF = fmap (\n->if n==0 then hiddenCSS else visibleCSS <> ("size" =: (T.pack . show $ sizeF n)))



{-
data MapState k = Empty | Singleton k | Bigger deriving (Eq)
toMapState::M.Map k v->MapState k
toMapState m = case M.size m of
  0 -> Empty
  1 -> let x = head $ M.keys m in Singleton x
  _ -> Bigger


lbcDropdown::forall t m k v.(RD.DomBuilder t m, MonadFix m, R.MonadHold t m, RD.PostBuild t m, Ord k)
  =>Int -- max dropdown size
  ->(k->v->T.Text) -- text to show in dropdown
  ->R.Dynamic t (LBCWidgetState k v) -- current state
  ->m (R.Event t (LBCWidgetUpdate k v)) 
lbcDropdown maxSize labelF curStateDyn = do
  let mDyn = unLBCWidgetState  <$> curStateDyn
      sizeF n = max maxSize (n+1)
      sizeDyn = M.size <$> mDyn
      mapStateDyn = toMapState <$> mDyn
      ddAttrs = ddAttrsDyn sizeF sizeDyn
      ddMap = M.mapWithKey (\k (v,_) -> labelF k v) <$> mDyn
      mapStateChangeEv = R.updated . R.uniqDyn $ mapStateDyn
      emptyWidget = RD.el "h1" $ RD.text "Container has no items!" >> return R.never
      widget mapState = case mapState of
        Empty -> Just emptyWidget
        Singleton k -> Just $ do
          let config = RD.DropdownConfig R.never ddAttrs
          fmap Reveal . RD._dropdown_change <$> RD.dropdown k ddMap config
        Bigger -> Nothing
      widgetEv::(RD.DomBuilder t m, MonadFix m, R.MonadHold t m, RD.PostBuild t m, Ord k)=>R.Event t (m (R.Event t (LBCWidgetUpdate k v)))
      widgetEv = R.fmapMaybe widget $ R.leftmost [mapStateChangeEv, R.attachWith const (R.current mapStateDyn) R.getPostBuild]
  R.switchPromptlyDyn <$> RD.widgetHold (emptyWidget) widgetEv



-}

{-
-- This is not optimal.  The Dropdown can handle dynamic maps which means we only need to re-widget when size switches from 0 to non-zero and vice-versa
selectableKeyList::(RD.DomBuilder t m, MonadFix m, R.MonadHold t m, RD.PostBuild t m, Ord k)=>Int->k->(k->v->T.Text)->R.Dynamic t (LBCWidgetState k v)->m (R.Dynamic t (Maybe k))
selectableKeyList maxSelectSize k0 labelF lbcStateDyn = do
  let mDyn = unLBCWidgetState  <$> lbcStateDyn
      sizeF n = max maxSelectSize (n+1)
      sizeDyn = M.size <$> mDyn
      ddAttrs = ddAttrsDyn sizeF sizeDyn
      newWidgetEv = (\m->(m,M.size m > 0)) <$> mDyn
      makeWidget (m,nonE) = case nonE of
        False -> el "h1" $ text "No entries in container!" >> return (R.constDyn Nothing) 
        True -> do
          let ddMap = M.mapWithKey labelF m
              config = DropDownConfig R.never ddAttrs
              k0 = head . M.keys $ m
          fmap Just . RD._dropdown_value <$> RD.dropdown k0 (R.constDyn ddMap) config
  RD.widgetHold (makeWidget (m,M.size > 0)) (makeWidget <$> newWidgetEv)
-}

{-
selectableKeyListWidget::(RD.DomBuilder t m, MonadFix m, R.MonadHold t m, RD.PostBuild t m, Ord k)=>Int->(k->v->T.Text)->R.Dynamic t (LBCWidgetState k v)->m (R.Dynamic t k)
selectableKeyListWidget maxSelectSize labelF lcbStateDyn = do
  let widget maxSize lF (LBCWidgetState m) = do
        let emptyWidget = RD.el "h1" $ RD.text "No elements in container!"
            nonEmptyWidget = do
              let k0 = head . M.keys $ m'
              selectableKeyList maxSize k0 lF               



slvWrapper::(DomBuilder t m, Ord k, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t (Maybe k)  -- ^ Current selection key
  -> Dynamic t (Map k v)  -- ^ Dynamic key/value map
  -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a)) -- ^ Function to create a widget for a given key from Dynamic value and Dynamic Bool indicating if this widget is currently selected
  -> m (Event t (k, a))
slvWrapper selDyn mapDyn widgetF =  do
  let nonEmptyEv = fmapMaybe id
-}

  
  
  
