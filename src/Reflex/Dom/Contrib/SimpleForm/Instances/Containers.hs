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
module Reflex.Dom.Contrib.SimpleForm.Instances.Containers () where

-- All the basic (primitive types, tuples, etc.) are in here
import Reflex.Dom.Contrib.SimpleForm.Instances.Basic (SimpleFormInstanceC)
import Reflex.Dom.Contrib.SimpleForm.Builder
import Reflex.Dom.Contrib.ReflexConstraints
import Reflex.Dom.Contrib.Layout.Types (LayoutDirection(..),LayoutOrientation(..))
-- reflex imports
import qualified Reflex as R 
import qualified Reflex.Dom as RD


import Control.Monad (join)
import Control.Monad.Reader (ReaderT, lift)
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


buildAdjustableContainer::(SimpleFormInstanceC t m,B.Builder (SimpleFormR t m) b,Traversable g)
                          =>SFAdjustableI fa g b k s->Maybe FieldName->Maybe fa->SimpleFormR t m fa
buildAdjustableContainer sfAdj mFN mfa = SimpleFormR  $ do
  formType <- getFormType
  case formType of
    ObserveOnly ->  buildReadOnlyContainer (cRep . sfAI $ sfAdj) mFN mfa
    Interactive ->  buildSFContainer (sfAI sfAdj) (buildDeletable (sfDI sfAdj)) mFN mfa
--    else buildSFContainer (sfAI sfAdj) (buildTraversableSFA idRep) mFN mfa

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

buildReadOnlyContainer::(SimpleFormInstanceC t m,B.Builder (SimpleFormR t m) b,Traversable g)
                        =>CRepI fa (g b)->BuildF t m fa
buildReadOnlyContainer = buildTraversableSFA  


listAppend::a->[a]->[a]
listAppend a as = as ++ [a]

listDeleteAt::Int->[a]->[a]
listDeleteAt n as = take n as ++ drop (n+1) as

listSFA::SFAppendableI [a] [] a
listSFA = SFAppendableI idRep [] listAppend L.length

listSFD::SFDeletableI [] a Int Int
listSFD = SFDeletableI (\_ n->n) 0 (+1) listDeleteAt

instance (SimpleFormInstanceC t m,B.Builder (SimpleFormR t m) a)=>B.Builder (SimpleFormR t m) [a] where
  buildA = buildAdjustableContainer (SFAdjustableI listSFA listSFD)


mapSFA::Ord k=>SFAppendableI (M.Map k v) (M.Map k) (k,v)
mapSFA = SFAppendableI (CRepI (M.mapWithKey (\k v->(k,v))) (\m->M.fromList $ snd <$> M.toList m)) mempty (\(k,x) m->M.insert k x m) M.size

mapSFD::Ord k=>SFDeletableI (M.Map k) (k,v) k ()
mapSFD = SFDeletableI (\(k,_) _ ->k) () id M.delete

instance (SimpleFormInstanceC t m,
          B.Builder  (SimpleFormR t m)  k, Ord k,
          B.Builder  (SimpleFormR t m)  a)
         =>B.Builder (SimpleFormR t m) (M.Map k a) where
  buildA = buildAdjustableContainer (SFAdjustableI mapSFA mapSFD)

intMapSFA::SFAppendableI (IM.IntMap v) IM.IntMap (IM.Key,v)
intMapSFA = SFAppendableI (CRepI (IM.mapWithKey (\k v->(k,v))) (\m->IM.fromList $ snd <$> IM.toList m)) mempty (\(k,x) m->IM.insert k x m) IM.size

intMapSFD::SFDeletableI IM.IntMap (IM.Key,v) IM.Key ()
intMapSFD = SFDeletableI (\(k,_) _ ->k) () id IM.delete

instance (SimpleFormInstanceC t m,
          B.Builder  (SimpleFormR t m)  IM.Key,
          B.Builder  (SimpleFormR t m)  a)
         =>B.Builder (SimpleFormR t m) (IM.IntMap a) where
  buildA = buildAdjustableContainer (SFAdjustableI intMapSFA intMapSFD)

seqSFA::SFAppendableI (Seq.Seq a) Seq.Seq a
seqSFA = SFAppendableI idRep Seq.empty (flip (Seq.|>)) Seq.length

seqSFD::SFDeletableI Seq.Seq a Int Int
seqSFD = SFDeletableI (\_ index->index) 0 (+1) (\n bs -> Seq.take n bs Seq.>< Seq.drop (n+1) bs)

instance (SimpleFormInstanceC t m,B.Builder (SimpleFormR t m) a)=>B.Builder (SimpleFormR t m) (Seq.Seq a) where
  buildA = buildAdjustableContainer (SFAdjustableI seqSFA seqSFD)

-- we transform to a map since Set is not Traversable and we want map-like semantics on the as. We could fix this with lens Traversables maybe?
setSFA::Ord a=>SFAppendableI (S.Set a) (M.Map a) a
setSFA = SFAppendableI (CRepI (\s ->M.fromList $ (\x->(x,x)) <$> S.toList s) (\m->S.fromList $ snd <$> M.toList m)) M.empty S.insert S.size

setSFD::Ord a=>SFDeletableI (M.Map a) a a ()
setSFD = SFDeletableI const () id M.delete

instance (SimpleFormInstanceC t m, B.Builder (SimpleFormR t m) a,Ord a)=>B.Builder (SimpleFormR t m) (S.Set a) where
  buildA = buildAdjustableContainer (SFAdjustableI setSFA setSFD)

hashMapSFA::(Eq k,Hashable k)=>SFAppendableI (HML.HashMap k v) (HML.HashMap k) (k,v)
hashMapSFA = SFAppendableI (CRepI (HML.mapWithKey (\k v->(k,v))) (\m->HML.fromList $ snd <$> HML.toList m)) mempty (\(k,x) m->HML.insert k x m) HML.size

hashMapSFD::(Eq k, Hashable k)=>SFDeletableI (HML.HashMap k) (k,v) k ()
hashMapSFD = SFDeletableI (\(k,_) _ ->k) () id HML.delete


instance (SimpleFormInstanceC t m,
          B.Builder  (SimpleFormR t m)  k, Eq k, Hashable k,
          B.Builder  (SimpleFormR t m)  v)
         =>B.Builder (SimpleFormR t m) (HML.HashMap k v) where
  buildA = buildAdjustableContainer (SFAdjustableI hashMapSFA hashMapSFD)


-- we transform to a HashMap since Set is not Traversable and we want map-like semantics on the as. We could fix this with lens Traversables maybe?
hashSetSFA::(Eq a,Hashable a)=>SFAppendableI (HS.HashSet a) (HML.HashMap a) a
hashSetSFA = SFAppendableI (CRepI (\hs ->HML.fromList $ (\x->(x,x)) <$> HS.toList hs) (\hm->HS.fromList $ snd <$> HML.toList hm)) HML.empty HS.insert HS.size

hashSetSFD::(Eq a, Hashable a)=>SFDeletableI (HML.HashMap a) a a ()
hashSetSFD = SFDeletableI const () id HML.delete

instance (SimpleFormInstanceC t m,
          B.Builder  (SimpleFormR t m)  a, Eq a, Hashable a)
         =>B.Builder (SimpleFormR t m) (HS.HashSet a) where
  buildA = buildAdjustableContainer (SFAdjustableI hashSetSFA hashSetSFD)


-- the various container builder components
type BuildF t m a = Maybe FieldName->Maybe a->SFRW t m a

-- This feels like a lot of machinery just to get removables...but it does work...
newtype SSFR s t m a = SSFR { unSSFR::StateT s (SFR m) (DynValidation t a) }

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
buildTraversableSFA'::(SimpleFormInstanceC t m,B.Builder (SimpleFormR t m) b,Traversable g)=>CRepI fa (g b)->BuildF t m b->BuildF t m fa
buildTraversableSFA' crI buildOne _ mfa =
  case mfa of
    Just fa -> unSF $ fromRep crI <$> traverse (liftF formRow . SimpleFormR . buildOne Nothing . Just) (toRep crI fa)
    Nothing -> return $ dynValidationNothing

-- styled, in case we ever want an editable container without add/remove

buildTraversableSFA::forall t m b g fa.(SimpleFormInstanceC t m,
                      B.Builder (SimpleFormR t m) b,Traversable g)=>CRepI fa (g b)->BuildF t m fa 
buildTraversableSFA crI md mfa = do
  validClasses <- validInputStyle
  lcF <- layoutCollapsible <$> view layoutConfig
  let attrsDyn = R.constDyn $ cssClassAttr validClasses :: R.Dynamic t (M.Map T.Text T.Text)
  formCol' attrsDyn $ lcF "" CollapsibleStartsOpen $ buildTraversableSFA' crI (\x -> itemL . unSF . B.buildA x) md mfa

-- TODO: need a new version to do widgetHold over whole thing if collapsible depends on size
buildSFContainer::(SimpleFormInstanceC t m,
                   B.Builder (SimpleFormR t m) b,
                   Traversable g)=>SFAppendableI fa g b->BuildF t m (g b)->BuildF t m fa
buildSFContainer aI buildTr mFN mfa = mdo
  attrsDyn <- sfAttrs dmfa mFN Nothing
  layoutCollapsibleF <- layoutCollapsible <$> view layoutConfig
  layoutCenteredF <- layoutCentered <$> view layoutConfig
  let initial = Just $ maybe (emptyT aI) (toT aI) mfa 
  dmfa <- formCol' attrsDyn $ layoutCollapsibleF "" CollapsibleStartsOpen $ mdo
    dmfa' <- unSF $ fromT aI <$> (SimpleFormR $ joinDynOfDynValidation <$> RD.widgetHold (buildTr mFN initial) newSFREv)
    let udmfa' = unDynValidation dmfa'
        sizeDM = fmap (sizeFa aI) dmfa'
        newSizeEv = R.updated . R.uniqDyn $ unDynValidation sizeDM
        resizedFaEv = R.attachPromptlyDynWithMaybe (\mFa ms -> accValidation (const Nothing) (const $ avToMaybe mFa) ms) udmfa' newSizeEv
          
    addEv <- formRow $  do
      let emptyB = unSF $ B.buildA Nothing Nothing
      -- we have to clear it once it's used. For now we replace it with a new one.
      dmb <- itemL $ joinDynOfDynValidation <$> RD.widgetHold emptyB (emptyB <$ newFaEv) 
      clickEv <-  layoutCenteredF LayoutVertical . itemR . lift $ containerActionButton "+" 
      return $ R.attachPromptlyDynWithMaybe (\a b -> avToMaybe a) (unDynValidation dmb) clickEv -- only fires if button is clicked when mb is a Just.
        
    let insert vfa' b = avToMaybe $ insertB aI <$> AccSuccess b <*> vfa'  
        newFaEv = R.attachPromptlyDynWithMaybe insert udmfa' addEv -- Event t (tr a), only fires if (Maybe fa) is not Nothing
        newSFREv = fmap (buildTr mFN . Just . toT aI) (R.leftmost [newFaEv,resizedFaEv]) -- Event t (SFRW t m (g b))
    return dmfa'
  return dmfa


buildSFContainer'::(SimpleFormInstanceC t m,B.Builder (SimpleFormR t m) b,Traversable g)=>SFAppendableI fa g b->BuildF t m (g b)->BuildF t m fa
buildSFContainer' aI buildTr mFN mfa = mdo
  attrsDyn <- sfAttrs dmfa mFN Nothing
  layoutCenteredF <- layoutCentered <$> view layoutConfig
  let initial = Just $ maybe (emptyT aI) (toT aI) mfa 
  dmfa <- formCol' attrsDyn $ mdo
    dmfa' <- unSF $ fromT aI <$> (SimpleFormR $ joinDynOfDynValidation <$> RD.widgetHold (buildTr mFN initial) newSFREv)
    let udmfa' = unDynValidation dmfa'
        sizeDM = fmap (sizeFa aI) dmfa'
        newSizeEv = R.updated . R.uniqDyn $ unDynValidation sizeDM
        resizedFaEv = R.attachPromptlyDynWithMaybe (\mFa ms -> accValidation (const Nothing) (const $ avToMaybe mFa) ms) udmfa' newSizeEv
          
    addEv <- formRow $  do
      let emptyB = unSF $ B.buildA Nothing Nothing
      -- we have to clear it once it's used. For now we replace it with a new one.
      dmb <- itemL $ joinDynOfDynValidation <$> RD.widgetHold emptyB (emptyB <$ newFaEv) 
      clickEv <-  layoutCenteredF LayoutVertical . itemR . lift $ containerActionButton "+" 
      return $ R.attachPromptlyDynWithMaybe (\a b -> avToMaybe a) (unDynValidation dmb)  clickEv -- only fires if button is clicked when mb is a Just.
        
    let insert mfa' b = avToMaybe $ insertB aI <$> AccSuccess b <*> mfa'  
        newFaEv = R.attachPromptlyDynWithMaybe insert udmfa' addEv -- Event t (tr a), only fires if (Maybe fa) is not Nothing
        newSFREv = fmap (buildTr mFN . Just . toT aI) (R.leftmost [newFaEv,resizedFaEv]) -- Event t (SFRW t m (g b))
    return dmfa'
  return dmfa

buildOneDeletable::(SimpleFormInstanceC t m, B.Builder (SimpleFormR t m) b)
                   =>SFDeletableI g b k s->Maybe FieldName->Maybe b->StateT ([R.Event t k],s) (SFR m) (DynValidation t b)
buildOneDeletable dI mFN ma = liftLF' formRow $ do     
    (evs,curS) <- get
    dma <- lift . itemL . unSF $ B.buildA mFN ma
    layoutCenteredF <- layoutCentered <$> view layoutConfig
    ev  <- lift . layoutCenteredF LayoutVertical . itemR . lift $ containerActionButton "-" 
    let ev' = R.attachPromptlyDynWithMaybe (\va' _ -> getKey dI <$> (avToMaybe va') <*> Just curS) (unDynValidation dma) ev
    put (ev':evs,updateS dI curS)
    return dma


buildDeletable::(SimpleFormInstanceC t m, B.Builder (SimpleFormR t m) b, Traversable g)=>SFDeletableI g b k s->BuildF t m (g b)
buildDeletable dI _ mgb = 
  case mgb of
    Nothing -> return dynValidationNothing
    Just gb -> mdo
      let f gb' = do
            (dmgb',(evs,_)) <- runStateT (unSSFR $ traverse (SSFR . liftLF' formRow  . buildOneDeletable dI Nothing . Just) gb') ([],initialS dI)
            return  (dmgb',R.leftmost evs)
--      (ddmgb,dEv) <- join $ R.splitDyn <$> RD.widgetHold (f gb) (f <$> newgbEv)
      (ddmgb,dEv) <- R.splitDynPure <$> RD.widgetHold (f gb) (f <$> newgbEv)
      let dmgb = join (unDynValidation <$> ddmgb)
          newgbEv = R.attachPromptlyDynWithMaybe (\mgb' key-> delete dI key <$> (avToMaybe mgb')) dmgb (R.switchPromptlyDyn dEv)
      return $ DynValidation dmgb

{- This fails with ambiguous types, because TR and E are not injective.  Why doesn't ScopedTypeVariables help?
class SFAppendable fa where
  type Tr fa :: * -> *
  type E fa :: *
  toT'::fa -> Tr fa (E fa)
  fromT'::Tr fa (E fa) -> fa 
  emptyT' :: Tr fa (E fa)
  insertT' :: E fa -> Tr fa (E fa) -> Tr fa (E fa)

buildSFContainer'::(SimpleFormC t m, SFAppendable fa, B.Builder (SimpleFormR t m) (E fa), Traversable (Tr fa))
                   =>BuildF t m (Tr fa (E fa))->BuildF t m fa
buildSFContainer' buildTr md mfa = do
  validClasses <- validItemStyle
  invalidClasses <- invalidItemStyle
  buttonClasses <- buttonStyle
  mdo
    attrsDyn <- sfAttrs dmla md Nothing
    let initial::Maybe (Tr fa (E fa)) 
        initial = maybe (Just emptyT') (Just . toT') mfa 
    dmla <- formCol' attrsDyn $ mdo
      dmla' <- R.joinDyn <$> RD.widgetHold (buildTr md initial) newSFREv -- SFRW t m (Dynamic t (g b))
      addEv <- formRow $ do
        let emptyA = unSF $ B.buildA md Nothing 
        dma <- itemL $ RD.joinDyn <$> RD.widgetHold (emptyA) (fmap (const emptyA) $ R.updated dmla')
        clickEv <-  itemR . lift $ buttonClass "+" (toCssString buttonClasses)-- need attributes for styling??
        return $ R.attachDynWithMaybe (\ma _ -> ma) dma clickEv -- only fires if button is clicked when a is a Just.
      let insert::Maybe (Tr fa (E fa))->Maybe (E fa)->Maybe (Tr fa (E fa))
          insert maa a = insertT' <$> (Just a) <*> maa 
          newTrEv = R.attachDynWithMaybe insert dmla' addEv -- Event t (tr a), only fires if traverable is not Nothing
          newSFREv = fmap (buildTr md . Just) newTrEv -- Event t (SFRW t m (g b)))
      return dmla'
    lift $ R.mapDyn (\ml -> fromT' <$> ml) dmla
-}
