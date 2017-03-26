{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveGeneric #-}
import           GHCJS.DOM.Types                  (JSM)
import           Language.Javascript.JSaddle.Warp (run)
import           Reflex
import           Reflex.Dom                       hiding (mainWidget, run)
import           Reflex.Dom.Core                  (mainWidget)
import           Reflex.Dom.Old                   (MonadWidget)

import           Control.Monad.Fix                (MonadFix)

import           Control.Monad                    (join)
import qualified Data.Map                         as M
import           Data.Maybe                       (isNothing,catMaybes)
import qualified Data.Text                        as T
import           Data.Traversable                 (sequenceA)

import           System.Process                   (spawnProcess)
import           Text.Read                        (readMaybe)

import           Generics.SOP                     ((:.:) (..), All2, Code,
                                                   ConstructorName, Generic,
                                                   HasDatatypeInfo, unComp)
import           Reflex.Dom.Contrib.DynamicUtils
import           Reflex.Dom.Contrib.SumType


import qualified GHC.Generics as GHC 

-- NB: This is just for warp.
main::IO ()
main = do
  let port :: Int = 3702
  pHandle <- spawnProcess "open" ["http://localhost:" ++ show port]
  run port testWidget

data TestEither = LeftInt Int | RightText T.Text deriving (Show,GHC.Generic)

instance Generic TestEither
instance HasDatatypeInfo TestEither

testWidget::JSM ()
testWidget = mainWidget $ do
  el "span" $ text "Int: "
  dynMInt <- build (Comp . constDyn $ Just (2::Int))
  el "br" $ blank
  el "span" $ text "Either Int Text: "
  dynMTE1 <- buildSum (LeftInt <$> dynMInt)
  el "br" blank
  dynMaybeText dynMTE1
  el "br" blank
  el "span" $ text "Text: "
  dynMText <- build (Comp $ constDyn (Nothing :: Maybe T.Text))
  el "br" blank
  el "span" $ text "Either Int Text: "
  dynMTE2 <- buildSum (RightText <$> dynMText)
  el "br" blank
  dynMaybeText dynMTE2

  return ()

dynMaybeText::(ReflexConstraints t m, Show a)=>DynMaybe t a->m ()
dynMaybeText = dynText . fmap (T.pack . show) . unComp

type ReflexConstraints t m = (MonadWidget t m, DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
type WidgetConstraints t m a = (ReflexConstraints t m, Show a)

type DynMaybe t = (Dynamic t) :.: Maybe
type DynIsCon t = (Dynamic t) :.: IsCon

dm2di::Reflex t=>DynMaybe t a -> DynIsCon t a
dm2di = Comp . fmap IsCon . unComp

di2dm::Reflex t=>DynIsCon t a -> DynMaybe t a
di2dm = Comp . fmap unIsCon . unComp

-- These don't "fire" on Nothing/Nothing updates.  But they do fire on Nothing/Just, Just/Nothing and Just/Just
uniqDynJust::Reflex t=>DynMaybe t a -> DynMaybe t a
uniqDynJust = Comp . uniqDynBy (\a b->isNothing a && isNothing b) . unComp

uniqDynIsCon::Reflex t=>DynIsCon t a -> DynIsCon t a
uniqDynIsCon = dm2di . uniqDynJust . di2dm

isConDyn::Reflex t=>DynIsCon t a -> Dynamic t Bool
isConDyn = fmap (not . isNothing . unIsCon) . unComp


fieldWidget'::(WidgetConstraints t m a,Read a)=>(T.Text -> Maybe a) -> DynMaybe t a -> m (DynMaybe t a)
fieldWidget' parse dma = do
  inputEv <- fmapMaybe id <$> dynAsEv (unComp dma) -- Event t a
  let inputEvT = T.pack . show <$> inputEv
      config = TextInputConfig "text" "" inputEvT (constDyn M.empty)
  aDyn <- _textInput_value <$> textInput config
  return . Comp $ parse <$> aDyn

fieldWidget::(WidgetConstraints t m a,Read a)=>DynMaybe t a -> m (DynMaybe t a)
fieldWidget = fieldWidget' (readMaybe . T.unpack)

fieldWidgetIC::(WidgetConstraints t m a,Read a)=>DynIsCon t a -> m (DynIsCon t a)
fieldWidgetIC = fmap dm2di . fieldWidget . di2dm

sumChooserSimple::WidgetConstraints t m a=>[(ConstructorName, (m :.: (DynIsCon t)) a)]->m (DynMaybe t a)
sumChooserSimple namedWidgets = do
  let (conNames, widgets') = unzip namedWidgets
      widgets = unComp <$> widgets' -- [m (DynIsCon t a)]
  dynIsCons <- sequenceA widgets -- [DynIsCon t a], put widgets in DOM
  let dynMaybes = di2dm <$> dynIsCons
      maybeDyns = unComp <$> dynMaybes
      listMaybeDyn = sequenceA maybeDyns
      headMay [] = Nothing
      headMay (x : xs) = Just x
      maybeDyn = (headMay . catMaybes) <$> listMaybeDyn
  return $ Comp maybeDyn
  

sumChooserWH::WidgetConstraints t m a=>[ConWidget t m a]->m (DynMaybe t a)
sumChooserWH cws = mdo
  let indexedCN = zip [0..] (T.pack . conName <$> cws)
      inputIndexEv = whichFired (switchedTo <$> cws)
      ddConfig = DropdownConfig inputIndexEv (constDyn mempty)
  chosenIndexEv <- _dropdown_change <$> dropdown 0 (constDyn $ M.fromList indexedCN) ddConfig -- put dropdown in DOM
  let newIndexEv = leftmost [inputIndexEv,chosenIndexEv]
  let newWidgetEv = (\n -> (unComp . widget <$> cws) !! n) <$> newIndexEv -- Event t (m (DynMaybe t a))
  curIndex <- holdDyn 0 newIndexEv
  dynText $ T.pack .show <$> curIndex
  Comp . join . fmap unComp <$> widgetHold (head $ unComp . widget <$> cws) newWidgetEv
  
  

class WidgetConstraints t m a => TestBuilder t m a where
  build::DynMaybe t a -> m (DynMaybe t a)


q::Reflex t=>(DynMaybe t :.: IsCon) a -> DynMaybe t a
q = Comp . fmap (join . fmap unIsCon) . unComp . unComp

instance TestBuilder t m a=>Map (Dynamic t :.: IsCon) (m :.: Dynamic t :.: Maybe) a where
  doMap = Comp . build . di2dm . uniqDynIsCon

buildSum'::forall a t m.(Generic a, HasDatatypeInfo a
          , WidgetConstraints t m a
          , All2 (Map (DynMaybe t :.: IsCon) (m :.: (DynIsCon t))) (Code a))=>DynMaybe t a->m (DynMaybe t a)
buildSum' = sumChooserSimple . functorDoPerConstructor' doAndSequence'

buildSum::forall a t m.(Generic a, HasDatatypeInfo a
          , WidgetConstraints t m a
          , All2 (Map (Dynamic t :.: IsCon) (m :.: (Dynamic t :.: Maybe))) (Code a))=>DynMaybe t a->m (DynMaybe t a)
buildSum = sumChooserWH . maybeDynToConWidgets doAndSequence'


instance WidgetConstraints t m Int => TestBuilder t m Int where
  build = fieldWidget

instance WidgetConstraints t m Double => TestBuilder t m Double where
  build = fieldWidget

instance WidgetConstraints t m T.Text => TestBuilder t m T.Text where
  build = fieldWidget

{-
instance TestBuilder t m a => TestBuilder t m (Maybe a) where
  build = buildSum

instance (TestBuilder t m a, TestBuilder t m b) => TestBuilder t m (Either a b) where
  build = buildSum
-}








