{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
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
import           GHCJS.DOM.Types                  (JSM)
import           Language.Javascript.JSaddle.Warp (run)
import           Reflex
import           Reflex.Dom                       hiding (mainWidget, run)
import           Reflex.Dom.Core                  (mainWidget)
import           Reflex.Dom.Old                   (MonadWidget)

import           Control.Monad.Fix                (MonadFix)

import           Control.Monad                    (join)
import qualified Data.Map                         as M
import           Data.Maybe                       (catMaybes, isNothing)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           Data.Traversable                 (sequenceA)

import           System.Process                   (spawnProcess)
import           Text.Read                        (readMaybe)

import           Generics.SOP                     ((:.:) (..), All2, Code,
                                                   ConstructorName, Generic,
                                                   HasDatatypeInfo, unComp)
import           Reflex.Dom.Contrib.DynamicUtils
import           Reflex.Dom.Contrib.SumType


import qualified GHC.Generics                     as GHC

-- NB: This is just for warp.
main::IO ()
main = do
  let port :: Int = 3702
  pHandle <- spawnProcess "open" ["http://localhost:" ++ show port]
  run port testWidget

data TestEither = LeftInt Int | RightText T.Text deriving (Show,GHC.Generic)

instance Generic TestEither
instance HasDatatypeInfo TestEither


data TestSum = A Int | B T.Text | C Double | D T.Text deriving (Show, GHC.Generic)
instance Generic TestSum
instance HasDatatypeInfo TestSum

data TestProd = TestProd Int Double deriving (Show,GHC.Generic)
instance Generic TestProd
instance HasDatatypeInfo TestProd


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
  dynMText <- build (Comp $ constDyn (Just $ ("ABC"::T.Text)))
  el "br" blank
  el "span" $ text "Either Int Text: "
  dynMTE2 <- buildSum (RightText <$> dynMText)
  el "br" blank
  dynMaybeText dynMTE2
  el "br" blank
  el "span" $ text "Double: "
  dynMDouble <- build (Comp . constDyn $ Nothing)
  el "br" blank
  el "span" $ text "TestSum: "
  dynMTS <- buildSum (C <$> dynMDouble)
  el "br" blank
  dynMaybeText dynMTS
  el "br" blank
  el "span" $ text "TestProd: "
  dynMTP <- buildSum (Comp . constDyn . Just $ TestProd 2 2.0)
  el "br" blank
  dynMaybeText dynMTP
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


fieldWidget'::(WidgetConstraints t m a)=>(T.Text -> Maybe a) -> (a -> T.Text)->DynMaybe t a -> m (DynMaybe t a)
fieldWidget' parse print dma = do
  inputEv <- fmapMaybe id <$> traceDynAsEv (const "fieldWidget'-") (unComp dma) -- Event t a
  let inputEvT = print <$> inputEv
      config = TextInputConfig "text" "" inputEvT (constDyn M.empty)
  aDyn <- _textInput_value <$> textInput config
  return . Comp $ parse <$> aDyn

fieldWidget::(WidgetConstraints t m a,Read a)=>DynMaybe t a -> m (DynMaybe t a)
fieldWidget = fieldWidget' (readMaybe . T.unpack) (T.pack . show)

fieldWidgetIC::(WidgetConstraints t m a,Read a)=>DynIsCon t a -> m (DynIsCon t a)
fieldWidgetIC = fmap dm2di . fieldWidget . di2dm

sumChooserWH::WidgetConstraints t m a=>[ConWidget t m a]->m (DynMaybe t a)
sumChooserWH cws = mdo
  let indexedCN = zip [0..] (T.pack . conName <$> cws)
      inputIndexEv = whichFired (switchedTo <$> cws)
      ddConfig = DropdownConfig inputIndexEv (constDyn mempty)
  chosenIndexEv <- _dropdown_change <$> dropdown 0 (constDyn $ M.fromList indexedCN) ddConfig -- put dropdown in DOM
  let newIndexEv = leftmost [inputIndexEv
                            ,chosenIndexEv]
  curIndex <- holdDyn 0 newIndexEv
  let switchWidgetEv = updated . uniqDyn $ curIndex
      newWidgetEv = (\n -> (unComp . widget <$> cws) !! n) <$> switchWidgetEv -- Event t (m (DynMaybe t a))
  dynText $ T.pack .show <$> curIndex
  Comp . join . fmap unComp <$> widgetHold (head $ unComp . widget <$> cws) newWidgetEv


class WidgetConstraints t m a => TestBuilder t m a where
  build::DynMaybe t a -> m (DynMaybe t a)


q::Reflex t=>(DynMaybe t :.: IsCon) a -> DynMaybe t a
q = Comp . fmap (join . fmap unIsCon) . unComp . unComp

instance TestBuilder t m a=>Map (Dynamic t :.: IsCon) (m :.: Dynamic t :.: Maybe) a where
  doMap = Comp . build . di2dm . uniqDynIsCon


buildSum::forall a t m.(Generic a, HasDatatypeInfo a
          , WidgetConstraints t m a
          , All2 (Map (Dynamic t :.: IsCon) (m :.: (Dynamic t :.: Maybe))) (Code a))=>DynMaybe t a->m (DynMaybe t a)
buildSum = sumChooserWH . maybeDynToConWidgets doAndSequence'


instance WidgetConstraints t m Int => TestBuilder t m Int where
  build = fieldWidget

instance WidgetConstraints t m Double => TestBuilder t m Double where
  build = fieldWidget

instance WidgetConstraints t m T.Text => TestBuilder t m T.Text where
  build = fieldWidget' Just id


instance TestBuilder t m a => TestBuilder t m (Maybe a) where
  build = buildSum

instance (TestBuilder t m a, TestBuilder t m b) => TestBuilder t m (Either a b) where
  build = buildSum









