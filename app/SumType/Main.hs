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

import           GHCJS.DOM.Types                  (JSM)
import           Language.Javascript.JSaddle.Warp (run)
import           Reflex
import           Reflex.Dom                       hiding (mainWidget, run)
import           Reflex.Dom.Core                  (mainWidget)
import           Reflex.Dom.Old                   (MonadWidget)

import           Control.Monad.Fix                (MonadFix)

import qualified Data.Map                         as M
import           Data.Maybe                       (isNothing)
import qualified Data.Text                        as T
import           Data.Traversable                 (sequenceA)
import           Control.Monad                    (join)

import           System.Process                   (spawnProcess)
import           Text.Read                        (readMaybe)

import           Generics.SOP                     ((:.:) (..), ConstructorName, Generic, HasDatatypeInfo, All2,Code,
                                                   unComp)
import           Reflex.Dom.Contrib.DynamicUtils
import           Reflex.Dom.Contrib.SumType

-- NB: This is just for warp.
main::IO ()
main = do
  let port :: Int = 3702
  pHandle <- spawnProcess "open" ["http://localhost:" ++ show port]
  run port testWidget

testWidget = return ()

type ReflexConstraints t m = (MonadWidget t m, DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
type WidgetConstraints t m a = (ReflexConstraints t m, Show a, Read a)

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


fieldWidget'::WidgetConstraints t m a=>(T.Text -> Maybe a) -> DynMaybe t a -> m (DynMaybe t a)
fieldWidget' parse dma = do
  inputEv <- fmapMaybe id <$> dynAsEv (unComp dma) -- Event t a
  let inputEvT = T.pack . show <$> inputEv
      config = TextInputConfig "text" "" inputEvT (constDyn M.empty)
  aDyn <- _textInput_value <$> textInput config
  return . Comp $ parse <$> aDyn

fieldWidget::WidgetConstraints t m a=>DynMaybe t a -> m (DynMaybe t a)
fieldWidget = fieldWidget' (readMaybe . T.unpack)

fieldWidgetIC::WidgetConstraints t m a=>DynIsCon t a -> m (DynIsCon t a)
fieldWidgetIC = fmap dm2di . fieldWidget . di2dm

hideableW::WidgetConstraints t m a=>Event t Bool -> m (DynIsCon t a) -> m (DynIsCon t a)
hideableW visEv w = do
  visDyn <- holdDyn False visEv
  let attrsDyn = (\x -> if x then visibleCSS else hiddenCSS) <$> visDyn
  elDynAttr "div" attrsDyn w

hiddenCSS::M.Map T.Text T.Text
hiddenCSS  = "style" =: "display: none"

visibleCSS::M.Map T.Text T.Text
visibleCSS = "style" =: "display: inline"

sumChooser::WidgetConstraints t m a=>[(ConstructorName, (m :.: (DynIsCon t)) a)]->m (DynMaybe t a)
sumChooser namedWidgets = mdo
  let (conNames, widgets') = unzip namedWidgets
      widgets = unComp <$> widgets'
      indexedNames = zip [1..] (T.pack <$> conNames)
      boolList n = (==n) <$> [1..]
      visWidgets = (\(vd,w) -> hideableW vd w) <$> zip visDyns widgets
      ddConfig = DropdownConfig inputIndexEv (constDyn mempty)
  chooserEv <- _dropdown_change <$> dropdown 1 (constDyn $ M.fromList indexedNames) ddConfig
  dynIsCons <- sequenceA visWidgets -- [DynIsCon t a]
  let eventAs = (fmapMaybe id . updated . fmap unIsCon . unComp) <$> dynIsCons -- [Event t a]
      visEvsInput = updated . isConDyn <$> dynIsCons -- [Event t Bool]
      inputIndexEv = whichFired eventAs
      chooserEvs = chooserIndexedEvs chooserEv (length conNames)
      visDyns = zipWith (\a b -> leftmost [a,b]) visEvsInput chooserEvs
  Comp <$> holdDyn Nothing (Just <$> leftmost eventAs)

chooserIndexedEvs::Reflex t=>Event t Int -> Int -> [Event t Bool]
chooserIndexedEvs intEv size = (\n -> (==n) <$> intEv) <$> [1..size]

class WidgetConstraints t m a => TestBuilder t m a where
  build::DynMaybe t a -> m (DynMaybe t a)

q::Reflex t=>(DynMaybe t :.: IsCon) a -> DynMaybe t a
q = Comp . fmap (join . fmap unIsCon) . unComp . unComp

instance TestBuilder t m a=>Map (DynMaybe t :.: IsCon) (m :.: DynIsCon t) a where
  doMap = Comp . fmap dm2di . build . q

buildSum::(Generic a, HasDatatypeInfo a, WidgetConstraints t m a, Applicative m, All2 (Map (DynMaybe t :.: IsCon) (m :.: (DynIsCon t))) (Code a))=>DynMaybe t a->m (DynMaybe t a)
buildSum = sumChooser . functorDoPerConstructor' doAndSequence'



instance WidgetConstraints t m Int => TestBuilder t m Int where
  build = fieldWidget

instance WidgetConstraints t m Double => TestBuilder t m Double where
  build = fieldWidget

instance WidgetConstraints t m T.Text => TestBuilder t m T.Text where
  build = fieldWidget







