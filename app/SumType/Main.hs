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
  el "span" $ text "Text: "
  dynMText <- build (Comp $ constDyn (Nothing :: Maybe T.Text))
  el "br" blank
  el "span" $ text "Either Int Text: "
  dynMTE <- buildSum (LeftInt <$> dynMInt)
  el "br" blank
  dynMaybeText dynMTE
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

hideableW::ReflexConstraints t m=>Event t Bool -> m a -> m a
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
      widgets = unComp <$> widgets' -- [m (DynIsCon t a)]
      indexedNames = zip [1..] (T.pack <$> conNames) 
      visWidgets = {- zipWith hideableW visEvs -} widgets -- [m (DynIsCon t a)]  with visEvs built in 
      ddConfig = DropdownConfig inputIndexEv (constDyn mempty)
  chooserEv <- _dropdown_change <$> dropdown 1 (constDyn $ M.fromList indexedNames) ddConfig -- put dropdown in DOM
  dynIsCons <- sequenceA visWidgets -- [DynIsCon t a], put widgets in DOM
  let eventAs = (fmapMaybe id . updated . fmap unIsCon . unComp) <$> dynIsCons -- [Event t a], only firing on updates to "Just"
      visEvsInput = updated . isConDyn <$> dynIsCons -- [Event t Bool]
      inputIndexEv = whichFired eventAs -- Event t Int
--      chooserEvs = chooserIndexedEvs chooserEv (length conNames) 
--      visEvs = zipWith (\a b -> leftmost [a,b])  visEvsInput  chooserEvs
  Comp <$> holdDyn Nothing (Just <$> leftmost eventAs)

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
  
chooserIndexedEvs::Reflex t=>Event t Int -> Int -> [Event t Bool]
chooserIndexedEvs intEv size = (\n -> (==n) <$> intEv) <$> [1..size]

class WidgetConstraints t m a => TestBuilder t m a where
  build::DynMaybe t a -> m (DynMaybe t a)


q::Reflex t=>(DynMaybe t :.: IsCon) a -> DynMaybe t a
q = Comp . fmap (join . fmap unIsCon) . unComp . unComp

instance TestBuilder t m a=>Map (DynMaybe t :.: IsCon) (m :.: DynIsCon t) a where
  doMap = Comp . fmap dm2di . build . uniqDynJust . q

buildSum::forall a t m.(Generic a, HasDatatypeInfo a
          , WidgetConstraints t m a
          , All2 (Map (DynMaybe t :.: IsCon) (m :.: (DynIsCon t))) (Code a))=>DynMaybe t a->m (DynMaybe t a)
buildSum = sumChooser . functorDoPerConstructor' doAndSequence'
{-
buildSum x =
  let listOfWidgets::Reflex t=>[(m :.: (DynIsCon t)) a]
      listOfWidgets = snd <$> functorDoPerConstructor' doAndSequence' x
      widgetOfList::m [DynIsCon t a]
      widgetOfList = sequenceA $ unComp <$> listOfWidgets
      widgetOfList'::m [DynMaybe t a]
      widgetOfList' = fmap di2dm <$> widgetOfList
  in head <$> widgetOfList'
-}

instance WidgetConstraints t m Int => TestBuilder t m Int where
  build = fieldWidget

instance WidgetConstraints t m Double => TestBuilder t m Double where
  build = fieldWidget

instance WidgetConstraints t m T.Text => TestBuilder t m T.Text where
  build = fieldWidget

instance TestBuilder t m a => TestBuilder t m (Maybe a) where
  build = buildSum

instance (TestBuilder t m a, TestBuilder t m b) => TestBuilder t m (Either a b) where
  build = buildSum








