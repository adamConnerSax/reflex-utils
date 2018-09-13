{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
module Main where

import           Control.Lens                                        (view,
                                                                      (^.))
import           Control.Monad                                       (foldM)
import           Control.Monad.Fix                                   (MonadFix)
import           Control.Monad.IO.Class                              as IOC (MonadIO)
import           Control.Monad.Reader                                (ask,
                                                                      runReaderT)
import           Data.Functor.Compose                                (Compose (Compose),
                                                                      getCompose)

import           Data.Monoid                                         ((<>))
import qualified GHC.Generics                                        as GHC
import           Prelude                                             hiding
                                                                      (div, rem,
                                                                      span)
import           Text.Show.Pretty                                    (ppShow)

import           Data.ByteString                                     (ByteString)
import           Data.Default                                        (def)
import qualified Data.HashSet                                        as HS
import qualified Data.Map                                            as M
import qualified Data.Sequence                                       as Seq
import qualified Data.Set                                            as Set
import qualified Data.Text                                           as T
import           Data.Time.Calendar                                  (Day (..), fromGregorian)
import           Data.Time.Clock                                     (UTCTime (..))
-- for a validation example...
import           Control.Lens.Iso                                    (iso)
import           Data.Proxy                                          (Proxy (..))
import           Data.Validation                                     (AccValidation (..))

import           Reflex
import qualified Reflex.Dom.Contrib.Widgets.Common                   as RDC
import           Reflex.Dom.Core                                     hiding (InputElementConfig)

import           GHCJS.DOM.Types                                     (JSM)
import           Reflex.Dom.Contrib.CssUtils                         (CssLink, CssLinks (..),
                                                                      headElt)
import           Reflex.Dom.Contrib.Layout.ClayUtils                 (cssToBS)
import           Reflex.Dom.Contrib.Layout.FlexLayout                (flexCol',
                                                                      flexCssBS,
                                                                      flexFill,
                                                                      flexItem',
                                                                      flexRow')
import           Reflex.Dom.Contrib.Layout.TabLayout
import           Reflex.Dom.Contrib.Layout.Types                     (CssClass (..),
                                                                      CssClasses (..),
                                                                      LayoutDirection (..),
                                                                      LayoutOrientation (..),
                                                                      emptyCss,
                                                                      oneClass)
import           Reflex.Dom.Contrib.ReflexConstraints                (MonadWidgetExtraC)
import           Reflex.Dom.Contrib.Widgets.WidgetResult             (widgetResultToDynamic)

#ifdef USE_WKWEBVIEW
--import           Language.Javascript.JSaddle.WKWebView               (run)
#endif

#ifdef USE_WARP
import           Language.Javascript.JSaddle.Warp                    (run)
#endif

--import Reflex.Dom.Contrib.Layout.LayoutP (doUnoptimizedLayout,doOptimizedLayout)
import           DataBuilder                                         as B
import           Reflex.Dom.Contrib.FormBuilder
import           Reflex.Dom.Contrib.FormBuilder.Configuration
import           Reflex.Dom.Contrib.FormBuilder.Instances            (FormInstanceC)
import           Reflex.Dom.Contrib.FormBuilder.Instances.Containers (DisplayCollection,
                                                                      DisplayCollection (..),
                                                                      formCollectionEditor,
                                                                      hideKeyEditVal,
                                                                      newItemWidget,
                                                                      showKeyEditVal)

import           Css

import qualified System.Process                                      as SP

--It's easy to add validation (via newtype wrapper)
newtype Age = Age { unAge::Int } deriving (Show, Read)

liftValidation :: (a->Bool)->(a->T.Text)->FormValidator a
liftValidation test msg = (\a -> if test a then AccSuccess a else AccFailure [FInvalid (msg a)])

instance B.Validatable FValidation Age where
  validator = liftValidation ((>0) . unAge) (const "Age must be > 0")

instance FormInstanceC t m => FormBuilder t m Age where
  buildForm va mFn dma =
    let labelF = labelForm "Age" "Age" "35" emptyCss
        vInt = liftValidation (>0) (const "Age must be > 0")
    in validateForm va . (fmap Age) $ labelF $ buildForm vInt mFn (unAge <$> dma)

newtype EmailAddress = EmailAddress { unEmailAddress::T.Text } deriving (Show)

validEmail::T.Text->Bool
validEmail address = let
  (userPart,domainPart) = T.breakOn "@" address
  in (T.length userPart >= 1) && (T.length domainPart >= 2)
--  in if valid then Right ea else Left "Email address must be of the form a@b"

instance Validatable FValidation EmailAddress where
  validator = liftValidation (validEmail . unEmailAddress) (const "Email address must be of the form a@b")

instance FormInstanceC t m => FormBuilder t m EmailAddress where
  buildForm va mFn dma =
    let labelF = labelForm "Email" "Email" "yourname@emailprovider.com" emptyCss
        vText = liftValidation validEmail (const "Email address must be of the form a@b")
    in validateForm va . (fmap EmailAddress) $ labelF $ buildForm vText mFn (unEmailAddress <$> dma)

newtype Name = Name { unName::T.Text } deriving (Show)
instance B.Validatable FValidation Name -- uses default which is that everything is valid

instance FormInstanceC t m => FormBuilder t m Name where
  buildForm va mFn dma =
    validateForm va $ labelForm "Name" "Name" "John Doe" emptyCss $ Name <$> buildVForm mFn (unName <$> dma)

-- a simple structure
data User = User { name::Name, email::EmailAddress, age::Age } deriving (GHC.Generic,Show)

instance Generic User
instance HasDatatypeInfo User
instance FormInstanceC t m=>FormBuilder t m User where
  buildForm va mFN = liftF fCol . gBuildFormValidated va mFN

testUserForm :: FormInstanceC t m =>FormConfiguration t m->m ()
testUserForm cfg = do
  let user = User (Name "Adam") (EmailAddress "adam@adam") (Age 45)
  el "p" $ text ""
  el "h2" $ text "From a simple data structure, Output is an event, fired when submit button is clicked but only if data is of right types and valid."
  newUserEv <- flexFill LayoutRight $ formWithSubmitAction cfg (Just user) (buttonNoSubmit' "submit")
  curUserDyn <- foldDyn const user newUserEv
  dynText (printUser <$> curUserDyn)

printUser::User->T.Text
printUser (User (Name n) (EmailAddress ea) (Age a)) = "User with name " <> n <> " email " <> ea <> " and age " <> (T.pack $ show a)

userFormTab::FormInstanceC t m=>FormConfiguration t m -> TabInfo t m ()
userFormTab cfg = TabInfo "userFormTab" (constDyn ("Classic Form", M.empty)) $ testUserForm cfg

buttonNoSubmit'::DomBuilder t m=>T.Text -> m (Event t ())
buttonNoSubmit' t = (domEvent Click . fst) <$> elAttr' "button" ("type" =: "button") (text t)

-- Some types to demonstrate what we can make into a form
data Color = Green | Yellow | Red deriving (Show,Read,Enum,Bounded,Eq,Ord,GHC.Generic)

data Shape = Square | Circle | Triangle deriving (Show,Read,Enum,Bounded,Eq,Ord,GHC.Generic)

data DateOrDateTime = D Day | DT UTCTime deriving (Show, Read, GHC.Generic)


-- Anything with a Read instance can be built using buildReadMaybe
data ReadableType = RTI Int | RTS String deriving (Show,Read)
instance FormInstanceC t m=>FormBuilder t m ReadableType where
  buildForm = buildDynReadMaybe


--We'll put these all in a Sum type to show how the form building handles that
data A = AI Int | AS String Shape | AC Color | AM (Maybe Double) | AB Bool | ADT DateOrDateTime | AET (Either (Shape,Color) (Shape,Int,Int)) | ART ReadableType | AA Age deriving (Show,GHC.Generic,Read)

newtype ListOfA = ListOfA { unListOfA :: [A] } deriving (Show, GHC.Generic)

--And then put those sum types in some other containerized contexts
data B = B { int::Int, listOfA::ListOfA } deriving (Show,GHC.Generic)

newtype MyMap = MyMap { map_String_B :: M.Map String B } deriving (Show,GHC.Generic)

newtype SMap a = SMap { unSMap :: M.Map T.Text a } deriving (Show, GHC.Generic)
newtype LMap a = LMap { unLMap :: M.Map T.Text a } deriving (Show, GHC.Generic)

data BRec = BRec { oneB::B, seqOfA::Seq.Seq A} deriving (Show)

data C = C { doubleC::Double, myMap::MyMap,  brec::BRec } deriving (Show,GHC.Generic)

-- generic instances
-- NB: "Generic" below is the Generics.SOP sort.
-- NB: You don't need the "buildA .. = .. gBuildA .. " lines if the default formatting is okay.  But this allows you to insert layout on a per type basis.

instance Generic A
instance HasDatatypeInfo A
instance FormInstanceC t m=>FormBuilder t m A where
  buildForm va mFN = liftF fRow . gBuildFormValidated va mFN

{-
instance Generic ListOfA
instance HasDatatypeInfo ListOfA
instance FormInstanceC t m => FormBuilder t m ListOfA
-}

convertValidator :: FormValidator ListOfA -> FormValidator [A]
convertValidator vLA = fmap (\(ListOfA x) -> x) . vLA . ListOfA

instance (FormInstanceC t m, VFormBuilderC t m A) => FormBuilder t m ListOfA where
  buildForm va mFN =
    let newItemW = newItemWidget (Proxy :: Proxy []) (Proxy :: Proxy A)
    in fmap ListOfA . formCollectionEditor (DisplayEach (constDyn M.empty) (T.pack . show)) hideKeyEditVal newItemW . fmap (\(ListOfA x)->x) where

instance Generic B
instance HasDatatypeInfo B
instance FormInstanceC t m=>FormBuilder t m B where
  buildForm va mFN = liftF (fieldSet "B" . fRow) . gBuildFormValidated va mFN

--instance Generic MyMap
--instance HasDatatypeInfo MyMap
instance FormInstanceC t m=>FormBuilder t m MyMap where
  buildForm va mFN =
    let va' = fmap map_String_B . va . MyMap
    in fmap MyMap . buildForm va' mFN . fmap map_String_B

instance Generic (SMap a)
instance HasDatatypeInfo (SMap a)
instance (FormInstanceC t m, VFormBuilderC t m a) => FormBuilder t m (SMap a) where
  buildForm va mFN =
    let va' = fmap unSMap . va . SMap
    in fmap SMap . buildForm va' mFN . fmap unSMap

instance Generic (LMap a)
instance HasDatatypeInfo (LMap a)
instance (FormInstanceC t m, VFormBuilderC t m a) => FormBuilder t m (LMap a)

instance Generic C
instance HasDatatypeInfo C
instance FormInstanceC t m=>FormBuilder t m C where
  buildForm va mFN = liftF (fieldSet "C" . fCol) . gBuildFormValidated va mFN


-- More layout options are available if you write custom instances.
-- handwritten single constructor instance
instance FormInstanceC t m=>FormBuilder t m BRec where
  buildForm va mFN mBRecDyn = validateForm va . makeForm $ do
    let b1 = buildVForm Nothing (oneB <$> mBRecDyn)
        b2 = liftF (fCenter LayoutHorizontal) $ buildVForm Nothing (seqOfA <$> mBRecDyn)
--        b3 = liftF (fCenter LayoutHorizontal) $ buildVForm Nothing (hashSetOfString <$> mBRecDyn)
    fRow . unF $ (BRec <$> b1 <*> b2)


-- handwritten sum instance for DateOrDateTime.  This is more complex because you need to know which, if any, matched the input.
buildDateOrDateTime::FormInstanceC t m
  =>FormValidator DateOrDateTime
  -> Maybe FieldName
  -> FormValue t DateOrDateTime
  -> Form t m DateOrDateTime
buildDateOrDateTime va mFN fvma =
  let mdWrapped = buildFMDWrappedList mFN fvma
      customizeWidget (MDWrapped hd (cn,mfn) w) = case cn of
        "Date"     -> MDWrapped hd (cn,mfn) w
        "DateTime" -> MDWrapped hd (cn,mfn) w
        _          -> MDWrapped hd (cn,mfn) w
  in fgvToForm . B.bSum $ customizeWidget <$> mdWrapped

instance Generic DateOrDateTime
instance HasDatatypeInfo DateOrDateTime
instance FormInstanceC t m=>FormBuilder t m DateOrDateTime where
  buildForm = buildDateOrDateTime

-- put some data in for demo purposes
lOfInt :: [Int] = [12,10,5,12]
lOfA1 = ListOfA [AI 10, AS "Hello" Square, AC Green, AI 4, AS "Goodbye" Circle]
lOfA2 = ListOfA [AI 1, AS "Hola" Triangle, AS "Adios" Circle, ADT (D (fromGregorian 1991 6 3)) ]
b1 = B 12 $ lOfA1
b2 = B 4 $ lOfA2

sm = SMap $ M.fromList [("a",1.0 :: Double),("b", 2)]
lm = LMap $ M.fromList [("a",1.0 :: Double),("b", 2)]

c = C 3.14159 (MyMap (M.fromList [("b1",b1),("b2",b2)])) (BRec (B 42 (ListOfA [])) Seq.empty)

listOfList :: [[Int]] = [[1,2],[3,4]]

testMap::M.Map T.Text Int
testMap = M.fromList [("A",1),("B",2)]

mapOfMap :: M.Map T.Text (M.Map T.Text T.Text)
mapOfMap = M.fromList [("MapA",M.fromList [("A","a"),("B","b")]),("MapB",M.fromList [("C","c"),("E","e")])]

mapOfList :: M.Map T.Text [T.Text]
mapOfList = M.fromList [("ListA",["a","b"]),("ListB",["c","e"])]

hs :: HS.HashSet String
hs = HS.fromList ["a","b"]

hseq :: Seq.Seq String
hseq = Seq.fromList ["a","b"]

seqA :: Seq.Seq A
seqA = Seq.fromList (unListOfA lOfA2)

bRec :: BRec
bRec = BRec b1 (Seq.fromList (unListOfA lOfA2))

testForm :: (FormInstanceC t m, VFormBuilderC t m a, Show a) => FormConfiguration t m -> a -> m ()
testForm cfg x = do
  el "p" $ text ""
  fv <- flexFill LayoutRight $ dynamicForm cfg (Just x)
  el "p" $ text "dynText:"
  dynText (fmap (T.pack . ppShow) . widgetResultToDynamic $ getCompose fv)
  el "p" $ text "Input into new form:"
  el "p" blank
  fv' <- flexFill LayoutRight $ dynamicFormOfFormValue cfg fv
  el "p" $ text "Observed:"
  el "p" blank
  _ <- flexFill LayoutRight $ observeDynamic cfg (fmap avToMaybe . widgetResultToDynamic $ getCompose fv')
  return ()

testContainers :: FormInstanceC t m => FormConfiguration t m -> m ()
testContainers cfg = do
  let tests =
        [
--          ("List of Int" , testForm cfg lOfInt)
--        , ("Seq of String" , testForm cfg hseq)
--        , ("List of A", testForm cfg lOfA1)
--        , ("Seq of A", testForm cfg seqA)
            ("[[Int]]", testForm cfg listOfList)
--        , ("Map Text Int", testForm cfg testMap)
          ,  ("Map Text [Text]", testForm cfg mapOfList)
          , ("Map Text (Map Text Text)", testForm cfg mapOfMap)
--        , ("Seq String", testForm cfg hseq)
--        , ("SelectView Map", testForm cfg sm)
--        , ("Record with Container", testForm cfg b1)
--        , ("Record of Containers", testForm cfg bRec)
--        , ("Combo", testForm cfg c)
        ]
      tabs = (\(n,w) -> TabInfo n (constDyn (n,M.empty)) w) <$> tests
  _ <- staticTabbedLayout def (head tabs) tabs
  return ()

containersTab::FormInstanceC t m => FormConfiguration t m -> TabInfo t m ()
containersTab cfg = TabInfo "containersTab" (constDyn ("Container Forms", M.empty))  $ testContainers cfg

flowTestWidget::(DomBuilder t m, HasDocument m, MonadWidgetExtraC t m, MonadFix m, MonadHold t m, PostBuild t m)=>Int->m (Dynamic t String)
flowTestWidget n = do
  text "Are all these checked?"
  boolDyns <- sequence $ take n $ Prelude.repeat (RDC._hwidget_value <$> RDC.htmlCheckbox (RDC.WidgetConfig never True (constDyn mempty)))
  allTrueDyn <- foldM (\x bDyn -> combineDyn (&&) x bDyn) (constDyn True) boolDyns
  forDyn allTrueDyn $ \b -> if b then "All Checked!" else "Some Unchecked."


testFlow :: FormInstanceC t m => FormConfiguration t m->m ()
testFlow cfg = do
  el "p" $ text ""
  el "h2" $ text "Observe a \"flow\", that is directly see input and output of a function of type WidgetMonad m=>a -> m b"
  el "h2" $ text "In this case, WidgetMonad m=>Int -> Dynamic t (String)"
  _ <- observeFlow cfg flowTestWidget 2
  return ()

flowTestTab :: FormInstanceC t m => FormConfiguration t m -> TabInfo t m ()
flowTestTab cfg = TabInfo "flowTestTab" (constDyn ("Flow Example", M.empty)) $ testFlow cfg

{-
-- test WidgetResult
leftWhenNotRight :: Reflex t => Event t a -> Event t b -> Event t a
leftWhenNotRight leftEv rightEv = fmapMaybe id $ leftmost [Nothing <$ rightEv, Just <$> leftEv]

changeOnlyEv :: Reflex t => Dynamic t a -> Dynamic t b -> Event t b
changeOnlyEv input result = leftWhenNotRight (updated result) (updated input)

changeOnlyWidget :: FormInstanceC t m => (Dynamic t a -> m (Dynamic t b)) -> Dynamic t a -> m (Event t b)
changeOnlyWidget dynamicWidget inputDyn =  changeOnlyEv inputDyn <$> dynamicWidget inputDyn

changeOnlyFormWidget :: FormInstanceC t m => (FormValue t a -> m (FormValue t b)) -> FormValue t a -> m (Event t (FValidation b))
changeOnlyFormWidget formW inputFV =
  let widget = fmap getCompose . formW . Compose
      inputDyn = getCompose inputFV
  in changeOnlyWidget widget inputDyn

testCycleBreaker :: FormInstanceC t m => FormConfiguration t m -> m ()
testCycleBreaker cfg = do
  let startValue = 2 :: Int
  el "p" (text "")
  el "p" (text "Input to both widgets")
  inputFV <- runForm cfg $ buildVForm Nothing (constFormValue startValue)
  el "p" (text "")
  el "p" (text "Regular form")
  regularEv <- updated . getCompose <$> (runForm cfg $ buildVForm Nothing inputFV)
  regularDyn <- holdDyn (AccSuccess startValue) regularEv
  dynText (T.pack . show <$> regularDyn)
  el "p" (text "")
  el "p" (text "Change only form")
  changeOnlyEvent <- changeOnlyFormWidget (runForm cfg . buildVForm Nothing) inputFV
  changeOnlyDyn <- holdDyn (AccSuccess startValue) changeOnlyEvent
  dynText (T.pack . show <$> changeOnlyDyn)

cycleBreakerTab :: FormInstanceC t m => FormConfiguration t m -> TabInfo t m ()
cycleBreakerTab cfg = TabInfo "cycleBreakerTestTab" (constDyn ("Cycle Breaker Example", M.empty)) $ testCycleBreaker cfg
-}


test :: FormInstanceC t m => FormConfiguration t m -> m ()
test cfg = do
  el "p" (text "")
  el "br" blank
  staticTabbedLayout def (containersTab cfg)
    [
  --    cycleBreakerTab cfg
      userFormTab cfg
    , containersTab cfg
    , flowTestTab cfg
    ]
  return ()


linkedCss::CssLinks
linkedCss = CssLinks []

customizeConfig::FormConfiguration t m -> FormConfiguration t m
customizeConfig = id

includedCss = def --bootstrapSFIncludedCss
toLink = linkedCss <> (cssToLink includedCss)
toEmbed = flexCssBS <> tabCssBS <> (cssToEmbed includedCss)

formBuilderMain  :: JSM ()
formBuilderMain  =
  mainWidgetWithHead (headElt "formBuilder demo" toLink toEmbed) $ test (customizeConfig def)

{--
#ifdef USE_WKWEBVIEW
main::IO ()
main = run formBuilderMain
#endif
--}


--this needs fixing if I want support webkit or whatever else as well.
#ifndef ghcjs_HOST_OS
main::IO ()
main = do
  let port :: Int = 3702
  _ <- SP.spawnProcess "open" ["-a","/Applications/Safari.App/Contents/MacOs/Safari", "http://localhost:" ++ show port]
--  _ <- SP.spawnProcess "open" ["http://localhost:" ++ show port]
  run port formBuilderMain
#endif


#ifdef ghcjs_HOST_OS
main :: IO ()
main = formBuilderMain
#endif

