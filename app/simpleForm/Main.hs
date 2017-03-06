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

import           Control.Lens                                (view, (^.))
import           Control.Monad                               (foldM)
import           Control.Monad.Fix                           (MonadFix)
import           Control.Monad.IO.Class                      as IOC (MonadIO)
import           Control.Monad.Reader                        (ask)

import           Data.Monoid                                 ((<>))
import qualified GHC.Generics                                as GHC
import           Prelude                                     hiding (div, rem,
                                                              span)
import           Text.Show.Pretty                            (ppShow)

import           Data.ByteString                             (ByteString)
import           Data.Default                                (def)
import qualified Data.HashSet                                as HS
import qualified Data.Map                                    as M
import qualified Data.Sequence                               as Seq
import qualified Data.Text                                   as T
import           Data.Time.Calendar                          (Day (..),
                                                              fromGregorian)
import           Data.Time.Clock                             (UTCTime (..))
-- for a validation example...
import           Control.Lens.Iso                            (iso)
import           Data.Validation                             (AccValidation (..))

import           Reflex
import qualified Reflex.Dom.Contrib.Widgets.Common           as RDC
import           Reflex.Dom.Core                             hiding (InputElementConfig)

import           GHCJS.DOM.Types                             (JSM)
import           Reflex.Dom.Contrib.CssUtils                 (CssLink,
                                                              CssLinks (..),
                                                              headElt)
import           Reflex.Dom.Contrib.Layout.ClayUtils         (cssToBS)
import           Reflex.Dom.Contrib.Layout.FlexLayout        (flexCol',
                                                              flexCssBS,
                                                              flexFill,
                                                              flexItem',
                                                              flexRow')
import           Reflex.Dom.Contrib.Layout.TabLayout
import           Reflex.Dom.Contrib.Layout.Types             (CssClass (..),
                                                              CssClasses (..),
                                                              LayoutDirection (..),
                                                              LayoutOrientation (..),
                                                              emptyCss,
                                                              oneClass)
import           Reflex.Dom.Contrib.ReflexConstraints        (MonadWidgetExtraC)

#ifdef USE_WKWEBVIEW
import           Language.Javascript.JSaddle.WKWebView       (run)
#endif

#ifdef USE_WARP
import           Language.Javascript.JSaddle.Warp            (run)
#endif

--import Reflex.Dom.Contrib.Layout.LayoutP (doUnoptimizedLayout,doOptimizedLayout)
import           DataBuilder                                 as B
import           Reflex.Dom.Contrib.SimpleForm
import           Reflex.Dom.Contrib.SimpleForm.Configuration
import           Reflex.Dom.Contrib.SimpleForm.Instances     (SimpleFormInstanceC)

import           Css

import qualified System.Process                              as SP

--It's easy to add validation (via newtype wrapper)
newtype Age = Age { unAge::Int } deriving (Show)

liftValidation::(a->Bool)->(a->T.Text)->FormValidator a
liftValidation test msg = (\a -> if test a then AccSuccess a else AccFailure [SFInvalid (msg a)])

instance B.Validatable SFValidation Age where
  validator = liftValidation ((>0) . unAge) (const "Age must be > 0")

instance SimpleFormInstanceC t m => FormBuilder t m Age where
  buildForm va mFn maDyn =
    let labelCfg = LabelConfig "Age" M.empty
        inputCfg = InputElementConfig (Just "35") (Just "Age") (Just labelCfg)
        vInt = liftValidation (>0) (const "Age must be > 0")
    in validateForm va . (fmap Age) $ liftF (setInputConfig inputCfg) $ buildForm vInt mFn (fmap unAge <$> maDyn)

newtype EmailAddress = EmailAddress { unEmailAddress::T.Text } deriving (Show)

validEmail::T.Text->Bool
validEmail address = let
  (userPart,domainPart) = T.breakOn "@" address
  in (T.length userPart >= 1) && (T.length domainPart >= 2)
--  in if valid then Right ea else Left "Email address must be of the form a@b"

instance Validatable SFValidation EmailAddress where
  validator = liftValidation (validEmail . unEmailAddress) (const "Email address must be of the form a@b")

instance SimpleFormInstanceC t m => FormBuilder t m EmailAddress where
  buildForm va mFn maDyn =
    let labelCfg = LabelConfig "Email" M.empty
        inputCfg = InputElementConfig (Just "yourname@emailprovider.com") (Just "Email") (Just labelCfg)
        vText = liftValidation validEmail (const "Email address must be of the form a@b")
    in validateForm va . (fmap EmailAddress) $ liftF (setInputConfig inputCfg) $ buildForm vText mFn (fmap unEmailAddress <$> maDyn)

newtype Name = Name { unName::T.Text } deriving (Show)
instance B.Validatable SFValidation Name -- uses default which is that everything is valid

instance SimpleFormInstanceC t m => FormBuilder t m Name where
  buildForm va mFn maDyn =
    let labelCfg = LabelConfig "Name" M.empty
        inputCfg = InputElementConfig (Just "John Doe") (Just "Name") (Just labelCfg)
    in validateForm va $ liftF (setInputConfig inputCfg) $ Name <$> buildForm' mFn (fmap unName <$> maDyn)

-- a simple structure
data User = User { name::Name, email::EmailAddress, age::Age } deriving (GHC.Generic,Show)

instance Generic User
instance HasDatatypeInfo User
instance SimpleFormInstanceC t m=>FormBuilder t m User where
  buildForm va mFN = liftF sfCol . fgvToSimpleFormR . gBuildValidated va mFN

testUserForm::(SimpleFormInstanceC t m, MonadIO (PushM t))=>SimpleFormConfiguration t m->m ()
testUserForm cfg = do
  let user = User (Name "Adam") (EmailAddress "adam@adam") (Age 45)
  el "p" $ text ""
  el "h2" $ text "From a simple data structure, Output is an event, fired when submit button is clicked but only if data is of right types and valid."
  newUserEv <- flexFill LayoutRight $ makeSimpleForm' cfg (Just user) (buttonNoSubmit' "submit")
  curUserDyn <- foldDyn const user newUserEv
  dynText (printUser <$> curUserDyn)

printUser::User->T.Text
printUser (User (Name n) (EmailAddress ea) (Age a)) = "User with name " <> n <> " email " <> ea <> " and age " <> (T.pack $ show a)

userFormTab::SimpleFormInstanceC t m=>SimpleFormConfiguration t m -> TabInfo t m ()
userFormTab cfg = TabInfo "userFormTab" "Classic Form" $ testUserForm cfg

buttonNoSubmit'::DomBuilder t m=>T.Text -> m (Event t ())
buttonNoSubmit' t = (domEvent Click . fst) <$> elAttr' "button" ("type" =: "button") (text t)

-- Some types to demonstrate what we can make into a form
data Color = Green | Yellow | Red deriving (Show,Enum,Bounded,Eq,Ord,GHC.Generic)

data Shape = Square | Circle | Triangle deriving (Show,Enum,Bounded,Eq,Ord,GHC.Generic)

data DateOrDateTime = D Day | DT UTCTime deriving (Show)

-- Anything with a Read instance can be built using buildReadMaybe
data ReadableType = RTI Int | RTS String deriving (Show,Read)
instance SimpleFormInstanceC t m=>FormBuilder t m ReadableType where
  buildForm = buildDynReadMaybe


--We'll put these all in a Sum type to show how the form building handles that
data A = AI Int | AS String Shape | AC Color | AM (Maybe Double) | AB Bool | ADT DateOrDateTime | AET (Either (Shape,Color) (Shape,Int,Int)) | ART ReadableType | AA Age deriving (Show,GHC.Generic)

--And then put those sum types in some other containerized contexts
data B = B { int::Int, listOfA::[A] } deriving (Show,GHC.Generic)

newtype MyMap = MyMap { map_String_B::M.Map String B } deriving (Show,GHC.Generic)

data BRec = BRec { oneB::B, seqOfA::Seq.Seq A, hashSetOfString::HS.HashSet String } deriving (Show)

data C = C { doubleC::Double, myMap::MyMap,  brec::BRec } deriving (Show,GHC.Generic)


-- generic instances
-- NB: "Generic" below is the Generics.SOP sort.
-- NB: You don't need the "buildA .. = .. gBuildA .. " lines if the default formatting is okay.  But this allows you to insert layout on a per type basis.

instance Generic A
instance HasDatatypeInfo A
instance SimpleFormInstanceC t m=>FormBuilder t m A where
  buildForm va mFN = liftF sfRow . fgvToSimpleFormR . gBuildValidated va mFN

instance Generic B
instance HasDatatypeInfo B
instance SimpleFormInstanceC t m=>FormBuilder t m B where
  buildForm va mFN = liftF (fieldSet "B" . sfRow) . fgvToSimpleFormR . gBuildValidated va mFN

instance Generic MyMap
instance HasDatatypeInfo MyMap
instance SimpleFormInstanceC t m=>FormBuilder t m MyMap

instance Generic C
instance HasDatatypeInfo C
instance SimpleFormInstanceC t m=>FormBuilder t m C where
  buildForm va mFN = liftF (fieldSet "C" . sfCol) . fgvToSimpleFormR . gBuildValidated va mFN

-- More layout options are available if you write custom instances.
-- handwritten single constructor instance
instance SimpleFormInstanceC t m=>FormBuilder t m BRec where
  buildForm va mFN mBRecDyn = validateForm va . makeSimpleFormR $ do
    let b1 = buildForm' Nothing (fmap oneB <$> mBRecDyn)
        b2 = liftF (sfCenter LayoutHorizontal) $ buildForm' Nothing (fmap seqOfA <$> mBRecDyn)
        b3 = liftF (sfCenter LayoutHorizontal) $ buildForm' Nothing (fmap hashSetOfString <$> mBRecDyn)
    sfRow . unSF $ (BRec <$> b1 <*> b2 <*> b3)


-- handwritten sum instance for DateOrDateTime.  This is more complex because you need to know which, if any, matched the input.
buildDate::SimpleFormInstanceC t m=>FormValidator DateOrDateTime->
  Maybe FieldName->Maybe (Dynamic t DateOrDateTime)->Dynamic t (SFMDWrapped t m DateOrDateTime)
buildDate va mFN msDyn =
  let blankBuilder = simpleFormRToFGV $ validateForm va $ D <$> buildForm' Nothing Nothing
  in case msDyn of
    Nothing -> constDyn (MDWrapped False ("Date",mFN) blankBuilder)
    Just ddtDyn ->
      let f ms = MDWrapped matched ("Date",mFN) bldr where
            (matched,mDay) = case ms of
                               (D day) -> (True,Just day)
                               _       -> (False, Nothing)
            bldr = simpleFormRToFGV $ validateForm va $ D <$> buildForm' Nothing (constDyn <$> mDay)
      in f <$> ddtDyn

buildDateTime::SimpleFormInstanceC t m=>FormValidator DateOrDateTime ->
  Maybe FieldName->Maybe (Dynamic t DateOrDateTime)->Dynamic t (MDWrapped (SFR t m) (Dynamic t) SFValidation DateOrDateTime)
buildDateTime va mFN msDyn =
  let blankBuilder = simpleFormRToFGV $ validateForm va $ DT <$> buildForm' Nothing Nothing
  in case msDyn of
    Nothing -> constDyn (MDWrapped False ("DateTime",mFN) blankBuilder)
    Just ddtDyn ->
      let f ms = MDWrapped matched ("DateTime",mFN) bldr where
            (matched,mDateTime) = case ms of
                                    (DT dt) -> (True,Just dt)
                                    _       -> (False, Nothing)
            bldr = simpleFormRToFGV $ validateForm va $ DT <$> buildForm' Nothing (constDyn <$> mDateTime)
      in f <$> ddtDyn

instance SimpleFormInstanceC t m=>FormBuilder t m DateOrDateTime where
  buildForm va mFN = fgvToSimpleFormR . B.buildAFromConList [buildDate,buildDateTime] va mFN

-- put some data in for demo purposes

b1 = B 12 [AI 10, AS "Hello" Square, AC Green, AI 4, AS "Goodbye" Circle]
b2 = B 4 [AI 1, AS "Hola" Triangle, AS "Adios" Circle, ADT (D (fromGregorian 1991 6 3)) ]
c = C 3.14159 (MyMap (M.fromList [("b1",b1),("b2",b2)])) (BRec (B 42 []) Seq.empty HS.empty)

testMap::M.Map T.Text Int
testMap = M.fromList [("A",1),("B",2),("C",3)]

testComplexForm::(SimpleFormInstanceC t m, MonadIO (PushM t))=>SimpleFormConfiguration t m -> m ()
testComplexForm cfg = do
  el "p" $ text ""
  el "h2" $ text "From a nested data structure, one with sum types and containers. Output is a Dynamic, rather than event based via a \"submit\" button."
  cDynM<- flexFill LayoutRight $ makeSimpleForm cfg (Just $ AI 10)
  el "p" $ text "C from form:"
  dynText ((T.pack . ppShow) <$> unDynValidation cDynM)
  el "p" $ text "Observed C:"
  el "p" blank
  _ <- flexFill LayoutRight $ observeDynamic cfg (avToMaybe <$> unDynValidation cDynM)

  return ()

complexFormTab::SimpleFormInstanceC t m=>SimpleFormConfiguration t m -> TabInfo t m ()
complexFormTab cfg = TabInfo "complexFormTab" "Complex Example" $ testComplexForm cfg


flowTestWidget::(DomBuilder t m,MonadWidgetExtraC t m,MonadFix m,MonadHold t m,PostBuild t m)=>Int->m (Dynamic t String)
flowTestWidget n = do
  text "Are all these checked?"
  boolDyns <- sequence $ take n $ Prelude.repeat (RDC._hwidget_value <$> RDC.htmlCheckbox (RDC.WidgetConfig never True (constDyn mempty)))
  allTrueDyn <- foldM (\x bDyn -> combineDyn (&&) x bDyn) (constDyn True) boolDyns
  forDyn allTrueDyn $ \b -> if b then "All Checked!" else "Some Unchecked."


testFlow::(SimpleFormInstanceC t m, MonadIO (PushM t))=>SimpleFormConfiguration t m->m ()
testFlow cfg = do
  el "p" $ text ""
  el "h2" $ text "Observe a \"flow\", that is directly see input and output of a function of type WidgetMonad m=>a -> m b"
  el "h2" $ text "In this case, WidgetMonad m=>Int -> Dynamic t (String)"
  _ <- observeFlow cfg flowTestWidget 2
  return ()

flowTestTab::SimpleFormInstanceC t m=>SimpleFormConfiguration t m -> TabInfo t m ()
flowTestTab cfg = TabInfo "flowTestTab" "Flow Example" $ testFlow cfg

test::(SimpleFormInstanceC t m, MonadIO (PushM t))=>SimpleFormConfiguration t m -> m ()
test cfg = do
  el "p" (text "")
  el "br" blank
  staticTabbedLayout def (userFormTab cfg)
    [
      userFormTab cfg
    , complexFormTab cfg
    , flowTestTab cfg
    ]
  return ()


linkedCss::CssLinks
linkedCss = CssLinks []

customizeConfig::SimpleFormConfiguration t m -> SimpleFormConfiguration t m
customizeConfig = id

includedCss = def --bootstrapSFIncludedCss
toLink = linkedCss <> (cssToLink includedCss)
toEmbed = flexCssBS <> tabCssBS <> (cssToEmbed includedCss)

simpleFormMain  :: JSM ()
simpleFormMain  =
  mainWidgetWithHead (headElt "simpleForm demo" toLink toEmbed) $ test (customizeConfig def)


#ifdef USE_WKWEBVIEW
main::IO ()
main = run simpleFormMain
#endif

#ifdef USE_WARP
main::IO ()
main = do
  let port :: Int = 3702
  pHandle <- SP.spawnProcess "open" ["http://localhost:" ++ show port]
  run port simpleFormMain
#endif

#ifdef USE_GHCJS
main :: IO ()
main = simpleFormMain
#endif
