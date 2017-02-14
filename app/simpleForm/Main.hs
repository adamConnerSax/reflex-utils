{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE RecursiveDo               #-}
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
import           Reflex.Dom.Contrib.SimpleForm
import           Reflex.Dom.Contrib.SimpleForm.Configuration
import           Reflex.Dom.Contrib.SimpleForm.Instances     (SimpleFormInstanceC)
--import DataBuilder

import           Css

--It's easy to add validation (via newtype wrapper) for things isomorphic to already buildable things
newtype Age = Age { unAge::Int } deriving (Show)

validAge::Age->Either T.Text Age
validAge a@(Age x) = if (x >= 0) then Right a else Left "Age must be > 0"

instance SimpleFormInstanceC t m => Builder (SimpleFormR t m) Age where
  buildA mFn ma = SimpleFormR $ mdo
    let labelCfg = LabelConfig "Age" M.empty
        inputCfg = InputElementConfig (Just "35") (Just "Age") (Just labelCfg)
    unSF $ liftF (setInputConfig inputCfg) $ buildValidated validAge unAge Age mFn ma

newtype EmailAddress = EmailAddress { unEmailAddress::T.Text } deriving (Show)

validEmail::EmailAddress->Either T.Text EmailAddress
validEmail ea@(EmailAddress address) = let
  (userPart,domainPart) = T.breakOn "@" address
  valid = (T.length userPart >= 1) && (T.length domainPart >= 2)
  in if valid then Right ea else Left "Email address must be of the form a@b"

instance SimpleFormInstanceC t m => Builder (SimpleFormR t m) EmailAddress where
  buildA mFn ma =
    let labelCfg = LabelConfig "Email" M.empty
        inputCfg = InputElementConfig (Just "yourname@emailprovider.com") (Just "Email") (Just labelCfg)
    in liftF (setInputConfig inputCfg) $ buildValidated validEmail unEmailAddress EmailAddress mFn ma

newtype Name = Name { unName::T.Text } deriving (Show)
instance SimpleFormInstanceC t m => Builder (SimpleFormR t m)  Name where
  buildA mFn ma =
    let labelCfg = LabelConfig "Name" M.empty
        inputCfg = InputElementConfig (Just "John Doe") (Just "Name") (Just labelCfg)
    in liftF (setInputConfig inputCfg) $ Name <$> buildA mFn (unName <$> ma)

-- a simple structure
data User = User { name::Name, email::EmailAddress, age::Age } deriving (GHC.Generic,Show)

instance Generic User
instance HasDatatypeInfo User
instance SimpleFormInstanceC t m=>Builder (SimpleFormR t m) User where
  buildA mFN = liftF sfCol . gBuildA mFN



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
instance SimpleFormInstanceC t m=>Builder (SimpleFormR t m) ReadableType where
  buildA = buildReadMaybe



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
instance SimpleFormInstanceC t m=>Builder (SimpleFormR t m) A where
  buildA mFN = liftF sfRow . gBuildA mFN

instance Generic B
instance HasDatatypeInfo B
instance SimpleFormInstanceC t m=>Builder (SimpleFormR t m) B where
  buildA mFN = liftF (fieldSet "B" . sfRow) . gBuildA mFN

instance Generic MyMap
instance HasDatatypeInfo MyMap
instance SimpleFormInstanceC t m=>Builder (SimpleFormR t m) MyMap

instance Generic C
instance HasDatatypeInfo C
instance SimpleFormInstanceC t m=>Builder (SimpleFormR t m) C where
  buildA mFN = liftF (fieldSet "C" . sfCol) . gBuildA mFN

-- More layout options are available if you write custom instances.
-- handwritten single constructor instance
instance SimpleFormInstanceC t m=>Builder (SimpleFormR t m) BRec where
  buildA mFN mBRec = SimpleFormR $ do
    let b1 = buildA Nothing (oneB <$> mBRec)
        b2 = liftF (sfCenter LayoutHorizontal) $ buildA Nothing (seqOfA <$> mBRec)
        b3 = liftF (sfCenter LayoutHorizontal) $ buildA Nothing (hashSetOfString <$> mBRec)
    sfRow . unSF $ (BRec <$> b1 <*> b2 <*> b3)


-- handwritten sum instance for DateOrDateTime.  This is more complex because you need to know which, if any, matched the input.
buildDate::SimpleFormInstanceC t m=>Maybe FieldName->Maybe DateOrDateTime->MDWrapped (SimpleFormR t m) DateOrDateTime
buildDate mFN ms = MDWrapped matched ("Date",mFN) bldr where
--  inputCfg = InputElementConfig (Just "1/1/2001") (Just "Date") Nothing
  (matched,mDay) = case ms of
    Just (D day) -> (True,Just day)
    _            -> (False, Nothing)
  bldr = D <$> buildA Nothing mDay

buildDateTime::SimpleFormInstanceC t m=>Maybe FieldName->Maybe DateOrDateTime->MDWrapped (SimpleFormR t m) DateOrDateTime
buildDateTime mFN ms = MDWrapped matched ("DateTime",mFN) bldr where
  (matched,mDateTime) = case ms of
    Just (DT dt) -> (True,Just dt)
    _            -> (False, Nothing)
  bldr = DT <$> buildA Nothing mDateTime

instance SimpleFormInstanceC t m=>Builder (SimpleFormR t m) DateOrDateTime where
  buildA = buildAFromConList [buildDate,buildDateTime]

-- put some data in for demo purposes
b1 = B 12 [AI 10, AS "Hello" Square, AC Green, AI 4, AS "Goodbye" Circle]
b2 = B 4 [AI 1, AS "Hola" Triangle, AS "Adios" Circle, ADT (D (fromGregorian 1991 6 3)) ]
c = C 3.14159 (MyMap (M.fromList [("b1",b1),("b2",b2)])) (BRec (B 42 []) Seq.empty HS.empty)


testComplexForm::(SimpleFormInstanceC t m, MonadIO (PushM t))=>SimpleFormConfiguration t m -> m ()
testComplexForm cfg = do
  el "p" $ text ""
  el "h2" $ text "From a nested data structure, one with sum types and containers. Output is a Dynamic, rather than event based via a \"submit\" button."
  cDynM<- flexFill LayoutRight $ makeSimpleForm cfg (Just c)
  el "p" $ text "C from form:"
  dynText ((T.pack . ppShow) <$> unDynValidation cDynM)
  el "p" $ text "Observed C:"
  el "p" blank
  _ <- flexFill LayoutRight $ observeDynValidation cfg cDynM
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
main = run 3702 simpleFormMain
#endif

#ifdef USE_GHCJS
main :: IO ()
main = simpleFormMain
#endif
