{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Monad                        (foldM)
import           Control.Monad.IO.Class               as IOC (MonadIO)
import           Control.Monad.Ref                    (Ref)
import           Control.Monad.Trans                  (MonadTrans,lift)
import           Control.Lens.Iso                     (iso)
import           Data.FileEmbed
import           Data.Monoid                          ((<>))
import qualified GHC.Generics                         as GHC
import           Prelude                              hiding (div, rem, span)
import           Data.List                            (foldl')
import           Text.Show.Pretty                     (ppShow)
--import Clay hiding (button,col,Color)
import           Data.Either
import qualified Data.HashSet                         as HS
import qualified Data.Map                             as M
import qualified Data.Sequence                        as Seq
import qualified Data.Text                            as T
import           Data.Time.Calendar                   (Day (..), fromGregorian)
import           Data.Time.Clock                      (UTCTime (..))
import           Data.Validation                      (AccValidation (..))
import           Reflex
import           Reflex.Dom
import qualified Reflex.Dom.Contrib.Widgets.Common    as RDC
import           Reflex.Dynamic.TH

import           Reflex.Dom.Contrib.Layout.ClayUtils  (cssToBS)
import           Reflex.Dom.Contrib.Layout.FlexLayout (flexCssBS, flexFillR)
import           Reflex.Dom.Contrib.Layout.Types      (CssClass (..),
                                                       CssClasses (..),
                                                       emptyCss)
                 
import           Reflex.Dom.Contrib.SimpleForm


-- Some types to demonstrate what we can make into a form
data Color = Green | Yellow | Red deriving (Show,Enum,Bounded,Eq,Ord,GHC.Generic)

data Shape = Square | Circle | Triangle deriving (Show,Enum,Bounded,Eq,Ord,GHC.Generic)

data DateOrDateTime = D Day | DT UTCTime deriving (Show)

-- Anything with a Read instance can be built using buildReadMaybe
data ReadableType = RTI Int | RTS String deriving (Show,Read)
instance SimpleFormC e t m=>Builder (SimpleFormR e t m) ReadableType where
  buildA = buildReadMaybe

--It's easy to add validation (via newtype wrapepr) for things isomorphic to already buildable things
newtype Age = Age { unAge::Int }
instance Show Age where
  show = show . unAge
  
validAge::Age->Either T.Text Age
validAge a@(Age x) = if (x >= 0) then Right a else Left "Age must be > 0" 

instance SimpleFormC e t m => Builder (SimpleFormR e t m) Age where
  buildA = buildValidated validAge unAge Age

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
instance SimpleFormC e t m=>Builder (SimpleFormR e t m) A where
  buildA mFN = liftF formRow . gBuildA mFN

instance Generic B
instance HasDatatypeInfo B
instance SimpleFormC e t m=>Builder (SimpleFormR e t m) B where
  buildA mFN = liftF (legend "B" . formRow) . gBuildA mFN

instance Generic MyMap
instance HasDatatypeInfo MyMap
instance SimpleFormC e t m=>Builder (SimpleFormR e t m) MyMap

instance Generic C
instance HasDatatypeInfo C
instance SimpleFormC e t m=>Builder (SimpleFormR e t m) C where
  buildA mFN = liftF (legend "C" . formCol) . gBuildA mFN


-- More layout options are available if you write custom instances.
-- handwritten single constructor instance
instance SimpleFormC e t m=>Builder (SimpleFormR e t m) BRec where
  buildA mFN mBRec= liftF (textOnTop "BRec" . formRow) $ BRec
               <$> buildA Nothing (oneB <$> mBRec)
               <*> liftF (textOnTop' layoutHC "Seq A") (buildA Nothing (seqOfA <$> mBRec))
               <*> liftF (textOnTop' layoutHC "HashSet String") (buildA Nothing (hashSetOfString <$> mBRec))


-- handwritten sum instance for DateOrDateTime.  This is more complex because you need to know which, if any, matched the input.
buildDate::SimpleFormC e t m=>Maybe FieldName->Maybe DateOrDateTime->MDWrapped (SimpleFormR e t m) DateOrDateTime
buildDate mFN ms = MDWrapped matched ("D",mFN) bldr where
  (matched,mDay) = case ms of
    Just (D day) -> (True,Just day)
    _            -> (False, Nothing)
  bldr = D <$> liftF (textAtLeft "Date") (buildA Nothing mDay)

buildDateTime::SimpleFormC e t m=>Maybe FieldName->Maybe DateOrDateTime->MDWrapped (SimpleFormR e t m) DateOrDateTime
buildDateTime mFN ms = MDWrapped matched ("DT",mFN) bldr where
  (matched,mDateTime) = case ms of
    Just (DT dt) -> (True,Just dt)
    _            -> (False, Nothing)
  bldr = DT <$> liftF (textAtLeft "DateTime") (buildA Nothing mDateTime)

instance SimpleFormC e t m=>Builder (SimpleFormR e t m) DateOrDateTime where
  buildA = buildAFromConList [buildDate,buildDateTime]


-- Equivalent TH-built instances
-- Can't do extra formatting.  Row and column templates are all you get.
{-
-- template derivation is simpler but may be a compile time issues for ghcjs.  And some people don't like TH.
deriveSFRowBuilder ''A
deriveSFRowBuilder ''MyMap
-}

-- put some data in for demo purposes
b1 = B 12 [AI 10, AS "Hello" Square, AC Green, AI 4, AS "Goodbye" Circle]
b2 = B 4 [AI 1, AS "Hola" Triangle, AS "Adios" Circle, ADT (D (fromGregorian 1991 6 3)) ]
c = C 3.14159 (MyMap (M.fromList [("b1",b1),("b2",b2)])) (BRec (B 42 []) Seq.empty HS.empty)

flowTestWidget::MonadWidget t m=>Int->m (Dynamic t String)
flowTestWidget n = do
  text "Are all these checked?"
  boolDyns <- sequence $ take n $ Prelude.repeat (RDC._hwidget_value <$> RDC.htmlCheckbox (RDC.WidgetConfig never True (constDyn mempty)))
  let allTrueDyn = foldl' (\x bDyn -> zipDynWith (&&) x bDyn) (constDyn True) boolDyns
  forDyn allTrueDyn $ \b -> if b then "All Checked!" else "Some Unchecked."

test::(SimpleFormC e t m, MonadIO (PushM t))=>e->m ()
test cfg = do
  cDynM<- flexFillR $ makeSimpleForm cfg (CssClass "sf-form") (Just c)
  el "p" $ text "C from form:"
  dynText ((T.pack . ppShow) <$> unDynValidation cDynM)
  el "p" $ text "Observed C:"
  el "p" blank
  _ <- flexFillR $ observeDynValidation cfg (CssClass "sf-observer") cDynM
  el "p" blank
  _ <- observeFlow cfg (CssClass "sf-form") (CssClass "sf-observer") flowTestWidget 2
  return ()

{-
   NB: AllDefault module exports DefSFCfg which is an instance of SimpleFormConfiguration with all the specific failure, sum and styling.
   Different instances of that class could be made to change styling or make it more customizable.
   That's the part to re-implement for different form behavior. Or just to use different layout functions than the ones I've
   implemented with here.
-}

demoCfg = DefSFCfg {
    cfgValidStyle = emptyCss -- (CssClasses [CssClass "sf-outline-black"])
  , cfgInvalidStyle = (CssClasses [CssClass "sf-invalid"])
  , cfgObserverStyle = (CssClasses [CssClass "sf-observer-item"])
  , cfgLabelF = Nothing
  , cfgObserver = False
  }

main  :: IO ()
main  = mainWidgetWithCss (flexCssBS
                           <> cssToBS simpleFormDefaultCss
                           <> cssToBS simpleObserverDefaultCss) $  test demoCfg




