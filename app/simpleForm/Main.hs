{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Prelude hiding (rem,div,span)
import Control.Monad (foldM)
import Control.Monad.IO.Class as IOC (MonadIO)
import Data.Monoid ((<>))
import Data.FileEmbed
import Text.Show.Pretty (ppShow)
import qualified GHC.Generics as GHC
import Clay hiding (button,col,Color)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.HashSet as HS
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..))

import Reflex
import Reflex.Dynamic.TH
import Reflex.Dom
import qualified Reflex.Dom.Contrib.Widgets.Common as RDC

import Reflex.Dom.Contrib.Layout.All (CssClasses(..),CssClass(..),emptyCss,flexCssBS,flexFillR,cssToBS)
import Reflex.Dom.Contrib.SimpleForm

{-
boxMargin m = sym margin (rem m)
cssBox m c = do
  boxMargin m
  border solid (px 2) c

cssBoxes = do
  ".demo-box-black" ? cssBox 0.1 black
  ".demo-box-red" ? cssBox 0.1 red
  ".demo-box-blue" ? cssBox 0.1 blue
  ".demo-box-green" ? cssBox 0.1 green

-}

-- Some types to demonstrate what we can make into a form
data Color = Green | Yellow | Red deriving (Show,Enum,Bounded,Eq,Ord,GHC.Generic)
data Shape = Square | Circle | Triangle deriving (Show,Enum,Bounded,Eq,Ord,GHC.Generic)
data A = AI Int | AS String Shape | AC Color | AM (Maybe Double) | AB Bool | AD Day | ADT UTCTime | AET (Either (Shape,Color) (Shape,Int,Int)) deriving (Show,GHC.Generic)
data B = B { int::Int, listOfA::[A] } deriving (Show,GHC.Generic)
newtype MyMap = MyMap { map_String_B::M.Map String B } deriving (Show,GHC.Generic)
data BRec = BRec { oneB::B, seqOfA::Seq.Seq A, hashSetOfString::HS.HashSet String } deriving (Show,GHC.Generic)
data C = C { doubleC::Double, myMap::MyMap,  brec::BRec } deriving (Show,GHC.Generic)


-- generic instances
-- NB: "Generic" below is the Generics.SOP sort.  
-- NB: You don't need the "buildA .. = .. gBuildA .. " lines if the default formatting is okay.  But this allows you to insert layout on a per type basis.
-- More layout options are available if you write custom instances. 
instance Generic A
instance HasDatatypeInfo A
instance SimpleFormC e t m=>Builder (SimpleFormR e t m) A 

instance Generic B
instance HasDatatypeInfo B
instance SimpleFormC e t m=>Builder (SimpleFormR e t m) B where
  buildA mFN = liftF formRow . gBuildA mFN

instance Generic MyMap
instance HasDatatypeInfo MyMap
instance SimpleFormC e t m=>Builder (SimpleFormR e t m) MyMap 

instance Generic BRec
instance HasDatatypeInfo BRec
instance SimpleFormC e t m=>Builder (SimpleFormR e t m) BRec where
  buildA mFN = liftF formRow . gBuildA mFN

instance Generic C
instance HasDatatypeInfo C
instance SimpleFormC e t m=>Builder (SimpleFormR e t m) C where
  buildA mFN = liftF formCol . gBuildA mFN

-- (Almost) Equivalent TH-built instances
{-
-- template derivation is simpler but may be a compile time issues for ghcjs.  And some people don't like TH.
deriveSFRowBuilder ''A
deriveSFRowBuilder ''B
deriveSFRowBuilder ''MyMap
deriveSFRowBuilder ''BRec
deriveSFColBuilder ''C
-}

-- put some data in for demo purposes
b1 = B 12 [AI 10, AS "Hello" Square, AC Green, AI 4, AS "Goodbye" Circle]
b2 = B 4 [AI 1, AS "Hola" Triangle, AS "Adios" Circle, AC Red ]
c = C 3.14159 (MyMap (M.fromList [("b1",b1),("b2",b2)])) (BRec (B 42 []) Seq.empty HS.empty)

flowTestWidget::MonadWidget t m=>Int->m (Dynamic t String)
flowTestWidget n = do
  text "Are all these checked?"
  boolDyns <- sequence $ take n $ Prelude.repeat (RDC._hwidget_value <$> RDC.htmlCheckbox (RDC.WidgetConfig never False (constDyn mempty)))
  allTrueDyn <- foldM (\x bDyn -> combineDyn (&&) x bDyn) (constDyn True) boolDyns
  forDyn allTrueDyn $ \b -> if b then "All Checked!" else "Some Unchecked."

test::(SimpleFormC e t m,MonadIO (PushM t))=>e->m ()
test cfg = do
  cDyn<- flexFillR $ makeSimpleForm cfg (CssClass "simpleForm") (Just c)
  mapDyn ppShow cDyn >>= dynText
  _ <- flexFillR $ observeDynamic cfg (CssClass "simpleObserver") cDyn
  _ <- observeFlow cfg (CssClass "simpleForm") (CssClass "simpleObserver") flowTestWidget 2
  return ()

{-
   NB: AllDefault module exports DefSFCfg which is an instance of SimpleFormConfiguration with all the specific failure, sum and styling.
   Different instances of that class could be made to change styling or make it more customizable.
   That's the part to re-implement for different form behavior. Or just to use different layout functions than the ones I've
   implemented with here.
-}

demoCfg = DefSFCfg {
    cfgValidStyle = (CssClasses [CssClass "sf-outline-black"])
  , cfgInvalidStyle = (CssClasses [CssClass "sf-invalid"])
  , cfgDisable = False
  }


main  :: IO ()
main  = mainWidgetWithCss (flexCssBS <> cssToBS simpleFormDefaultCss <> cssToBS simpleObserverDefaultCss) $ test demoCfg




