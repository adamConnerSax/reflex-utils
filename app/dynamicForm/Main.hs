{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
module Main where

import Reflex
import Reflex.Dom
import qualified Reflex.Dom.Contrib.Widgets.Common as RDC
import Reflex.Dom.Contrib.Layout.All
import Reflex.Dom.Contrib.Layout.GridConfigs
import Reflex.Dom.Contrib.DynamicForm
import GHC.Generics (Generic)


import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Data.Monoid ((<>))
import Data.FileEmbed

import Prelude hiding (rem,div,span)
import Clay hiding (button,col,Color)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
--import qualified Data.List as T

boxMargin m = sym margin (rem m)
cssBox m c = do
  boxMargin m
  border solid (px 2) c

cssBoxes = do
  ".demo-box" ? boxMargin 0.1
  ".demo-box-none" ? boxMargin 0.1 
  ".demo-box-black" ? cssBox 0.1 black
  ".demo-box-red" ? cssBox 0.1 red


clayCssBS::B.ByteString
clayCssBS = B.concat . BL.toChunks . encodeUtf8  $ renderWith pretty [] $ cssBoxes

boxClassCss = ("class" =: "demo-box")

demoDiv x = elAttr "div" boxClassCss $ text x


type DynamicFormC t m = (MonadWidget t m, MonadIO (PushM t))

data Color = Green | Yellow | Red deriving (Show,Enum,Bounded,Eq,Ord)

data A = AI Int | AS String | AC Color deriving (Show,Generic)
data B = B { int::Int, as::[A] } deriving (Show,Generic)
data C = C Double (M.Map String B) deriving (Show,Generic)

-- generic instances
--instance DynamicFormC t m=>DynamicForm e t m Color
instance DynamicFormC t m=>DynamicForm e t m A
instance DynamicFormC t m=>DynamicForm e t m B
instance DynamicFormC t m=>DynamicForm e t m C

b1 = B 12 [AI 10, AS "Hello", AC Red, AI 4, AS "Goodbye"]
b2 = B 4 [AI 1, AS "Hola", AS "Adios", AC Green ]
c = C 3.14159 $ M.fromList [("b1",b1),("b2",b2)]


data DFLayout (t :: *) (m :: * -> *) = DFLayout

instance DynamicFormC t m=>HasDFLayout (DFLayout t m) t m where
  --type MW t m = MonadWidget t m=>LayoutM t m
  containerLayout = const flexLayoutCol
  containedItemLayout = const $ flexFillR . flexLayoutRow
  multiItemLayout = const  flexLayoutRow
  itemLayout = const $ flexCol 1
  itemStyle = const $ CssClasses [CssClass "demo-box-red"]
  showTypeNames = const False
  showConstructorNames = const False
  showFieldNames = const False


test::DynamicFormC t m=>LayoutM t m ()
test = do
  let dfL = DFLayout
  cDyn<-flexFillR $ makeDynamicForm dfL c
  mapDyn show cDyn >>= dynText 


allCss = flexCssBS <> clayCssBS

main  :: IO ()
main  = mainWidgetWithCss allCss $ runLayoutMain (LayoutConfig pure24GridConfig emptyClassMap emptyDynamicCssMap) $ test




