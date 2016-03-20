{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Dom.Contrib.SimpleForm.Instances.Basic () where

import Control.Applicative (liftA2)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.Morph (hoist)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Maybe (isJust)
import Data.Readable (Readable)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- types for instances
import GHC.Tuple
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Word (Word8,Word16,Word32,Word64)
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import Data.Time.Calendar (Day)

-- for using the generic builder
import qualified GHC.Generics as GHCG

-- reflex imports
import qualified Reflex as R 
import qualified Reflex.Dom as RD
import Reflex.Dynamic.TH (mkDyn)
import Reflex.Dom.Contrib.Widgets.Common --(HtmlWidget,combineWidgets)

-- From this lib
import Reflex.Dom.Contrib.Layout.Types (CssClasses,IsCssClass(..))


import qualified DataBuilder as B

import Reflex.Dom.Contrib.SimpleForm.Builder

-- instances

--some helpers
buildReadable::(SimpleFormC e t m,Readable a, Show a)=>Maybe FieldName->Maybe a->SimpleFormR e t m a
buildReadable mFN ma = SimpleFormR $ mdo
  attrsDyn <- sfAttrs dma mFN Nothing
  dma <- itemL . lift $ _hwidget_value <$> restrictWidget blurOrEnter readableWidget (WidgetConfig RD.never ma attrsDyn)
  return dma

buildReadMaybe::(SimpleFormC e t m,Read a, Show a)=>Maybe FieldName->Maybe a->SimpleFormR e t m a
buildReadMaybe mFN ma = SimpleFormR $ mdo
  attrsDyn <- sfAttrs dma mFN Nothing
  let initial = maybe "" show ma
  dma <- itemL . lift $ do
    tW <- _hwidget_value <$> restrictWidget blurOrEnter (htmlTextInput (maybe "" id mFN)) (WidgetConfig RD.never initial attrsDyn)
    R.mapDyn readMaybe tW
  return dma

-- | String and Text
instance SimpleFormC e t m =>B.Builder (SimpleFormR e t m) T.Text where
  buildA mFN mInitial = SimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "Text")
    let initial = maybe (T.pack "") id mInitial
    dma <- itemL . lift $ do
      tW <- _hwidget_value <$> restrictWidget blurOrEnter (htmlTextInput "Text") (WidgetConfig RD.never (T.unpack initial) attrsDyn)
      R.mapDyn (Just . T.pack) tW
    return dma

instance {-# OVERLAPPING #-} SimpleFormC e t m=>B.Builder (SimpleFormR e t m) String where
  buildA mFN mInitial = SimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "String")
    let initial = maybe "" id mInitial
    dma <- itemL . lift $ do
      tW <- _hwidget_value <$> restrictWidget blurOrEnter (htmlTextInput "String") (WidgetConfig RD.never initial attrsDyn)
      R.mapDyn Just tW
    return dma

{- Not clear what to do here! Default behavior is bad since Char is a huge enum.
instance SimpleFormC e t m=>B.Builder (RFormWidget e t m) Char where
  buildA md mInitial = RFormWidget $ do
    e <- ask
    attrsDyn <- makeSFAttrs "Char"
    lift $ itemL attrs0e $ _hwidget_value <$> readableWidget (WidgetConfig RD.never mInitial attrsDyn)
-}

-- We don't need this.  If we leave it out, the Enum instance will work an we get a dropdown instead of a checkbox.  Which might be better...
instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Bool where
  buildA mFN mInitial = SimpleFormR $ mdo
    let initial = maybe False id mInitial
    attrsDyn <- sfAttrs (R.constDyn $ Just False) mFN (Just $ "Bool")
    dma <- itemL . lift $ _hwidget_value <$> htmlCheckbox (WidgetConfig RD.never initial attrs0)
    lift $ R.mapDyn Just dma

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Double where
  buildA mFN mInitial = SimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "Double")
    dma <- itemL . lift $ _hwidget_value <$> restrictWidget blurOrEnter doubleWidget (WidgetConfig RD.never mInitial attrsDyn)
    return dma

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Float where
  buildA = buildReadable

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Int where
  buildA mFN mInitial = SimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "Int")
    dma <- itemL . lift $ _hwidget_value <$> restrictWidget blurOrEnter intWidget (WidgetConfig RD.never mInitial attrsDyn)
    return dma

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Integer where
  buildA mFN mInitial = SimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "Int")
    dma <- itemL . lift $ _hwidget_value <$> restrictWidget blurOrEnter integerWidget (WidgetConfig RD.never mInitial attrsDyn)
    return dma

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Int8 where
  buildA = buildReadable

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Int16 where
  buildA = buildReadable

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Int32 where
  buildA = buildReadable

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Int64 where
  buildA = buildReadable

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Word8 where
  buildA = buildReadable

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Word16 where
  buildA = buildReadable

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Word32 where
  buildA = buildReadable

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Word64 where
  buildA = buildReadable

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) ByteString where
  buildA = buildReadable

--dateTime and date
instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) UTCTime where
  buildA mFN mInitial = SimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "UTCTime")
    dma<-itemL . lift $ _hwidget_value <$> restrictWidget blurOrEnter dateTimeWidget (WidgetConfig RD.never mInitial attrsDyn)
    return dma

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Day where
  buildA mFN mInitial = SimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "Day")
    dma <- itemL . lift $ _hwidget_value <$> restrictWidget blurOrEnter dateWidget (WidgetConfig RD.never mInitial attrsDyn)
    return dma


-- I should be able to make TH do this, right? Or not.  Since it would have to reify a.  So it would need a template splice for every a.
instance (SimpleFormC e t m,B.Builder (SimpleFormR e t m) a)=>B.Builder (SimpleFormR e t m) (Maybe a)

instance (SimpleFormC e t m,B.Builder (SimpleFormR e t m) a,B.Builder (SimpleFormR e t m) b)=>B.Builder (SimpleFormR e t m) (Either a b)
    

-- | Enums become dropdowns
instance {-# OVERLAPPABLE #-} (SimpleFormC e t m,Enum a,Show a,Bounded a, Eq a)
                              =>B.Builder (SimpleFormR e t m) a where
  buildA mFN mInitial = SimpleFormR $ mdo
    dropdownClasses <- dropdownStyle
    attrsDyn <- sfAttrs' dma mFN Nothing dropdownClasses 
    let values = [minBound..] :: [a]
        initial = maybe (head values) id mInitial
    dma <- itemL . lift $ do
      eW <- _widget0_value <$> htmlDropdownStatic values show Prelude.id (WidgetConfig RD.never initial attrsDyn)
      R.mapDyn Just eW
    return dma

-- |  Tuples. 2,3,4,5 tuples are here.  TODO: add more? Maybe write a TH function to do them to save space here?  Since I'm calling mkDyn anyway
instance (SimpleFormC e t m,
          B.Builder  (SimpleFormR e t m)  a,
          B.Builder  (SimpleFormR e t m)  b)
         =>B.Builder (SimpleFormR e t m) (a,b) where
  buildA mFN mTup = SimpleFormR $ do
    let (ma,mb) = maybe (Nothing,Nothing) (\(a,b)->(Just a, Just b)) mTup
    formRow  $ do
      maW <- unSF $ B.buildA mFN ma
      mbW <- unSF $ B.buildA mFN mb
      lift $ R.combineDyn (liftA2 (,)) maW mbW


instance (SimpleFormC e t m,
          B.Builder  (SimpleFormR e t m)  a,
          B.Builder  (SimpleFormR e t m)  b,
          B.Builder  (SimpleFormR e t m)  c)
         =>B.Builder (SimpleFormR e t m) (a,b,c) where
  buildA mFN mTup = SimpleFormR $ do
    let (ma,mb,mc) = maybe (Nothing,Nothing,Nothing) (\(a,b,c)->(Just a, Just b, Just c)) mTup
    formRow $ do
      maW <- unSF $ B.buildA mFN ma
      mbW <- unSF $ B.buildA mFN mb
      mcW <- unSF $ B.buildA mFN mc
      lift $ [mkDyn|(,,) <$> $maW <*> $mbW <*> $mcW|]

instance (SimpleFormC e t m,
          B.Builder  (SimpleFormR e t m)  a,
          B.Builder  (SimpleFormR e t m)  b,
          B.Builder  (SimpleFormR e t m)  c,
          B.Builder  (SimpleFormR e t m)  d)
         =>B.Builder (SimpleFormR e t m) (a,b,c,d) where
  buildA mFN mTup = SimpleFormR $ do
    let (ma,mb,mc,md) = maybe (Nothing,Nothing,Nothing,Nothing) (\(a,b,c,d)->(Just a, Just b, Just c,Just d)) mTup
    formRow $ do
      maW <- unSF $ B.buildA mFN ma
      mbW <- unSF $ B.buildA mFN mb
      mcW <- unSF $ B.buildA mFN mc
      mdW <- unSF $ B.buildA mFN md
      lift $ [mkDyn|(,,,) <$> $maW <*> $mbW <*> $mcW <*> $mdW|]

instance (SimpleFormC e t m,
          B.Builder  (SimpleFormR e t m)  a,
          B.Builder  (SimpleFormR e t m)  b,
          B.Builder  (SimpleFormR e t m)  c,
          B.Builder  (SimpleFormR e t m)  d,
          B.Builder  (SimpleFormR e t m)  e)
         =>B.Builder (SimpleFormR e t m) (a,b,c,d,e) where
  buildA mFN mTup = SimpleFormR $ do
    let (ma,mb,mc,md,me) = maybe (Nothing,Nothing,Nothing,Nothing,Nothing) (\(a,b,c,d,e)->(Just a, Just b, Just c, Just d, Just e)) mTup
    formRow $ do
      maW <- unSF $ B.buildA mFN ma
      mbW <- unSF $ B.buildA mFN mb
      mcW <- unSF $ B.buildA mFN mc
      mdW <- unSF $ B.buildA mFN md
      meW <- unSF $ B.buildA mFN me
      lift $ [mkDyn|(,,,,) <$> $maW <*> $mbW <*> $mcW <*> $mdW <*> $meW|]
