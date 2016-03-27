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
module Reflex.Dom.Contrib.SimpleForm.Instances.Basic
       (
         sfWidget
       ) where

import Control.Applicative (liftA2)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.Morph (hoist)
import Control.Monad.Fix (MonadFix)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Maybe (isJust,fromJust)
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
readOnlyW::RD.MonadWidget t m=>(a->String)->WidgetConfig t a->m (R.Dynamic t a) 
readOnlyW f wc = do
  da <- R.foldDyn const (_widgetConfig_initialValue wc) (_widgetConfig_setValue wc)
  ds <- R.mapDyn f da
  RD.elDynAttr "div" (_widgetConfig_attributes wc) $ RD.dynText ds
  return da

sfWidget::SimpleFormC e t m=>(a->b)->(a->String)->WidgetConfig t a->(WidgetConfig t a->m (R.Dynamic t a))->ReaderT e m (R.Dynamic t b)
sfWidget fDyn fString wc widget = do
  disabled <- inputsDisabled
  lift $ (if disabled then readOnlyW fString wc else widget wc) >>= R.mapDyn fDyn


buildReadable::(SimpleFormC e t m,Readable a, Show a)=>Maybe FieldName->Maybe a->SimpleFormR e t m a
buildReadable mFN ma = SimpleFormR $ mdo
  attrsDyn <- sfAttrs dma mFN Nothing
  let wc = WidgetConfig RD.never ma attrsDyn
  dma <- itemL $ sfWidget id (show . fromJust) wc (\c->_hwidget_value <$> restrictWidget blurOrEnter readableWidget c)
  return dma

buildReadMaybe::(SimpleFormC e t m,Read a, Show a)=>Maybe FieldName->Maybe a->SimpleFormR e t m a
buildReadMaybe mFN ma = SimpleFormR $ mdo
  attrsDyn <- sfAttrs dma mFN Nothing
  let initial = maybe "" show ma
      wc = WidgetConfig RD.never initial attrsDyn
  dma <- itemL $ sfWidget readMaybe show wc $ \c -> do
    _hwidget_value <$> restrictWidget blurOrEnter (htmlTextInput (maybe "" id mFN)) c 
  return dma

-- | String and Text
instance SimpleFormC e t m =>B.Builder (SimpleFormR e t m) T.Text where
  buildA mFN mInitial = SimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "Text")
    let initial = maybe (T.pack "") id mInitial
        wc = WidgetConfig RD.never (T.unpack initial) attrsDyn
    dma <- itemL $ sfWidget (Just . T.pack) show wc $ \c -> do
      _hwidget_value <$> restrictWidget blurOrEnter (htmlTextInput "Text") c
    return dma

instance {-# OVERLAPPING #-} SimpleFormC e t m=>B.Builder (SimpleFormR e t m) String where
  buildA mFN mInitial = SimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "String")
    let initial = maybe "" id mInitial
        wc = WidgetConfig RD.never initial attrsDyn
    dma <- itemL $ sfWidget Just id wc $ \c-> do
      _hwidget_value <$> restrictWidget blurOrEnter (htmlTextInput "text") c
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
        wc = WidgetConfig RD.never initial attrsDyn
    attrsDyn <- sfAttrs (R.constDyn $ Just False) mFN (Just $ "Bool")
    itemL $ sfWidget Just show wc $ \c -> _hwidget_value <$> htmlCheckbox c

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Double where
  buildA mFN mInitial = SimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "Double")
    let wc = WidgetConfig RD.never mInitial attrsDyn
    dma <- itemL $ sfWidget id (show . fromJust) wc $ \c -> _hwidget_value <$> restrictWidget blurOrEnter doubleWidget c
    return dma

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Float where
  buildA = buildReadable

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Int where
  buildA mFN mInitial = SimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "Int")
    let wc = WidgetConfig RD.never mInitial attrsDyn
    dma <- itemL $ sfWidget id  (show . fromJust) wc $ \c->_hwidget_value <$> restrictWidget blurOrEnter intWidget c
    return dma


instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Integer where
  buildA mFN mInitial = SimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "Int")
    let wc = WidgetConfig RD.never mInitial attrsDyn
    dma <- itemL $ sfWidget id (show . fromJust) wc $ \c -> _hwidget_value <$> restrictWidget blurOrEnter integerWidget c
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
    let wc = WidgetConfig RD.never mInitial attrsDyn
    dma<-itemL $ sfWidget id (show . fromJust) wc $ \c -> _hwidget_value <$> restrictWidget blurOrEnter dateTimeWidget c
    return dma

instance SimpleFormC e t m=>B.Builder (SimpleFormR e t m) Day where
  buildA mFN mInitial = SimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "Day")
    let wc = WidgetConfig RD.never mInitial attrsDyn
    dma <- itemL $ sfWidget id (show . fromJust) wc $ \c -> _hwidget_value <$> restrictWidget blurOrEnter dateWidget c
    return dma

-- uses generics to build instances
instance (SimpleFormC e t m,B.Builder (SimpleFormR e t m) a)=>B.Builder (SimpleFormR e t m) (Maybe a)

instance (SimpleFormC e t m,B.Builder (SimpleFormR e t m) a,B.Builder (SimpleFormR e t m) b)=>B.Builder (SimpleFormR e t m) (Either a b)
    

-- | Enums become dropdowns
instance {-# OVERLAPPABLE #-} (SimpleFormC e t m,Enum a,Show a,Bounded a, Eq a)
                              =>B.Builder (SimpleFormR e t m) a where
  buildA mFN mInitial = SimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN Nothing
    let values = [minBound..] :: [a]
        initial = maybe (head values) id mInitial
        wc = WidgetConfig RD.never initial attrsDyn
    dma <- itemL $ sfWidget Just show wc $ \c -> _widget0_value <$> htmlDropdownStatic values show Prelude.id c
    return dma

-- |  Tuples. 2,3,4,5 tuples are here.  TODO: add more? Maybe write a TH function to do them to save space here?  Since I'm calling mkDyn anyway
-- generics for (,) since mkDyn is not an optimization here
instance (SimpleFormC e t m,
          B.Builder  (SimpleFormR e t m)  a,
          B.Builder  (SimpleFormR e t m)  b)
         =>B.Builder (SimpleFormR e t m) (a,b) 

instance (SimpleFormC e t m,
          B.Builder  (SimpleFormR e t m)  a,
          B.Builder  (SimpleFormR e t m)  b,
          B.Builder  (SimpleFormR e t m)  c)
         =>B.Builder (SimpleFormR e t m) (a,b,c) where
  buildA mFN mTup = SimpleFormR $ do
    let (ma,mb,mc) = maybe (Nothing,Nothing,Nothing) (\(a,b,c)->(Just a, Just b, Just c)) mTup
    formRow $ do
      maW <- unSF $ B.buildA Nothing ma
      mbW <- unSF $ B.buildA Nothing mb
      mcW <- unSF $ B.buildA Nothing mc
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
      maW <- unSF $ B.buildA Nothing ma
      mbW <- unSF $ B.buildA Nothing mb
      mcW <- unSF $ B.buildA Nothing mc
      mdW <- unSF $ B.buildA Nothing md
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
      maW <- unSF $ B.buildA Nothing ma
      mbW <- unSF $ B.buildA Nothing mb
      mcW <- unSF $ B.buildA Nothing mc
      mdW <- unSF $ B.buildA Nothing md
      meW <- unSF $ B.buildA Nothing me
      lift $ [mkDyn|(,,,,) <$> $maW <*> $mbW <*> $mcW <*> $mdW <*> $meW|]
