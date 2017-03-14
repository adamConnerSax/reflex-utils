{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
--{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Reflex.Dom.Contrib.SimpleForm.Instances.BasicDynamic
       (
         buildDynReadMaybe
       , buildDynReadable
       ) where

import           Control.Lens                          (over, view, (^.))
import           Control.Monad.Fix                     (MonadFix)
import           Control.Monad.Reader                  (lift)
import qualified Data.Map                              as M
import           Data.Maybe                            (fromMaybe)
import           Data.Monoid                           ((<>))
import           Data.Readable                         (Readable, fromText)
import qualified Data.Text                             as T
import           Data.Validation                       (AccValidation (..))
import           Text.Read                             (readMaybe)


-- types for instances
import           Data.ByteString                       (ByteString)
import           Data.Int                              (Int16, Int32, Int64,
                                                        Int8)
import           Data.Time.Calendar                    (Day)
import           Data.Time.Clock                       (UTCTime)
import           Data.Word                             (Word16, Word32, Word64,
                                                        Word8)
-- reflex imports
import qualified Reflex                                as R
import qualified Reflex.Dom                            as RD
import           Reflex.Dom.Contrib.Widgets.Common

-- From this lib

import qualified DataBuilder                           as B
import           Reflex.Dom.Contrib.Layout.Types       (emptyCss, toCssString)
import           Reflex.Dom.Contrib.ReflexConstraints
import           Reflex.Dom.Contrib.SimpleForm.Builder
import           Reflex.Dom.Contrib.SimpleForm.Instances.Basic
-- instances


-- turn a Dynamic into an Event with an initial firing to represent the value at postbuild.  Should we sample and return (a,Event t a)?
mDynToInputEv::(R.Reflex t,RD.PostBuild t m)=>Maybe (R.Dynamic t a)-> m (R.Event t a)
mDynToInputEv mDyn = do
  postbuild <- RD.getPostBuild
  let startValueEv x = R.attachWith const x postbuild
      comboEv d = R.leftmost [startValueEv (R.current d), R.updated d]
      updateEv = maybe R.never comboEv mDyn      
  return updateEv -- this might cause loops from the startValueEv??

buildDynReadable::(SimpleFormInstanceC t m, Readable a, Show a)
  =>B.Validator (DynValidation t) a
  ->Maybe FieldName
  ->Maybe (R.Dynamic t a)
  ->SimpleFormR t m a
buildDynReadable va mFN maDyn = makeSimpleFormR $ do
  let vfwt x = parseAndValidate mFN x fromText va
  inputEv <- mDynToInputEv maDyn
  unSF $ sfWidget' inputEv "" showText vfwt showText mFN Nothing $ textWidgetValue mFN

buildDynReadMaybe::(SimpleFormInstanceC t m, Read a, Show a)=>B.Validator (DynValidation t) a->Maybe FieldName->Maybe (R.Dynamic t a)->SimpleFormR t m a
buildDynReadMaybe va mFN maDyn = makeSimpleFormR $ do
  let vfwt x = parseAndValidate mFN x (readMaybe . T.unpack) va
  inputEv <- mDynToInputEv maDyn
  unSF $ sfWidget' inputEv "" showText vfwt showText mFN Nothing $ textWidgetValue mFN

{-
-- | String and Text
instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) (Dynamic t T.Text) where
  buildValidated va mFN mInitialDyn = do
    postbuild <- getPostBuild
    
    sfWidget' R.never (maybe "" id mInitial) id va id mFN Nothing $ textWidgetValue mFN

instance {-# OVERLAPPING #-} SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) String where
  buildValidated va mFN mInitial =
    let va' = (\t -> T.pack <$> va (T.unpack t))
    in T.unpack <$> buildValidated va' mFN (T.pack <$> mInitial)


{- Not clear what to do here! Default behavior is bad since Char is a huge enum.
instance SimpleFormC e t m=>B.Builder (RFormWidget e t m) Char where
  buildValidated va md mInitial = RFormWidget $ do
    e <- ask
    attrsDyn <- makeSFAttrs "Char"
    lift $ item attrs0e $ _hwidget_value <$> readableWidget (WidgetConfig RD.never mInitial attrsDyn)
-}

-- We don't need this.  If we leave it out, the Enum instance will work an we get a dropdown instead of a checkbox.  Which might be better...
instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Bool where
  buildValidated va mFN mInitial = sfWidget' R.never (fromMaybe False mInitial) id va showText mFN Nothing $ (\c -> _hwidget_value <$> htmlCheckbox c)

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Double where
  buildValidated = buildReadable

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Float where
  buildValidated = buildReadable

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Int where
  buildValidated = buildReadable

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Integer where
  buildValidated = buildReadable

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Int8 where
  buildValidated  = buildReadable

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Int16 where
  buildValidated  = buildReadable

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Int32 where
  buildValidated = buildReadable

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Int64 where
  buildValidated = buildReadable

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Word8 where
  buildValidated = buildReadable

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Word16 where
  buildValidated = buildReadable

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Word32 where
  buildValidated = buildReadable

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Word64 where
  buildValidated = buildReadable

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) ByteString where
  buildValidated = buildReadable

--dateTime and date
instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) UTCTime where
  buildValidated va mFN mInitial =
    let vfwt x = case x of
          Nothing -> dynValidationErr [SFNoParse "Couldn't parse as UTCTime."]
          Just y -> va y
    in sfWidget' R.never mInitial Just vfwt showText mFN Nothing $  (\c -> _hwidget_value <$> restrictWidget blurOrEnter dateTimeWidget c)


instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Day where
  buildValidated va mFN mInitial =
    let vfwt x = case x of
          Nothing -> dynValidationErr [SFNoParse "Couldn't parse as Day."]
          Just y -> va y
    in sfWidget' R.never mInitial Just vfwt showText mFN Nothing $  (\c -> _hwidget_value <$> restrictWidget blurOrEnter dateWidget c)

-- uses generics to build instances
instance (SimpleFormC t m, VBuilderC t m a)=>B.Builder (SFR t m) (DynValidation t) (Maybe a)

instance (SimpleFormC t m, VBuilderC t m a, VBuilderC t m b)=>B.Builder (SFR t m) (DynValidation t) (Either a b)


-- | Enums become dropdowns
instance {-# OVERLAPPABLE #-} (SimpleFormInstanceC t m,Enum a,Show a,Bounded a, Eq a)
                              =>B.Builder (SFR t m) (DynValidation t) a where
  buildValidated va mFN mInitial =
    let values = [minBound..] :: [a]
        initial = fromMaybe (head values) mInitial
    in sfWidget' RD.never initial id va showText mFN Nothing $ (\c -> _widget0_value <$> htmlDropdownStatic values showText Prelude.id c)


-- |  Tuples. 2,3,4,5 tuples are here.  TODO: add more? Maybe write a TH function to do them to save space here?  Since I'm calling mkDyn anyway
-- generics for (,) since mkDyn is not an optimization here
instance (SimpleFormC t m, VBuilderC t m a, VBuilderC t m b)=>B.Builder (SFR t m) (DynValidation t) (a,b)

instance (SimpleFormC t m, VBuilderC t m a, VBuilderC t m b, VBuilderC t m c)=>B.Builder (SFR t m) (DynValidation t) (a,b,c) where
  buildValidated va mFN mTup = B.validateFV va . makeSimpleFormR $ do
    let (ma,mb,mc) = maybe (Nothing,Nothing,Nothing) (\(a,b,c)->(Just a, Just b, Just c)) mTup
    sfRow $ do
      maW <- unSF $ B.buildA Nothing ma
      mbW <- unSF $ B.buildA Nothing mb
      mcW <- unSF $ B.buildA Nothing mc
      return $ (,,) <$> maW <*> mbW <*> mcW

instance (SimpleFormC t m, VBuilderC t m a, VBuilderC t m b, VBuilderC t m c, VBuilderC t m d)
         =>B.Builder (SFR t m) (DynValidation t) (a,b,c,d) where
  buildValidated va mFN mTup = B.validateFV va . makeSimpleFormR $ do
    let (ma,mb,mc,md) = maybe (Nothing,Nothing,Nothing,Nothing) (\(a,b,c,d)->(Just a, Just b, Just c,Just d)) mTup
    sfRow $ do
      maW <- unSF $ B.buildA Nothing ma
      mbW <- unSF $ B.buildA Nothing mb
      mcW <- unSF $ B.buildA Nothing mc
      mdW <- unSF $ B.buildA Nothing md
      return $ (,,,) <$> maW <*> mbW <*> mcW <*> mdW

instance (SimpleFormC t m, VBuilderC t m a, VBuilderC t m b, VBuilderC t m c, VBuilderC t m d, VBuilderC t m e)
         =>B.Builder (SFR t m) (DynValidation t) (a,b,c,d,e) where
  buildValidated va mFN mTup = B.validateFV va . makeSimpleFormR $ do
    let (ma,mb,mc,md,me) = maybe (Nothing,Nothing,Nothing,Nothing,Nothing) (\(a,b,c,d,e)->(Just a, Just b, Just c, Just d, Just e)) mTup
    sfRow $ do
      maW <- unSF $ B.buildA Nothing ma
      mbW <- unSF $ B.buildA Nothing mb
      mcW <- unSF $ B.buildA Nothing mc
      mdW <- unSF $ B.buildA Nothing md
      meW <- unSF $ B.buildA Nothing me
      return $ (,,,,) <$> maW <*> mbW <*> mcW <*> mdW <*> meW
-}
