{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveGeneric         #-}

module Reflex.Dom.Contrib.SimpleForm.Instances.Basic
       (
         sfWidget
       , sfWidget'
       , dynAsEv
       , traceDynAsEv
       , buildDynReadMaybe
       , buildDynReadable
       , BasicC
       , SimpleFormInstanceC
       , showText
       , parseError
       , parseAndValidate
       , textWidgetValue
       , item
       ) where

import           Control.Lens                          (over, view, (^.))
import           Control.Monad                         (join)
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
import           Data.Time.Calendar                    (Day, fromGregorian)
import           Data.Time.Clock                       (UTCTime (..),
                                                        secondsToDiffTime)
import           Data.Tuple.Select                     (sel1, sel2, sel3, sel4,
                                                        sel5)
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
-- instances

--some helpers
showText::Show a=>a->T.Text
showText = T.pack . show

type BasicC t m = (RD.DomBuilder t m, R.MonadHold t m, MonadFix m)
type SimpleFormInstanceC t m = (SimpleFormC t m, MonadWidgetExtraC t m, RD.PostBuild t m, MonadFix m)
--type VBuilderC t m a = (B.Builder (SFR t m) (DynValidation t) a, B.Validatable (DynValidation t) a)

instance {-# OVERLAPPABLE #-} B.Validatable (SFValidation) a where
  validator a = AccSuccess a

readOnlyW::(BasicC t m, RD.PostBuild t m)=>(a->T.Text)->WidgetConfig t a->m (R.Dynamic t a)
readOnlyW f wc = do
  da <- R.holdDyn (_widgetConfig_initialValue wc) (_widgetConfig_setValue wc)
  let ds = f <$> da
  RD.elDynAttr "div" (_widgetConfig_attributes wc) $ RD.dynText ds
  return da

sfWidget::forall t m a b.(R.Reflex t,SimpleFormC t m, RD.PostBuild t m,MonadFix m)
  =>(a->b) -- map in/out type to widget type (int to text, e.g.,)
  ->(a->T.Text) -- show in/out type
  ->Maybe FieldName -- field name ?
  ->WidgetConfig t a
  ->(WidgetConfig t a->m (R.Dynamic t a)) -- underlying reflex-dom-contrib widget
  ->SFR t m (R.Dynamic t b)
sfWidget f fString mFN wc widget = do
  let addToAttrs::R.Reflex t=>T.Text->Maybe T.Text->RD.Dynamic t (M.Map T.Text T.Text)->RD.Dynamic t (M.Map T.Text T.Text)
      addToAttrs attr mVal attrsDyn = case mVal of
        Nothing -> attrsDyn
        Just val -> M.union (attr RD.=: val) <$> attrsDyn
  isObserver <- (==ObserveOnly) <$> getFormType
  inputCfg <- view inputConfig
  inputCss <- inputClasses
  let mTitleVal = maybe (T.pack <$> mFN) Just (_inputTitle inputCfg) -- if none specified and there's a fieldname, use it.
      addTitle = addToAttrs "title" mTitleVal
      addPlaceHolder = addToAttrs "placeholder" (_inputPlaceholder inputCfg)
      mInputClasses = if inputCss == emptyCss then Nothing else Just (toCssString inputCss)
      addInputClass = addToAttrs "class" mInputClasses
      wcAll = over widgetConfig_attributes addTitle wc
      wcInput = over widgetConfig_attributes (addPlaceHolder . addInputClass) wcAll
      labeledWidget iw = case _inputLabelConfig inputCfg of
        Nothing -> iw
        Just (LabelConfig t attrs) -> RD.elAttr "label" attrs  $ (RD.el "span" $ RD.text t) >> iw
  lift . labeledWidget $ (fmap f <$> (if isObserver then readOnlyW fString wcAll else widget wcInput))

sfWidget'::(R.Reflex t,SimpleFormC t m, RD.PostBuild t m,MonadFix m)
  =>R.Event t a -- update value events
  ->b -- initial value to display in b-widget
  ->(a->b) -- map in/out type to widget type
  ->(b->SFValidation a) --validate and map back
  ->(b->T.Text) -- showText for widget type
  ->Maybe FieldName -- field name ?
  ->Maybe T.Text -- type name ?
  ->(WidgetConfig t b->m (R.Dynamic t b)) -- underlying reflex-dom-contrib widget
  ->SFR t m (DynValidation t a)
sfWidget' updateEv initialWV toWT validateWT showWT mFN mTypeName widget = mdo
  attrsDyn <- sfAttrs dva mFN mTypeName
  let wc = WidgetConfig (toWT <$> updateEv) initialWV attrsDyn
  dva <- item $ DynValidation <$> sfWidget validateWT showWT mFN wc widget
  return dva


item::Monad m=>SFLayoutF t m
item = sfItem

instance R.Reflex t=>Functor (HtmlWidget t) where
  fmap f (HtmlWidget v c kp kd ku hf) = HtmlWidget (f <$> v) (f <$> c) kp kd ku hf

textWidgetValue::SimpleFormInstanceC t m=>Maybe FieldName->WidgetConfig t T.Text -> m (R.Dynamic t T.Text)
textWidgetValue mFN c = _hwidget_value <$> restrictWidget' blurOrEnter (htmlTextInput (maybe "" T.pack mFN)) c

textWidgetValue'::SimpleFormInstanceC t m=>Maybe FieldName->WidgetConfig t T.Text -> m (R.Dynamic t T.Text)
textWidgetValue' mFN c = _hwidget_value <$> htmlTextInput (maybe "" T.pack mFN) c

-- this does what restrictWidget does but allows the set event to change the "authoritative value"
restrictWidget'::(RD.DomBuilder t m, R.MonadHold t m)
  =>(HtmlWidget t a -> R.Event t a)
  -> GWidget t m a
  -> GWidget t m a
restrictWidget' restrictFunc wFunc cfg = do
  w <- wFunc cfg
  let e = R.traceEventWith (const "restricWidget e") $ R.leftmost [R.traceEventWith (const "restricWidget setVal") (_widgetConfig_setValue cfg), restrictFunc w]
  v <- R.holdDyn (_widgetConfig_initialValue cfg) e
  return $ w { _hwidget_value = v 
             , _hwidget_change = e
             }

parseError::Maybe FieldName->T.Text->T.Text
parseError mFN x = T.pack (fromMaybe "N/A" mFN) <> ": " <> x

parseAndValidate::Maybe FieldName->(T.Text -> Maybe a)->FormValidator a->T.Text->AccValidation SimpleFormErrors a
parseAndValidate mFN parse va t =
  case parse t of
    Nothing -> AccFailure [SFNoParse $ parseError mFN t]
    Just y -> va y

-- NB: It's crucial that the updated event be first.  If the dyn is updated by the caller's use of postbuild then
-- that's the value we want not the tagged current value. 
dynAsEv::RD.PostBuild t m=>R.Dynamic t a->m (R.Event t a)
dynAsEv dyn = (\x -> R.leftmost [R.updated dyn, R.tagPromptlyDyn dyn x]) <$> RD.getPostBuild

traceDynAsEv::RD.PostBuild t m=>(a->String)->R.Dynamic t a->m (R.Event t a)
traceDynAsEv f dyn = do
  postbuild <- RD.getPostBuild
  let f' prefix x = prefix ++ f x
      upEv = R.traceEventWith (f' "update-") $ R.updated dyn
      pbEv = R.traceEventWith (f' "postbuild-") $ R.tagPromptlyDyn dyn postbuild
  return $ R.leftmost [upEv, pbEv] 


-- turn a Dynamic into an Event with an initial firing to represent the value at postbuild.  Should we sample and return (a,Event t a)?
mDynToInputEv::(R.Reflex t,RD.PostBuild t m)=>Maybe (R.Dynamic t a)-> m (R.Event t a)
mDynToInputEv mDyn = maybe (return R.never) dynAsEv mDyn

buildDynReadable::(SimpleFormInstanceC t m, Readable a, Show a)
  =>FormValidator a
  ->Maybe FieldName
  ->Maybe (R.Dynamic t a)
  ->SimpleFormR t m a
buildDynReadable va mFN maDyn = makeSimpleFormR $ do
  let vfwt = parseAndValidate mFN fromText va
  inputEv <- mDynToInputEv maDyn
  sfWidget' inputEv "" showText vfwt showText mFN Nothing $ textWidgetValue mFN

buildDynReadMaybe::(SimpleFormInstanceC t m, Read a, Show a)
  =>FormValidator a
  ->Maybe FieldName
  ->Maybe (R.Dynamic t a)
  ->SimpleFormR t m a
buildDynReadMaybe va mFN maDyn = makeSimpleFormR $ do
  let vfwt = parseAndValidate mFN (readMaybe . T.unpack) va
  inputEv <- mDynToInputEv maDyn
  sfWidget' inputEv "" showText vfwt showText mFN Nothing $ textWidgetValue mFN

-- NB this will handle Dynamic t (Dynamic t a)) inputs
instance (SimpleFormInstanceC t m, VFormBuilderC t m a)=>FormBuilder t m (R.Dynamic t a) where
  buildForm va mFN = validateForm va . fmap R.constDyn . buildForm' mFN . fmap join

-- | String and Text
{-
instance SimpleFormInstanceC t m=>FormBuilder t m T.Text where
  buildForm va mFN mInitialDyn = makeSimpleFormR $ do
    inputEv <- maybe (return R.never) (traceDynAsEv (const "FormBuilder t m T.Text")) mInitialDyn  -- mDynToInputEv mInitialDyn
    sfWidget' inputEv "" id va id mFN Nothing $ textWidgetValue mFN
-}

{-
instance SimpleFormInstanceC t m=>FormBuilder t m T.Text where
  buildForm va mFN mInitialDyn = makeSimpleFormR $ do
    inputEv <- maybe (return R.never) (traceDynAsEv (const "FormBuilder t m T.Text")) mInitialDyn  -- mDynToInputEv mInitialDyn
    let wc = WidgetConfig inputEv "" (R.constDyn M.empty)
    valDyn <- sfWidget id id mFN wc $ (\c -> _hwidget_value <$> htmlTextInput (maybe "" T.pack mFN) c) -- textWidgetValue mFN
    return . DynValidation $ AccSuccess <$> valDyn 
-}


instance SimpleFormInstanceC t m=>FormBuilder t m T.Text where
  buildForm va mFN mInitialDyn = makeSimpleFormR $ do
    inputEv <- maybe (return R.never) (traceDynAsEv (const "FormBuilder t m T.Text")) mInitialDyn  -- mDynToInputEv mInitialDyn
--    let config = RD.def {RD._textInputConfig_setValue = inputEv }
    let wc = WidgetConfig inputEv "" (R.constDyn M.empty)
    valDyn <- textWidgetValue mFN wc --RD._textInput_value <$> RD.textInput config
    return . DynValidation $ AccSuccess <$> valDyn 


instance {-# OVERLAPPING #-} SimpleFormInstanceC t m=>FormBuilder t m String where
  buildForm va mFN mInitialDyn =
    let va' = (\t -> T.pack <$> va (T.unpack t))
    in T.unpack <$> buildForm va' mFN (fmap T.pack <$> mInitialDyn)


{- Not clear what to do here! Default behavior is bad since Char is a huge enum.
instance SimpleFormC e t m=>B.Builder (RFormWidget e t m) Char where
  buildValidated va md mInitial = RFormWidget $ do
    e <- ask
    attrsDyn <- makeSFAttrs "Char"
    lift $ item attrs0e $ _hwidget_value <$> readableWidget (WidgetConfig RD.never mInitial attrsDyn)
-}


-- We don't need this.  If we leave it out, the Enum instance will work an we get a dropdown instead of a checkbox.  Which might be better...
instance SimpleFormInstanceC t m=>FormBuilder t m Bool where
  buildForm va mFN mInitialDyn = makeSimpleFormR $ do
    inputEv <- mDynToInputEv mInitialDyn
    sfWidget' inputEv False id va showText mFN Nothing $ (\c -> _hwidget_value <$> htmlCheckbox c)

instance SimpleFormInstanceC t m=>FormBuilder t m Double where
  buildForm = buildDynReadable

instance SimpleFormInstanceC t m=>FormBuilder t m Float where
  buildForm = buildDynReadable

instance SimpleFormInstanceC t m=>FormBuilder t m Int where
  buildForm = buildDynReadable

instance SimpleFormInstanceC t m=>FormBuilder t m Integer where
  buildForm = buildDynReadable

instance SimpleFormInstanceC t m=>FormBuilder t m Int8 where
  buildForm  = buildDynReadable

instance SimpleFormInstanceC t m=>FormBuilder t m Int16 where
  buildForm  = buildDynReadable

instance SimpleFormInstanceC t m=>FormBuilder t m Int32 where
  buildForm = buildDynReadable

instance SimpleFormInstanceC t m=>FormBuilder t m Int64 where
  buildForm = buildDynReadable

instance SimpleFormInstanceC t m=>FormBuilder t m Word8 where
  buildForm = buildDynReadable

instance SimpleFormInstanceC t m=>FormBuilder t m Word16 where
  buildForm = buildDynReadable

instance SimpleFormInstanceC t m=>FormBuilder t m Word32 where
  buildForm = buildDynReadable

instance SimpleFormInstanceC t m=>FormBuilder t m Word64 where
  buildForm = buildDynReadable

instance SimpleFormInstanceC t m=>FormBuilder t m ByteString where
  buildForm = buildDynReadable

--dateTime and date
instance SimpleFormInstanceC t m=>FormBuilder t m UTCTime where
  buildForm va mFN mInitialDyn = makeSimpleFormR $ do
    let vfwt x = case x of
          Nothing -> AccFailure [SFNoParse "Couldn't parse as UTCTime."]
          Just y -> va y
        initialDateTime = Just $ UTCTime (fromGregorian 1971 1 1) (secondsToDiffTime 0)
    inputEv <- mDynToInputEv mInitialDyn
    sfWidget' inputEv initialDateTime Just vfwt (maybe "" showText) mFN Nothing $  (\c -> _hwidget_value <$> restrictWidget blurOrEnter dateTimeWidget c)


instance SimpleFormInstanceC t m=>FormBuilder t m Day where
  buildForm va mFN mInitialDyn = makeSimpleFormR $ do
    let vfwt x = case x of
          Nothing -> AccFailure [SFNoParse "Couldn't parse as Day."]
          Just y -> va y
        initialDay = Just $ fromGregorian 1971 1 1
    inputEv <- mDynToInputEv mInitialDyn
    sfWidget' inputEv initialDay Just vfwt (maybe "" showText) mFN Nothing $  (\c -> _hwidget_value <$> restrictWidget blurOrEnter dateWidget c)


-- uses generics to build instances
instance (SimpleFormC t m, VFormBuilderC t m a)=>FormBuilder t m (Maybe a) 

instance (SimpleFormC t m, VFormBuilderC t m a, VFormBuilderC t m b)=>FormBuilder t m (Either a b)

avToEither::AccValidation a b -> Either a b
avToEither (AccSuccess x) = Right x
avToEither (AccFailure x) = Left x

eitherToAV::Either a b->AccValidation a b
eitherToAV (Left x) = AccFailure x
eitherToAV (Right x) = AccSuccess x

instance (SimpleFormC t m, VFormBuilderC t m a, VFormBuilderC t m b)=>FormBuilder t m (AccValidation a b) where
  buildForm va mFN mInitialDyn = eitherToAV <$> buildForm (fmap avToEither . va . eitherToAV) mFN (fmap avToEither <$> mInitialDyn)

-- | Enums become dropdowns
instance {-# OVERLAPPABLE #-} (SimpleFormInstanceC t m,Enum a,Show a,Bounded a, Eq a)=>FormBuilder t m a where
  buildForm va mFN mInitialDyn = makeSimpleFormR $ do
    let values = [minBound..] :: [a]
        initial = head values
    inputEv <- mDynToInputEv mInitialDyn
    sfWidget' inputEv initial id va showText mFN Nothing $ (\c -> _widget0_value <$> htmlDropdownStatic values showText Prelude.id c)


-- |  Tuples. 2,3,4,5 tuples are here.  TODO: add more? Maybe write a TH function to do them to save space here?  Since I'm calling mkDyn anyway
-- generics for (,) since mkDyn is not an optimization here
instance (SimpleFormC t m, VFormBuilderC t m a, VFormBuilderC t m b)=>FormBuilder t m (a,b)

instance (SimpleFormC t m, VFormBuilderC t m a, VFormBuilderC t m b, VFormBuilderC t m c)=>FormBuilder t m (a,b,c) where
  buildForm va mFN mTupDyn = validateForm va . makeSimpleFormR $ do
      maW <- unSF $ buildForm' Nothing $ maybe Nothing (Just . fmap sel1) mTupDyn
      mbW <- unSF $ buildForm' Nothing $ maybe Nothing (Just . fmap sel2) mTupDyn
      mcW <- unSF $ buildForm' Nothing $ maybe Nothing (Just . fmap sel3) mTupDyn
      return $ (,,) <$> maW <*> mbW <*> mcW


instance (SimpleFormC t m, VFormBuilderC t m a, VFormBuilderC t m b, VFormBuilderC t m c, VFormBuilderC t m d)=>FormBuilder t m (a,b,c,d) where
  buildForm va mFN mTupDyn = validateForm va . makeSimpleFormR $ do
      maW <- unSF $ buildForm' Nothing $ maybe Nothing (Just . fmap sel1) mTupDyn
      mbW <- unSF $ buildForm' Nothing $ maybe Nothing (Just . fmap sel2) mTupDyn
      mcW <- unSF $ buildForm' Nothing $ maybe Nothing (Just . fmap sel3) mTupDyn
      mdW <- unSF $ buildForm' Nothing $ maybe Nothing (Just . fmap sel4) mTupDyn
      return $ (,,,) <$> maW <*> mbW <*> mcW <*> mdW

instance (SimpleFormC t m, VFormBuilderC t m a, VFormBuilderC t m b, VFormBuilderC t m c, VFormBuilderC t m d, VFormBuilderC t m e)=>FormBuilder t m (a,b,c,d,e) where
  buildForm va mFN mTupDyn = validateForm va . makeSimpleFormR $ do
      maW <- unSF $ buildForm' Nothing $ maybe Nothing (Just . fmap sel1) mTupDyn
      mbW <- unSF $ buildForm' Nothing $ maybe Nothing (Just . fmap sel2) mTupDyn
      mcW <- unSF $ buildForm' Nothing $ maybe Nothing (Just . fmap sel3) mTupDyn
      mdW <- unSF $ buildForm' Nothing $ maybe Nothing (Just . fmap sel4) mTupDyn
      meW <- unSF $ buildForm' Nothing $ maybe Nothing (Just . fmap sel5) mTupDyn
      return $ (,,,,) <$> maW <*> mbW <*> mcW <*> mdW <*> meW

