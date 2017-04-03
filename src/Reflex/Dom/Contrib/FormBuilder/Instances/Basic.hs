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
--{-# LANGUAGE DeriveGeneric         #-}

module Reflex.Dom.Contrib.FormBuilder.Instances.Basic
       (
         formWidget
       , formWidget'
       , dynAsEv
       , traceDynAsEv
       , buildDynReadMaybe
       , buildDynReadable
--       , BasicC
       , FormInstanceC
       , buildFormIso
       ) where

import           Control.Lens                          (over, view, (^.))
import           Control.Monad                         (join)
import           Control.Monad.Fix                     (MonadFix)
import           Control.Monad.Reader                  (lift)
import           Data.Functor.Compose                  (Compose(Compose,getCompose))
import qualified Data.Map                              as M
import           Data.Maybe                            (fromMaybe,fromJust) --FIX FromJust
import           Data.Monoid                           ((<>))
import           Data.Readable                         (Readable, fromText)
import qualified Data.Text                             as T
import           Data.Validation                       (AccValidation (..))
import           Text.Read                             (readMaybe)

import           Control.Lens                          (view)
import           Control.Lens.Iso                      (Iso', from, iso)

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
import           Reflex.Dom.Contrib.DynamicUtils (dynAsEv,traceDynAsEv,mDynAsEv,traceMDynAsEv,traceDynMAsEv)
import           Reflex.Dom.Contrib.Layout.Types       (emptyCss, toCssString)
import           Reflex.Dom.Contrib.ReflexConstraints
import           Reflex.Dom.Contrib.FormBuilder.Builder
import           Reflex.Dom.Contrib.FormBuilder.DynValidation (DynMaybe,constDynMaybe,dynMaybeAsEv)
-- instances

--some helpers
showText::Show a=>a->T.Text
showText = T.pack . show

type WidgetC t m = (RD.DomBuilder t m, R.MonadHold t m, MonadFix m)
type FormInstanceC t m = (WidgetC t m, MonadWidgetExtraC t m, RD.PostBuild t m)
--type VBuilderC t m a = (B.Builder (SFR t m) (DynValidation t) a, B.Validatable (DynValidation t) a)

instance {-# OVERLAPPABLE #-} B.Validatable (FValidation) a where
  validator a = AccSuccess a

readOnlyW::(RD.DomBuilder t m, R.MonadHold t m, RD.PostBuild t m)=>(a->T.Text)->WidgetConfig t a->m (R.Dynamic t a)
readOnlyW f wc = do
  da <- R.holdDyn (_widgetConfig_initialValue wc) (_widgetConfig_setValue wc)
  let ds = f <$> da
  RD.elDynAttr "div" (_widgetConfig_attributes wc) $ RD.dynText ds
  return da

formWidget::forall t m a b.(R.Reflex t,RD.DomBuilder t m, R.MonadHold t m, RD.PostBuild t m,MonadFix m)
  =>(a->b) -- map in/out type to widget type (int to text, e.g.,)
  ->(a->T.Text) -- show in/out type
  ->Maybe FieldName -- field name ?
  ->WidgetConfig t a
  ->(WidgetConfig t a->m (R.Dynamic t a)) -- underlying reflex-dom-contrib widget
  ->FR t m (R.Dynamic t b)
formWidget f fString mFN wc widget = do
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

formWidget'::(RD.DomBuilder t m, R.MonadHold t m, RD.PostBuild t m,MonadFix m)
  =>R.Event t a -- update value events
  ->b -- initial value to display in b-widget
  ->(a->b) -- map in/out type to widget type
  ->(b->FValidation a) --validate and map back
  ->(b->T.Text) -- showText for widget type
  ->Maybe FieldName -- field name ?
  ->Maybe T.Text -- type name ?
  ->(WidgetConfig t b->m (R.Dynamic t b)) -- underlying reflex-dom-contrib widget
  ->FR t m (DynValidation t a)
formWidget' updateEv initialWV toWT validateWT showWT mFN mTypeName widget = mdo
  attrsDyn <- fAttrs dva mFN mTypeName
  let wc = WidgetConfig (toWT <$> updateEv) initialWV attrsDyn
  dva <- item $ DynValidation <$> formWidget validateWT showWT mFN wc widget
  return dva


item::Monad m=>FLayoutF t m
item = fItem

instance R.Reflex t=>Functor (HtmlWidget t) where
  fmap f (HtmlWidget v c kp kd ku hf) = HtmlWidget (f <$> v) (f <$> c) kp kd ku hf

textWidgetValue::FormInstanceC t m=>Maybe FieldName->WidgetConfig t T.Text -> m (R.Dynamic t T.Text)
textWidgetValue mFN c = _hwidget_value <$> restrictWidget' blurOrEnter (htmlTextInput (maybe "" T.pack mFN)) c

textWidgetValue'::FormInstanceC t m=>Maybe FieldName->WidgetConfig t T.Text -> m (R.Dynamic t T.Text)
textWidgetValue' mFN c = _hwidget_value <$> htmlTextInput (maybe "" T.pack mFN) c

-- this does what restrictWidget does but allows the set event to change the "authoritative value"
restrictWidget'::(RD.DomBuilder t m, R.MonadHold t m)
  =>(HtmlWidget t a -> R.Event t a)
  -> GWidget t m a
  -> GWidget t m a
restrictWidget' restrictFunc wFunc cfg = do
  w <- wFunc cfg
  let e = R.leftmost [(_widgetConfig_setValue cfg), restrictFunc w]
  v <- R.holdDyn (_widgetConfig_initialValue cfg) e
  return $ w { _hwidget_value = v 
             , _hwidget_change = e
             }

parseError::Maybe FieldName->T.Text->T.Text
parseError mFN x = T.pack (fromMaybe "N/A" mFN) <> ": " <> x

parseAndValidate::Maybe FieldName->(T.Text -> Maybe a)->FormValidator a->T.Text->FValidation a
parseAndValidate mFN parse va t =
  case parse t of
    Nothing -> AccFailure [FNoParse $ parseError mFN t]
    Just y -> va y


buildDynReadable::(FormInstanceC t m, Readable a, Show a)
  =>FormValidator a
  ->Maybe FieldName
  ->DynMaybe t a
  ->Form t m a
buildDynReadable va mFN dma = makeForm $ do
  let vfwt = parseAndValidate mFN fromText va
  inputEv <- dynMaybeAsEv dma
  formWidget' inputEv "" showText vfwt showText mFN Nothing $ textWidgetValue mFN

buildDynReadMaybe::(FormInstanceC t m, Read a, Show a)
  =>FormValidator a
  ->Maybe FieldName
  ->DynMaybe t a
  ->Form t m a
buildDynReadMaybe va mFN dma = makeForm $ do
  let vfwt = parseAndValidate mFN (readMaybe . T.unpack) va
  inputEv <- dynMaybeAsEv dma
  formWidget' inputEv "" showText vfwt showText mFN Nothing $ textWidgetValue mFN

-- NB this will handle Dynamic t (Dynamic t a)) inputs
instance (FormInstanceC t m, VFormBuilderC t m a)=>FormBuilder t m (R.Dynamic t a) where
  buildForm va mFN = validateForm va . fmap R.constDyn . buildForm' mFN . Compose . join . fmap sequenceA . getCompose

-- | String and Text
instance FormInstanceC t m=>FormBuilder t m T.Text where
  buildForm va mFN initialMDyn = makeForm $ do
    inputEv <- traceDynMAsEv (\t->T.unpack $ "FormBuilder t m T.Text (val=" <> t <> ")") (getCompose initialMDyn) -- FIXTrace
--    inputEv <- maybe (return R.never) (traceDynAsEv (\t->T.unpack $ "FormBuilder t m T.Text (val=" <> t <> ")")) mInitialDyn  -- mDynAsEv mInitialDyn
    formWidget' inputEv "" id va id mFN Nothing $ textWidgetValue mFN

instance {-# OVERLAPPING #-} FormInstanceC t m=>FormBuilder t m String where
  buildForm va mFN initialMDyn =
    let va' = (\t -> T.pack <$> va (T.unpack t))
    in T.unpack <$> buildForm va' mFN (T.pack <$> initialMDyn)


{- Not clear what to do here! Default behavior is bad since Char is a huge enum.
instance FormC e t m=>B.Builder (RFormWidget e t m) Char where
  buildValidated va md mInitial = RFormWidget $ do
    e <- ask
    attrsDyn <- makeSFAttrs "Char"
    lift $ item attrs0e $ _hwidget_value <$> readableWidget (WidgetConfig RD.never mInitial attrsDyn)
-}


-- We don't need this.  If we leave it out, the Enum instance will work and we get a dropdown instead of a checkbox.  Which might be better...
instance FormInstanceC t m=>FormBuilder t m Bool where
  buildForm va mFN initialMDyn = makeForm $ do
    inputEv <- dynMaybeAsEv initialMDyn
    formWidget' inputEv False id va showText mFN Nothing $ (\c -> _hwidget_value <$> htmlCheckbox c)

instance FormInstanceC t m=>FormBuilder t m Double where
  buildForm = buildDynReadable

instance FormInstanceC t m=>FormBuilder t m Float where
  buildForm = buildDynReadable

instance FormInstanceC t m=>FormBuilder t m Int where
  buildForm = buildDynReadable

instance FormInstanceC t m=>FormBuilder t m Integer where
  buildForm = buildDynReadable

instance FormInstanceC t m=>FormBuilder t m Int8 where
  buildForm  = buildDynReadable

instance FormInstanceC t m=>FormBuilder t m Int16 where
  buildForm  = buildDynReadable

instance FormInstanceC t m=>FormBuilder t m Int32 where
  buildForm = buildDynReadable

instance FormInstanceC t m=>FormBuilder t m Int64 where
  buildForm = buildDynReadable

instance FormInstanceC t m=>FormBuilder t m Word8 where
  buildForm = buildDynReadable

instance FormInstanceC t m=>FormBuilder t m Word16 where
  buildForm = buildDynReadable

instance FormInstanceC t m=>FormBuilder t m Word32 where
  buildForm = buildDynReadable

instance FormInstanceC t m=>FormBuilder t m Word64 where
  buildForm = buildDynReadable

instance FormInstanceC t m=>FormBuilder t m ByteString where
  buildForm = buildDynReadable

--dateTime and date
instance FormInstanceC t m=>FormBuilder t m UTCTime where
  buildForm va mFN initialMDyn = makeForm $ do
    let vfwt x = case x of
          Nothing -> AccFailure [FNoParse "Couldn't parse as UTCTime."]
          Just y -> va y
        initialDateTime = Just $ UTCTime (fromGregorian 1971 1 1) (secondsToDiffTime 0)
    inputEv <- dynMaybeAsEv initialMDyn
    formWidget' inputEv initialDateTime Just vfwt (maybe "" showText) mFN Nothing $  (\c -> _hwidget_value <$> restrictWidget blurOrEnter dateTimeWidget c)


instance FormInstanceC t m=>FormBuilder t m Day where
  buildForm va mFN initialMDyn = makeForm $ do
    let vfwt x = case x of
          Nothing -> AccFailure [FNoParse "Couldn't parse as Day."]
          Just y -> va y
        initialDay = Just $ fromGregorian 1971 1 1
    inputEv <- dynMaybeAsEv initialMDyn
    formWidget' inputEv initialDay Just vfwt (maybe "" showText) mFN Nothing $  (\c -> _hwidget_value <$> restrictWidget blurOrEnter dateWidget c)


-- uses generics to build instances
instance (FormInstanceC t m, VFormBuilderC t m a)=>FormBuilder t m (Maybe a) 

instance (FormInstanceC t m, VFormBuilderC t m a, VFormBuilderC t m b)=>FormBuilder t m (Either a b)

-- if two things are Isomorphic, we can use one to build the other
-- NB: Isomorphic sum types will end up using the constructors, etc. of the one used to bootstrap 
buildFormIso::(FormInstanceC t m, VFormBuilderC t m a)=>Iso' a b->FormValidator b->Maybe FieldName->DynMaybe t b->Form t m b
buildFormIso isoAB vb mFN dmb = makeForm $ do
  let a2b = view isoAB
      b2a = view $ from isoAB
  unF $ a2b <$> buildForm (fmap b2a . vb . a2b) mFN (b2a <$> dmb)  

avToEither::AccValidation a b -> Either a b
avToEither (AccSuccess x) = Right x
avToEither (AccFailure x) = Left x

eitherToAV::Either a b->AccValidation a b
eitherToAV (Left x) = AccFailure x
eitherToAV (Right x) = AccSuccess x

-- NB: Here, AccSuccess will appear as Right and AccFailure as Left in the forms.  To Avoid this we need to build a specific instance.
instance (FormInstanceC t m, VFormBuilderC t m a, VFormBuilderC t m b)=>FormBuilder t m (AccValidation a b) where
  buildForm = buildFormIso (iso eitherToAV avToEither)

-- | Enums become dropdowns
instance {-# OVERLAPPABLE #-} (FormInstanceC t m,Enum a,Show a,Bounded a, Eq a)=>FormBuilder t m a where
  buildForm va mFN initialMDyn = makeForm $ do
    let values = [minBound..] :: [a]
        initial = head values
    inputEv <- dynMaybeAsEv initialMDyn
    formWidget' inputEv initial id va showText mFN Nothing $ (\c -> _widget0_value <$> htmlDropdownStatic values showText Prelude.id c)


-- |  Tuples. 2,3,4,5 tuples are here.  TODO: add more? Maybe write a TH function to do them to save space here?  Since I'm calling mkDyn anyway
-- generics for (,) since mkDyn is not an optimization here
instance (FormInstanceC t m, VFormBuilderC t m a, VFormBuilderC t m b)=>FormBuilder t m (a,b) where
  buildForm va mFN tupMDyn = validateForm va . makeForm $ do
      maW <- unF $ buildForm' Nothing (sel1 <$> tupMDyn)
      mbW <- unF $ buildForm' Nothing (sel2 <$> tupMDyn)
      return $ (,) <$> maW <*> mbW 
  
instance (FormInstanceC t m
         , VFormBuilderC t m a
         , VFormBuilderC t m b
         , VFormBuilderC t m c)=>FormBuilder t m (a,b,c) where
  buildForm va mFN tupMDyn = validateForm va . makeForm $ do
      maW <- unF $ buildForm' Nothing (sel1 <$> tupMDyn)
      mbW <- unF $ buildForm' Nothing (sel2 <$> tupMDyn)
      mcW <- unF $ buildForm' Nothing (sel3 <$> tupMDyn)
      return $ (,,) <$> maW <*> mbW <*> mcW


instance (FormInstanceC t m
         , VFormBuilderC t m a
         , VFormBuilderC t m b
         , VFormBuilderC t m c
         , VFormBuilderC t m d)=>FormBuilder t m (a,b,c,d) where
  buildForm va mFN tupMDyn = validateForm va . makeForm $ do
      maW <- unF $ buildForm' Nothing (sel1 <$> tupMDyn)
      mbW <- unF $ buildForm' Nothing (sel2 <$> tupMDyn)
      mcW <- unF $ buildForm' Nothing (sel3 <$> tupMDyn)
      mdW <- unF $ buildForm' Nothing (sel4 <$> tupMDyn)
      return $ (,,,) <$> maW <*> mbW <*> mcW <*> mdW

instance (FormInstanceC t m
         , VFormBuilderC t m a
         , VFormBuilderC t m b
         , VFormBuilderC t m c
         , VFormBuilderC t m d
         , VFormBuilderC t m e)=>FormBuilder t m (a,b,c,d,e) where
  buildForm va mFN tupMDyn = validateForm va . makeForm $ do
      maW <- unF $ buildForm' Nothing (sel1 <$> tupMDyn)
      mbW <- unF $ buildForm' Nothing (sel2 <$> tupMDyn)
      mcW <- unF $ buildForm' Nothing (sel3 <$> tupMDyn)
      mdW <- unF $ buildForm' Nothing (sel4 <$> tupMDyn)
      meW <- unF $ buildForm' Nothing (sel5 <$> tupMDyn)
      return $ (,,,,) <$> maW <*> mbW <*> mcW <*> mdW <*> meW

