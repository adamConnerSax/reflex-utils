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

module Reflex.Dom.Contrib.SimpleForm.Instances.Basic
       (
         sfWidget
       , buildReadMaybe
       , buildReadable
       , BasicC
       , SimpleFormInstanceC
       , VBuilderC
       ) where

import           Control.Lens                          (over, view, (^.))
import           Control.Monad.Fix                     (MonadFix)
import           Control.Monad.Reader                  (lift)
import qualified Data.Map                              as M
import           Data.Maybe                            (fromMaybe)
import           Data.Monoid                           ((<>))
import           Data.Readable                         (Readable)
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
-- instances

--some helpers
showText::Show a=>a->T.Text
showText = T.pack . show

type BasicC t m = (RD.DomBuilder t m, R.MonadHold t m, MonadFix m)
type SimpleFormInstanceC t m = (SimpleFormC t m, MonadWidgetExtraC t m, RD.PostBuild t m, MonadFix m)
type VBuilderC t m a = (B.Builder (SFR t m) (DynValidation t) a, B.Validatable (DynValidation t) a)

readOnlyW::(BasicC t m, RD.PostBuild t m)=>(a->T.Text)->WidgetConfig t a->m (R.Dynamic t a)
readOnlyW f wc = do
  da <- R.foldDyn const (_widgetConfig_initialValue wc) (_widgetConfig_setValue wc)
  let ds = f <$> da
  RD.elDynAttr "div" (_widgetConfig_attributes wc) $ RD.dynText ds
  return da

sfWidget::forall t m a b.(R.Reflex t,SimpleFormC t m, RD.PostBuild t m,MonadFix m)=>
          (a->b)->
          (a->T.Text)->
          Maybe FieldName->
          WidgetConfig t a->
          (WidgetConfig t a-> m (R.Dynamic t a))->
          SFR t m (R.Dynamic t b)
sfWidget fDyn fString mFN wc widget = do
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
  lift . labeledWidget $ (fmap fDyn <$> (if isObserver then readOnlyW fString wcAll else widget wcInput))

fromAccVal::AccValidation e a->a
fromAccVal (AccSuccess a) = a
fromAccVal (AccFailure _) = undefined

item::Monad m=>SFLayoutF t m
item = sfItem

instance R.Reflex t=>Functor (HtmlWidget t) where
  fmap f (HtmlWidget v c kp kd ku hf) = HtmlWidget (f <$> v) (f <$> c) kp kd ku hf

gWidgetMToAV::RD.DomBuilder t m=>GWidget t m (Maybe a)->GWidget t m (AccValidation SimpleFormErrors a)
gWidgetMToAV gwma wcav = fmap maybeToAV <$> gwma (avToMaybe <$> wcav)

buildReadable::(SimpleFormInstanceC t m, Readable a, Show a)=>B.Validator (DynValidation t) a->Maybe FieldName->Maybe a->SimpleFormR t m a
buildReadable va mFN ma = B.validateFV va . makeSimpleFormR $ mdo
  attrsDyn <- sfAttrs dma mFN Nothing
  let wc = WidgetConfig RD.never (maybeToAV ma) attrsDyn
  dma <- item $ DynValidation <$> sfWidget id (showText . fromAccVal) mFN wc (\c -> _hwidget_value <$> restrictWidget blurOrEnter (gWidgetMToAV readableWidget) c)
  return dma

readMaybeAV::Read a=>Maybe FieldName->T.Text->AccValidation SimpleFormErrors a
readMaybeAV mFN t =
  let prefix = T.pack (fromMaybe "N/A" mFN) <> ": "  in
    case readMaybe $ T.unpack t of
      Nothing -> AccFailure [SFNoParse (prefix <> t)]
      Just a -> AccSuccess a

buildReadMaybe::(SimpleFormInstanceC t m, Read a, Show a)=>B.Validator (DynValidation t) a->Maybe FieldName->Maybe a->SimpleFormR t m a
buildReadMaybe va mFN ma = B.validateFV va . makeSimpleFormR $ mdo
  attrsDyn <- sfAttrs dma mFN Nothing
  let initial = maybe "" showText ma
      wc = WidgetConfig RD.never initial attrsDyn
  dma <- item $ DynValidation <$> sfWidget (readMaybeAV mFN) showText mFN wc (\c -> _hwidget_value <$> restrictWidget blurOrEnter (htmlTextInput (maybe "" T.pack mFN)) c)
  return dma

-- | String and Text
instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) T.Text where
  buildValidated va mFN mInitial = B.validateFV va . makeSimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just "Text")
    let initial = fromMaybe "" mInitial
        wc = WidgetConfig RD.never initial attrsDyn
    dma <- item $ DynValidation <$> sfWidget AccSuccess showText mFN wc (\c -> _hwidget_value <$> restrictWidget blurOrEnter (htmlTextInput "Text") c)
    return dma


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
  buildValidated va mFN mInitial = B.validateFV va . makeSimpleFormR $ mdo
    let initial = fromMaybe False mInitial
        wc = WidgetConfig RD.never initial attrsDyn
    attrsDyn <- sfAttrs dynValidationNothing mFN (Just $ "Bool")
    item $ DynValidation <$> (sfWidget AccSuccess showText mFN wc $ \c -> _hwidget_value <$> htmlCheckbox c)

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Double where
  buildValidated va mFN mInitial = B.validateFV va . makeSimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just "Double")
    let wc = WidgetConfig RD.never (maybeToAV mInitial) attrsDyn
    dma <- item $ DynValidation <$> sfWidget id (showText . fromAccVal) mFN wc (\c -> _hwidget_value <$> restrictWidget blurOrEnter (gWidgetMToAV doubleWidget) c)
    return dma

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Float where
  buildValidated va = buildReadable va

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Int where
  buildValidated va mFN mInitial = B.validateFV va . makeSimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just "Int")
    let wc = WidgetConfig RD.never (maybeToAV mInitial) attrsDyn
    dma <- item $ DynValidation <$> sfWidget id  (showText . fromAccVal) mFN wc (\c->_hwidget_value <$> restrictWidget blurOrEnter (gWidgetMToAV intWidget) c)
    return dma


instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Integer where
  buildValidated va mFN mInitial = B.validateFV va . makeSimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "Int")
    let wc = WidgetConfig RD.never (maybeToAV mInitial) attrsDyn
    dma <- item $ DynValidation <$> sfWidget id (showText . fromAccVal) mFN wc (\c -> _hwidget_value <$> restrictWidget blurOrEnter (gWidgetMToAV integerWidget) c)
    return dma

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
  buildValidated va mFN mInitial = B.validateFV va . makeSimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "UTCTime")
    let wc = WidgetConfig RD.never (maybeToAV mInitial) attrsDyn
    dma<-item $ DynValidation <$> sfWidget id (showText . fromAccVal) mFN wc (\c -> _hwidget_value <$> restrictWidget blurOrEnter (gWidgetMToAV dateTimeWidget) c)
    return dma

instance SimpleFormInstanceC t m=>B.Builder (SFR t m) (DynValidation t) Day where
  buildValidated va mFN mInitial = B.validateFV va . makeSimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN (Just $ "Day")
    let wc = WidgetConfig RD.never (maybeToAV mInitial) attrsDyn
    dma <- item $ DynValidation <$> sfWidget id (showText . fromAccVal) mFN wc (\c -> _hwidget_value <$> restrictWidget blurOrEnter (gWidgetMToAV dateWidget) c)
    return dma

-- uses generics to build instances
instance (SimpleFormC t m, VBuilderC t m a)=>B.Builder (SFR t m) (DynValidation t) (Maybe a)

instance (SimpleFormC t m, VBuilderC t m a, VBuilderC t m b)=>B.Builder (SFR t m) (DynValidation t) (Either a b)


-- | Enums become dropdowns
instance {-# OVERLAPPABLE #-} (SimpleFormInstanceC t m,Enum a,Show a,Bounded a, Eq a)
                              =>B.Builder (SFR t m) (DynValidation t) a where
  buildValidated va mFN mInitial = B.validateFV va . makeSimpleFormR $ mdo
    attrsDyn <- sfAttrs dma mFN Nothing
    let values = [minBound..] :: [a]
        initial = fromMaybe (head values) mInitial
        wc = WidgetConfig RD.never initial attrsDyn
    dma <- item . B.validatefv va $  DynValidation <$> sfWidget AccSuccess showText mFN wc (\c -> _widget0_value <$> htmlDropdownStatic values showText Prelude.id c)
    return dma

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
      return  $ (,,) <$> maW <*> mbW <*> mcW

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
