{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module Reflex.Dom.Contrib.DynamicForm
       (
         DynamicForm(..)
       , makeDynamicForm
       , HasDFLayout(..)
       , liftDF
       )
       where

import GHC.Generics
import Control.Monad.IO.Class (MonadIO)
import Control.Lens ((^.))
import Control.Monad (foldM,join,when)
import Control.Monad.Reader (ReaderT,runReaderT,lift,ask)
import Control.Applicative (liftA2)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid ((<>))

import qualified Reflex as R 
import qualified Reflex.Dom as RD

import Reflex.Dom.Contrib.Widgets.Common --(HtmlWidget,combineWidgets)

import Reflex.Dom.Contrib.Layout.Types (LayoutM,CssClasses,IsCssClass(..))
import Reflex.Dom.Contrib.Layout.Core() --for LayoutM instances


class HasDFLayout e t m | e->t,e->m where
  containerLayout::RD.MonadWidget t m=>e->LayoutM t m a -> LayoutM t m a
  containedItemLayout::RD.MonadWidget t m=>e->LayoutM t m a->LayoutM t m a
  multiItemLayout::RD.MonadWidget t m=>e->LayoutM t m a -> LayoutM t m a
  itemLayout::RD.MonadWidget t m=>e->LayoutM t m a -> LayoutM t m a
  itemStyle::e->CssClasses
  showTypeNames::e->Bool
  showConstructorNames::e->Bool
  showFieldNames::e->Bool


type DFM e t m = ReaderT e (LayoutM t m)
type DynMaybe t a = R.Dynamic t (Maybe a)
type DFMC e t m = (RD.MonadWidget t m,HasDFLayout e t m)
type DFMPC e t m = (DFMC e t m, MonadIO (R.PushM t))


liftDF::RD.MonadWidget t m=>(LayoutM t m a -> LayoutM t m b) -> DFM e t m a -> DFM e t m b
liftDF f x = do
  env <- ask
  let ma = runReaderT x env
  lift $ f ma


class DynamicForm e t m a {-| e->t,t->m -} where
  dynFormW::DFMC e t m=>a->DFM e t m (DynMaybe t a)
  
  default dynFormW::(Generic a, GDynamicForm e t m (Rep a),DFMPC e t m)=>a->DFM e t m (DynMaybe t a)
  dynFormW x = do
    mRepDyn <- gDynFormW (from x)
    lift $ R.mapDyn (fmap to) mRepDyn
{-
  newOne::Monad m=>ReaderT e m a

  default newOne::(Default a,Monad m)=>ReaderT e m a
  newOne = return def
-}

makeDynamicForm::(DynamicForm e t m a,DFMC e t m)=>e->a->LayoutM t m (DynMaybe t a)
makeDynamicForm env a = runReaderT (dynFormW a) env

class GDynamicForm e t m f  where
  gDynFormW::DFMPC e t m=>f a -> DFM e t m (DynMaybe t (f a)) 

instance GDynamicForm e t m U1 where
  gDynFormW U1 = return $ R.constDyn Nothing 

instance (GDynamicForm e t m a, GDynamicForm e t m b) => GDynamicForm e t m (a :*: b) where
  gDynFormW (x :*: y) = do
    e <- ask
    liftDF (multiItemLayout e) $ do
      wa <- gDynFormW x
      wb <- gDynFormW y
      lift $ R.combineDyn (liftA2 (:*:)) wa wb

instance (GDynamicForm e t m a, GDynamicForm e t m b) => GDynamicForm e t m (a :+: b) where
  gDynFormW (L1 x) = join $ (R.mapDyn (fmap L1)) <$> gDynFormW x
  gDynFormW (R1 y) = join $ (R.mapDyn (fmap R1)) <$> gDynFormW y

instance DynamicForm e t m a=>GDynamicForm e t m (K1 i a) where
  gDynFormW (K1 x) = join $ R.mapDyn (fmap K1) <$> dynFormW x

multiIf::(RD.MonadWidget t m,HasDFLayout e t m)=>(e->Bool)->DFM e t m a->DFM e t m a
multiIf cond dfma = do
  env <- ask
  if (cond env) then liftDF (multiItemLayout env) dfma else dfma
    
instance (GDynamicForm e t m f, Datatype c)=>GDynamicForm e t m (M1 D c f) where
  gDynFormW s@(M1 x) = do
    env <- ask
    multiIf showTypeNames  $ do
      w<-join $ R.mapDyn (fmap M1) <$> gDynFormW x
      when (showTypeNames env) $ liftDF (itemLayout env) $ lift $ RD.el "span" $ RD.text $ "(" ++ (datatypeName s) ++ ")"
      return w

instance (GDynamicForm e t m f,Constructor c)=>GDynamicForm e t m (M1 C c f) where
  gDynFormW s@(M1 x) = do
    env <- ask
    multiIf showConstructorNames $ do
      w<-join $ R.mapDyn (fmap M1) <$> gDynFormW x
      when (showConstructorNames env) $ liftDF (itemLayout env) $ lift $ RD.el "span" $ RD.text $ "(" ++ (conName s) ++ ")"
      return w

instance (GDynamicForm e t m f,Selector c)=>GDynamicForm e t m (M1 S c f) where
  gDynFormW s@(M1 x) = do
    env <- ask
    multiIf showFieldNames $ do
      w<-join $ R.mapDyn (fmap M1) <$> gDynFormW x
      let fieldName = selName s
      when (showFieldNames env && not (null fieldName)) $ liftDF (itemLayout env) $ lift $ RD.el "span" $ RD.text $ "(" ++ fieldName ++ ")"
      return w


--  gDynFormW (M1 x) = join $ R.mapDyn (fmap M1) <$> gDynFormW x


-- instances
-- ignoring layout for now
ta::String->M.Map String String
ta x = ("title" RD.=: x)

cl::CssClasses->M.Map String String
cl x = ("class" RD.=: toCssString x)

makeDFAttrs::(RD.MonadWidget t m, HasDFLayout e t m)=>String->DFM e t m (R.Dynamic t (M.Map String String))
makeDFAttrs typeS = do
  env <- ask
  return $ R.constDyn ((ta typeS) <> cl (itemStyle env))

instance (MonadIO (R.PushM t))=>DynamicForm e t m Int where
  dynFormW initial = do
    env <- ask
    attrsDyn <- makeDFAttrs "Int"
    lift $ itemLayout env $ _hwidget_value <$> intWidget (WidgetConfig RD.never (Just initial) attrsDyn)

instance (MonadIO (R.PushM t))=>DynamicForm e t m Double where
  dynFormW initial = do
    e <- ask
    attrsDyn <- makeDFAttrs "Double"
    lift $ itemLayout e $ _hwidget_value <$> doubleWidget (WidgetConfig RD.never (Just initial) attrsDyn)

instance (MonadIO (R.PushM t))=>DynamicForm e t m T.Text where
  dynFormW initial = do
    e <- ask
    attrsDyn <- makeDFAttrs "Text"
    lift $ do
      tW <- itemLayout e $ _hwidget_value <$> htmlTextInput "WTF" (WidgetConfig RD.never (T.unpack initial) attrsDyn)
      R.mapDyn (Just . T.pack) tW

instance {-# OVERLAPPING #-} (R.MonadHold t m,MonadIO (R.PushM t))=>DynamicForm e t m String where
  dynFormW initial = do
    e <- ask
    attrsDyn <- makeDFAttrs "String"
    lift $ do
      tW <- itemLayout e $ _hwidget_value <$> htmlTextInput "WTF" (WidgetConfig RD.never initial attrsDyn)
      R.mapDyn Just tW


instance {-# OVERLAPPABLE #-} (MonadIO (R.PushM t),
                               Enum a,Show a,Bounded a, Eq a)=>DynamicForm e t m a where
  dynFormW initial = do
    e <- ask
    attrsDyn <- makeDFAttrs "Enum"
    lift $ itemLayout e $ do
      eW <- _widget0_value <$> htmlDropdownStatic [minBound..] show Prelude.id (WidgetConfig RD.never initial attrsDyn)
      R.mapDyn Just eW


-- TODO: these map and list do not allow new entries or deletion. That should be doable.  
instance {-# OVERLAPPABLE #-} (MonadIO (R.PushM t),DynamicForm e t m a)=>DynamicForm e t m [a] where
  dynFormW as = do
    e <- ask
    let appendMaybe mas ma = do
          a <- ma
          as <- mas
          return (as ++ [a])
    widgetList <- liftDF (containerLayout e) $ mapM ((liftDF $ containedItemLayout e) . dynFormW) as
    foldM (R.combineDyn appendMaybe) (R.constDyn (Just [])) widgetList

instance (MonadIO (R.PushM t), DynamicForm e t m a, DynamicForm e t m b)=>DynamicForm e t m (a,b) where
  dynFormW (a0,b0) = do
    e <- ask
    liftDF (multiItemLayout e) $ do
      maW <- dynFormW a0
      mbW <- dynFormW b0
      lift $ R.combineDyn (liftA2 (,)) maW mbW

instance (MonadIO (R.PushM t), DynamicForm e t m k, Ord k, DynamicForm e t m a)=>DynamicForm e t m (M.Map k a) where
  dynFormW as = do
    e <- ask
    let appendMaybe mas ma = do
          a <- ma
          as <- mas
          return (as ++ [a])
    widgetList <- liftDF (containerLayout e) $ mapM ((liftDF $ containedItemLayout e) . dynFormW) (M.toList as)
    widgetOfList <- lift $ foldM (R.combineDyn appendMaybe) (R.constDyn (Just [])) widgetList
    lift $ R.mapDyn (\x->M.fromList <$> x) widgetOfList



