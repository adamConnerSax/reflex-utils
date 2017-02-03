{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Contrib.Layout.GridConfigs
       (
           pure5GridConfig
         , pure24GridConfig
         , skeleton12GridConfig
         , flexboxGridConfig
       ) where

import Reflex.Dom.Contrib.Layout.Types (CssClass(..),CssClasses(..),CssGridConfig(..))

import qualified Data.Text as T

pureRow::CssClass
pureRow = CssClass "pure-g"

pure5Cols::CssClasses
pure5Cols = CssClasses $ fmap (\x->CssClass $ T.pack $ "pure-u-" ++ show x ++ "-5") [1..5]                

pure5GridConfig::CssGridConfig
pure5GridConfig = CssGridConfig pureRow pure5Cols

pure24Cols::CssClasses
pure24Cols = CssClasses $ fmap (\x->CssClass $ T.pack $ "pure-u-" ++ show x ++ "-24") [1..24]

pure24GridConfig::CssGridConfig
pure24GridConfig = CssGridConfig pureRow pure24Cols

skeletonRow::CssClass
skeletonRow = CssClass "row"

skeleton12Cols::CssClasses
skeleton12Cols = CssClasses $ (\x->CssClass $ T.pack $ x ++ " column") <$> ["one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve"] 

skeleton12GridConfig::CssGridConfig
skeleton12GridConfig = CssGridConfig skeletonRow skeleton12Cols

flexboxGridRow::CssClass
flexboxGridRow = CssClass "row"

flexboxGridColumns::CssClasses
flexboxGridColumns = CssClasses $ (\x->CssClass $ T.pack $ "col-xs-" ++ show x) <$> [1..12]

flexboxGridConfig::CssGridConfig
flexboxGridConfig = CssGridConfig flexboxGridRow flexboxGridColumns
