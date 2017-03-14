{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Css where

import           Data.ByteString                             (empty)
import           Reflex.Dom.Contrib.CssUtils                 (CssLink (..),
                                                              CssLinks (..))
import           Reflex.Dom.Contrib.Layout.Types             (emptyCss,
                                                              oneClass)
import           Reflex.Dom.Contrib.FormBuilder.AllDefault
import           Reflex.Dom.Contrib.FormBuilder.Configuration

import           Data.Default                                (def)

bootstrapFIncludedCss = FormIncludedCss empty (CssLinks [bootstrapLink])

bootstrapLink::CssLink
bootstrapLink = CssLink
  "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
  (Just "sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u")
  (Just "anonymous")

bootstrapFConfig::DefaultConfigurationC t m=>FormConfiguration t m
bootstrapFConfig = bootstrapCss $ def

bootstrapCss::FConfigChanger t m
bootstrapCss =
  let cssCfg = CssConfiguration
        (const $ oneClass "container")
        (const $ oneClass "form-group") (const $ oneClass "form-control") (const emptyCss) (const emptyCss)
  in (\cfg -> cfg { _cssConfig = cssCfg })
