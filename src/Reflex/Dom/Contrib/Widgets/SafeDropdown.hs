{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeFamilies      #-}
module Reflex.Dom.Contrib.Widgets.SafeDropdown
  (
    SafeDropdown(..)
  , safeDropdown_value
  , safeDropdown_change
  , safeDropdownWrappedWidgetResult
  , SafeDropdownConfig(..)
  , safeDropdownConfig_setValue
  , safeDropdownConfig_attributes
  , safeDropdown
  , safeDropdownOfLabelKeyedValue
  ) where

import Reflex.Dom.Contrib.DynamicUtils (dynAsEv)
import Reflex.Dom.Contrib.Widgets.WidgetResult (WrappedWidgetResult, unsafeBuildWrappedWidgetResult)

import Reflex (Dynamic,Event,Reflex,never,attachWithMaybe,leftmost)
import Reflex.Dynamic (updated,constDyn,current,tagPromptlyDyn)
import Reflex.Dom (widgetHold,dropdown,DropdownConfig(..))
import qualified Reflex.Dom as RD
import qualified Reflex as R

import Control.Lens (makeLenses)
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Data.Default
import Safe (headMay)
import qualified Data.Map as M
import qualified Data.Text as T

data SafeDropdown t k
  = SafeDropdown { _safeDropdown_value :: Dynamic t (Maybe k)
                 , _safeDropdown_change :: Event t (Maybe k)
                 }

data SafeDropdownConfig t k
  = SafeDropdownConfig { _safeDropdownConfig_setValue :: Event t (Maybe k)
                       , _safeDropdownConfig_attributes :: Dynamic t (M.Map T.Text T.Text)
                       }

instance Reflex t => Default (SafeDropdownConfig t k) where
  def = SafeDropdownConfig never (constDyn M.empty)


-- | Safe dropdown widget
-- 1. Needs no default, though you can supply one.  Otherwise supply Nothing.  If supplied, it's checked to see if in map and ignored if not.
-- 2. Checks if value set by setValue is present.  Ignores if not. Forwards to internal dropdown otherwise.
-- 3. Checks if current selection has been removed. Switches to something else (if possible) in that case.
-- 4. Disappears from DOM (via widgetHold) if map of options is empty.  Reappears when there are options.
-- 5. value and change are both Maybe to account for the possibility of an empty set of options.
-- Change could still be k but then not fire when options become empty?
safeDropdown :: forall k t m. (RD.DomBuilder t m, MonadFix m, R.MonadHold t m, RD.PostBuild t m, Ord k)
  => Maybe k -> Dynamic t (M.Map k T.Text) -> SafeDropdownConfig t k ->m (SafeDropdown t k)
safeDropdown k0m optionsDyn (SafeDropdownConfig setEv attrsDyn) = do
  postbuild <- RD.getPostBuild
  optionsNullEv <- R.holdUniqDyn (M.null <$> optionsDyn) >>= dynAsEv
  let (someOptionsEv, noOptionsEv) = fanBool optionsNullEv
      (setToNothingEv, keySetEv) = R.fanEither $ maybe (Left ()) Right <$> leftmost [setEv, Just <$> R.fmapMaybe (const k0m) postbuild]
      noOptionsWidget ev = return $ SafeDropdown (constDyn Nothing) ev
      noOptionsWidgetEv = noOptionsWidget (Nothing <$ noOptionsEv) <$ leftmost [noOptionsEv, setToNothingEv]
      defaultKeyNewOptionsEv = R.fmapMaybe id $ tagPromptlyDyn (headMay . M.keys <$> optionsDyn) someOptionsEv
      keySetSafeEv =
        let checkKey m k = if M.member k m then Just k else Nothing
        in attachWithMaybe checkKey (current optionsDyn) keySetEv
        
      dropdownWidget k0 = mdo
        let newK curK m = if M.member curK m then Nothing else headMay $ M.keys m
            newKEv = attachWithMaybe newK (current ddVal) (updated optionsDyn)
            k0removed oldK0 m = if M.member oldK0 m then Nothing else headMay $ M.keys m
            k0removedEv = attachWithMaybe k0removed (current k0Dyn) (updated optionsDyn)
            ddConfig = DropdownConfig (leftmost [newKEv, keySetSafeEv]) attrsDyn  
        k0Dyn <- R.holdDyn k0 k0removedEv
        ddDyn <- RD.widgetHold (dropdown k0 optionsDyn ddConfig) $ (\x->dropdown x optionsDyn ddConfig) <$> k0removedEv
        let ddVal = join $ RD._dropdown_value <$> ddDyn
        ddChangeEvEv <- dynAsEv $ RD._dropdown_change <$> ddDyn
        ddChangeEvBeh <- R.hold R.never ddChangeEvEv
        let ddChangeEv = R.switch ddChangeEvBeh
        return $ SafeDropdown (Just <$> ddVal) (Just <$> ddChangeEv)
        
      newWidgetEv = leftmost [noOptionsWidgetEv, dropdownWidget <$> defaultKeyNewOptionsEv]
  safeDyn <- widgetHold (noOptionsWidget never) newWidgetEv -- Dynamic t (SafeDropdown t k)
  return $ SafeDropdown (join $ _safeDropdown_value <$> safeDyn) (R.switch . current $ _safeDropdown_change <$> safeDyn)


safeDropdownOfLabelKeyedValue :: forall m t v l. (RD.DomBuilder t m
                                                 , MonadFix m
                                                 , R.MonadHold t m
                                                 , RD.PostBuild t m
                                                 , Ord l)
  => (l -> T.Text) -> Maybe l -> Dynamic t (M.Map l v) -> SafeDropdownConfig t l -> m (SafeDropdown t v)
safeDropdownOfLabelKeyedValue labelToText l0m optionsDyn cfg = do
  let sdOptionsDyn = M.mapWithKey (\l _ -> labelToText l) <$> optionsDyn
      maybeLookup opts ml = ml >>= flip M.lookup opts
      mapEvent = R.attachWith maybeLookup (current optionsDyn) 
      mapDynamic = R.zipDynWith maybeLookup optionsDyn 
  SafeDropdown valDyn changeEv <- safeDropdown l0m sdOptionsDyn cfg -- SafeDropdown t l
  return $ SafeDropdown (mapDynamic valDyn) (mapEvent changeEv)

boolToEither :: Bool -> Either () ()
boolToEither True = Right ()
boolToEither False = Left ()

-- NB: right event fires if true, left if false--(FalseEv,TrueEv)--which fits Either but is not intuitive, at least to me
fanBool :: Reflex t => Event t Bool -> (Event t (), Event t ())
fanBool = R.fanEither . fmap boolToEither  

concat <$> mapM makeLenses
  [ ''SafeDropdown
  , ''SafeDropdownConfig
  ]

-- this is okay because of how SafeDropDown is built. 
safeDropdownWrappedWidgetResult :: Reflex t => SafeDropdown t k -> WrappedWidgetResult t Maybe k
safeDropdownWrappedWidgetResult sdd = unsafeBuildWrappedWidgetResult (_safeDropdown_value sdd) (_safeDropdown_change sdd) 

instance RD.HasAttributes (SafeDropdownConfig t k) where
  type Attrs (SafeDropdownConfig t k) = Dynamic t (M.Map T.Text T.Text)
  attributes = safeDropdownConfig_attributes

instance RD.HasSetValue (SafeDropdownConfig t k) where
  type SetValue (SafeDropdownConfig t k) = Event t (Maybe k)
  setValue = safeDropdownConfig_setValue
