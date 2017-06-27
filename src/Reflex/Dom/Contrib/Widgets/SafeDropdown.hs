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
import Reflex.Dom.Contrib.EventUtils (fanBool)
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
import Data.Bool (bool)
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
-- 2. Checks if value set by setValue is present.  Ignores if not.
-- 3. Checks if current selection has been removed and switches to something else if input map is non-empty.
-- 4. Checks if current default has been removed and switches, via widgetHold, to something else if input map is non-empty.
-- 5. Disappears from DOM (via widgetHold) if map of options is empty.  Reappears when there are options.
-- 6. Value and change are both Maybe to account for the possibility of an empty set of options.
-- Change could still be k but then not fire when options become empty? Then the event doesn't indicate the transition.
safeDropdown :: forall k t m. ( RD.DomBuilder t m
                              , MonadFix m
                              , R.MonadHold t m
                              , RD.PostBuild t m, Ord k)
  => Maybe k
  -> Dynamic t (M.Map k T.Text)
  -> SafeDropdownConfig t k
  -> m (SafeDropdown t k)
safeDropdown k0m optionsDyn (SafeDropdownConfig setEv attrsDyn) = do
  postbuild <- RD.getPostBuild
  optionsNullEv <- R.updated <$> R.holdUniqDyn (M.null <$> optionsDyn) 
  let (someOptionsEv, noOptionsEv) = fanBool optionsNullEv
      (setToNothingEv, keySetEv) = R.fanEither $ maybe (Left ()) Right <$> leftmost [setEv, Just <$> R.fmapMaybe (const k0m) postbuild]
      keySetSafeEv = 
        let checkKey m k = if M.member k m then Just k else Nothing
        in attachWithMaybe checkKey (current optionsDyn) keySetEv
  safeKeyDyn <- R.holdDyn Nothing (Just <$> keySetSafeEv)  
  let noOptionsWidget ev = return $ SafeDropdown (constDyn Nothing) ev
      noOptionsWidgetEv = noOptionsWidget (Nothing <$ noOptionsEv) <$ leftmost [ noOptionsEv, setToNothingEv]
      defaultKeyOnStartEv = R.fmapMaybe id $ tagPromptlyDyn safeKeyDyn postbuild
      defaultKeyNewOptionsEv = R.fmapMaybe id $ tagPromptlyDyn (headMay . M.keys <$> optionsDyn) someOptionsEv
      newDropDownEv = R.leftmost [defaultKeyOnStartEv, defaultKeyNewOptionsEv]
      
      dropdownWidget k0 = mdo
        let newK curK m = if M.member curK m then Nothing else headMay $ M.keys m
            newKEv = attachWithMaybe newK (current ddVal) (updated optionsDyn)
            k0removed oldK0 m = if M.member oldK0 m then Nothing else headMay $ M.keys m
            k0removedEv = attachWithMaybe k0removed (current k0Dyn) (updated optionsDyn)
            innerSetEv = leftmost [newKEv, keySetSafeEv]
            ddConfig = DropdownConfig innerSetEv attrsDyn  
        k0Dyn <- R.holdDyn k0 k0removedEv
        ddDyn <- RD.widgetHold (dropdown k0 optionsDyn ddConfig) $ (\x->dropdown x optionsDyn ddConfig) <$> k0removedEv
        let ddVal = join $ RD._dropdown_value <$> ddDyn
        ddChangeEvEv <- dynAsEv $ RD._dropdown_change <$> ddDyn
        ddChangeEvBeh <- R.hold R.never ddChangeEvEv
        let ddChangeEv = R.leftmost [newKEv, k0removedEv, R.switch ddChangeEvBeh]
        return $ SafeDropdown (Just <$> ddVal) (Just <$> ddChangeEv)

      dropdownWidgetEv = dropdownWidget <$> newDropDownEv  
      newWidgetEv =  leftmost [noOptionsWidgetEv, dropdownWidgetEv]
  safeDyn <- widgetHold (noOptionsWidget never) newWidgetEv -- Dynamic t (SafeDropdown t k)
  return $ SafeDropdown (join $ _safeDropdown_value <$> safeDyn) (R.switch . current $ _safeDropdown_change <$> safeDyn)



-- | Safe dropdown, applied to a map of where the dropdown labels are a function of the keys and the values we want
-- to select are the values of the map.  This requires a function for getting a text representation of the map keys.
-- NB: THe set event is if type l, the input map key, but the return values are of type v, the map value.
safeDropdownOfLabelKeyedValue :: forall m t v l. (RD.DomBuilder t m
                                                 , MonadFix m
                                                 , R.MonadHold t m
                                                 , RD.PostBuild t m
                                                 , Ord l)
  => (l -> v -> T.Text)
  -> Maybe l
  -> Dynamic t (M.Map l v)
  -> SafeDropdownConfig t l
  -> m (SafeDropdown t v)
safeDropdownOfLabelKeyedValue labelToText l0m optionsDyn cfg = do
  let sdOptionsDyn = M.mapWithKey (\l v -> labelToText l v) <$> optionsDyn -- Map l Text
      maybeLookup opts ml = ml >>= flip M.lookup opts -- Map l v -> Maybe l -> Maybe v
      mapEvent = R.attachWith maybeLookup (current optionsDyn) -- Event t (Maybe l) -> Event t (Maybe v)
      mapDynamic = R.zipDynWith maybeLookup optionsDyn 
  SafeDropdown valDyn changeEv <- safeDropdown l0m sdOptionsDyn cfg -- SafeDropdown t l
  return $ SafeDropdown (mapDynamic valDyn) (mapEvent changeEv)

-- this is safe because of how SafeDropDown is built. 
safeDropdownWrappedWidgetResult :: Reflex t => SafeDropdown t k -> WrappedWidgetResult t Maybe k
safeDropdownWrappedWidgetResult sdd = unsafeBuildWrappedWidgetResult (_safeDropdown_value sdd) (_safeDropdown_change sdd) 

concat <$> mapM makeLenses
  [ ''SafeDropdown
  , ''SafeDropdownConfig
  ]

instance RD.HasAttributes (SafeDropdownConfig t k) where
  type Attrs (SafeDropdownConfig t k) = Dynamic t (M.Map T.Text T.Text)
  attributes = safeDropdownConfig_attributes

instance RD.HasSetValue (SafeDropdownConfig t k) where
  type SetValue (SafeDropdownConfig t k) = Event t (Maybe k)
  setValue = safeDropdownConfig_setValue
