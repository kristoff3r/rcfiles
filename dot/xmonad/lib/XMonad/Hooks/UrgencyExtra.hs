module XMonad.Hooks.UrgencyExtra (LibNotifyUrgencyHook(..)) where

import XMonad
import XMonad.Hooks.UrgencyHook(UrgencyHook(..))
import XMonad.Util.NamedWindows(getName)
import XMonad.Util.Run(safeSpawn)
import qualified XMonad.StackSet as W

-- Implementation of LibNotifyUrgencyHook
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
 
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name <- getName w
        ws <- gets windowset
        whenJust (W.findTag w ws) (flash name)
      where flash name index =
                safeSpawn "notify-send" [show name ++ " requests your attention on workspace " ++ index]
