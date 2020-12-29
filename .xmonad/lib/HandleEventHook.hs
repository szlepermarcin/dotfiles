module HandleEventHook (myHandleEventHook) where

import           Data.Monoid
import           XMonad
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.Minimize
import           XMonad.ManageHook

myHandleEventHook :: Event -> X All
myHandleEventHook =
  fullscreenEventHook <+> docksEventHook <+> minimizeEventHook
