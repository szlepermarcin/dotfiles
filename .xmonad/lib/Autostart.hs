module Autostart (myStartupHook) where

import           XMonad
import           XMonad.Hooks.SetWMName

myStartupHook :: X ()
myStartupHook = do
  spawn "$HOME/.xmonad/scripts/autostart.sh"
  setWMName "LG3D"
