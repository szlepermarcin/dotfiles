module ManageHook (myManageHook) where

import           Control.Monad
import           Workspaces                 (myWorkspaces)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.ManageHook
import qualified XMonad.StackSet            as W

myManageHook = composeAll . concat
  $ [ [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [className =? c --> doShift (head myWorkspaces)
        <+> viewShift (head myWorkspaces)
      | c <- my1Shifts]
    , [className =? c --> doShift (myWorkspaces !! 1)
        <+> viewShift (myWorkspaces !! 1)
      | c <- my2Shifts]
    , [className =? c --> doShift (myWorkspaces !! 2)
        <+> viewShift (myWorkspaces !! 2)
      | c <- my3Shifts]
    , [className =? c --> doShift (myWorkspaces !! 3)
        <+> viewShift (myWorkspaces !! 3)
      | c <- my4Shifts]
    , [className =? c --> doShift (myWorkspaces !! 4)
        <+> viewShift (myWorkspaces !! 4)
      | c <- my5Shifts]
    , [className =? c --> doShift (myWorkspaces !! 5)
        <+> viewShift (myWorkspaces !! 5)
      | c <- my6Shifts]
    , [className =? c --> doShift (myWorkspaces !! 6)
        <+> viewShift (myWorkspaces !! 6)
      | c <- my7Shifts]
    , [className =? c --> doShift (myWorkspaces !! 7)
        <+> viewShift (myWorkspaces !! 7)
      | c <- my8Shifts]
    , [className =? c --> doShift (myWorkspaces !! 8)
        <+> viewShift (myWorkspaces !! 8)
      | c <- my9Shifts]
    , [className =? c --> doShift (myWorkspaces !! 9)
        <+> viewShift (myWorkspaces !! 9)
      | c <- my10Shifts]
    , [manageDocks]
    ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift

    myCFloats = [ "Arandr"
                , "Galculator"
                , "Gimp"
                , "feh"
                , "mpv"
                , "Xfce4-terminal"
                , "VirtualBox Manager"
                , "zoom"
                ]

    myTFloats = ["Downloads", "Save As..."]

    myRFloats = []

    myIgnores = ["desktop_window", "Trayer"]

    my1Shifts = ["Chromium", "Vivaldi-stable", "Firefox", "Brave-browser"]

    my2Shifts = []

    my3Shifts = ["Inkscape"]

    my4Shifts = []

    my5Shifts = ["Gimp", "feh"]

    my6Shifts = ["vlc", "mpv"]

    my7Shifts = ["VirtualBox Manager"]

    my8Shifts = ["Thunar"]

    my9Shifts = ["Slack"]

    my10Shifts = ["discord", "zoom"]

