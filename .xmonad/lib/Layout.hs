module Layout (myLayoutHook) where

import           XMonad.Hooks.ManageDocks
import           XMonad.Layout
import           XMonad.Layout.Gaps
import           XMonad.Layout.Grid
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.ThreeColumns

myLayoutHook = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True
  $ gaps [(U, 35), (D, 5), (R, 5), (L, 5)]
  $ avoidStruts
  $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
  $ smartBorders
  $ tiled
  ||| Grid
  ||| spiral (6 / 7)
  ||| ThreeColMid 1 (3 / 100) (1 / 2)
  ||| noBorders Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    delta = 3 / 100
    ratio = 1 / 2
