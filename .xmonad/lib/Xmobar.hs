module Xmobar (spawnXmobar, myXmobarConf) where

import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Util.Run


myTitleColor = "#c91a1a" -- color of window title

myTitleLength = 80 -- truncate window title to this length

myCurrentWSColor = "#6790eb" -- color of active workspace

myVisibleWSColor = "#aaaaaa" -- color of inactive workspace

myUrgentWSColor = "#c91a1a" -- color of workspace with 'urgent' window

myHiddenNoWindowsWSColor = "white"

spawnXmobar :: IO Handle
spawnXmobar = spawnPipe "xmobar -x 0 $HOME/.xmonad/.xmobarrc "

myXmobarConf :: Handle -> XConfig a -> XConfig a
myXmobarConf h conf =
  conf { logHook = dynamicLogWithPP
           $ def { ppOutput = System.IO.hPutStrLn h
                 , ppTitle = xmobarColor myTitleColor "" . const ""
                 , ppCurrent = xmobarColor myCurrentWSColor "" . wrap "" ""
                 , ppVisible = xmobarColor myVisibleWSColor "" . wrap "" ""
                 , ppHidden = wrap "" ""
                 , ppHiddenNoWindows = xmobarColor myHiddenNoWindowsWSColor ""
                 , ppUrgent = xmobarColor myUrgentWSColor ""
                 , ppSep = "  "
                 , ppWsSep = "  "
                 , ppLayout = \x -> case x of
                     "Spacing Tall" -> "<fn=1>Tall</fn>"
                     "Spacing Grid" -> "<fn=1>Grid</fn>"
                     "Spacing Spiral" -> "<fn=1>Spiral</fn>"
                     "Spacing ThreeCol" -> "<fn=1>ThreeColMid</fn>"
                     "Spacing Full" -> "<fn=1>Full</fn>"
                     _ -> x
                 }
       }
