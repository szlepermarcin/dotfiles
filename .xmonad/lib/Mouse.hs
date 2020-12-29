module Mouse (myMouseBindings) where

import qualified Data.Map as M
import           XMonad
import qualified XMonad.StackSet as W

myMouseBindings
  :: XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ())
myMouseBindings XConfig { XMonad.modMask = modMask } = M.fromList
  [ ((modMask, 1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  , ((modMask, 2), \w -> focus w >> windows W.shiftMaster)
  , ( (modMask, 3)
    , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)]

