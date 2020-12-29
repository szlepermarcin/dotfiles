import           XMonad
import           XMonad.Hooks.EwmhDesktops
import           Autostart (myStartupHook)
import           HandleEventHook (myHandleEventHook)
import           Keys (myKeys)
import           Layout (myLayoutHook)
import           ManageHook (myManageHook)
import           Mouse (myMouseBindings)
import           Workspaces (myWorkspaces)
import           Xmobar (spawnXmobar, myXmobarConf)

mydefaults =
  def { terminal = "terminator"
      , normalBorderColor = "#4c566a"
      , focusedBorderColor = "#5e81ac"
      , focusFollowsMouse = True
      , mouseBindings = myMouseBindings
      , workspaces = myWorkspaces
      , keys = myKeys
      , modMask = mod4Mask
      , borderWidth = 2
      , layoutHook = myLayoutHook
      , startupHook = myStartupHook
      , manageHook = myManageHook
      , handleEventHook = myHandleEventHook
      }

main :: IO ()
main = do
  myXmobarWrapper <- myXmobarConf <$> spawnXmobar
  xmonad $ ewmh $ myXmobarWrapper mydefaults
