module Keys (myKeys) where

import qualified Data.Map                            as M
import           Graphics.X11.ExtraTypes.XF86
import           System.Exit
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import qualified XMonad.StackSet                     as W

runInTerminal :: XConfig l -> [Char] -> [Char]
runInTerminal conf command = XMonad.terminal conf ++ " -e " ++ command

-- SUPER + ...
superKeys :: XConfig Layout -> [((KeyMask, KeySym), X ())]
superKeys conf@XConfig { XMonad.modMask = modMask } =
  [ ((modMask, xK_e), spawn $ runInTerminal conf "vim")
  , ((modMask, xK_f), sendMessage $ Toggle NBFULL)
  , ((modMask, xK_h), spawn $ runInTerminal conf "htop")
  , ((modMask, xK_j), windows W.focusDown)
  , ((modMask, xK_k), windows W.focusUp)
  , ((modMask, xK_m), spawn "pragha")
  , ((modMask, xK_q), kill)
  , ((modMask, xK_t), spawn $ XMonad.terminal conf)
  , ((modMask, xK_v), spawn "pavucontrol")
  , ((modMask, xK_space), sendMessage NextLayout)
  , ((modMask, xK_Escape), spawn "xkill")
  , ((modMask, xK_Return), spawn $ XMonad.terminal conf)
  , ((modMask, xK_F1), spawn "brave")
  , ((modMask, xK_F3), spawn "inkscape")
  , ((modMask, xK_F4), spawn "gimp")
  , ((modMask, xK_F6), spawn "vlc --video-on-top")
  , ((modMask, xK_F7), spawn "virtualbox")
  , ((modMask, xK_F8), spawn "thunar")
  , ((modMask, xK_F10), spawn "spotify")]

-- F-Keys
functionKeys :: XConfig Layout -> [((KeyMask, KeySym), X ())]
functionKeys conf =
  [ ((0, xK_F12), spawn "xfce4-terminal --drop-down")
  , ( (0, xK_Print)
    , spawn
        "scrot 'ArchLinux-%Y-%m-%d-%s_screenshot_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir PICTURES)'")]

-- SUPER + SHIFT + ...
superShiftKeys :: XConfig Layout -> [((KeyMask, KeySym), X ())]
superShiftKeys conf@XConfig { XMonad.modMask = modMask } =
  [ ((modMask .|. shiftMask, xK_Return), spawn "thunar")
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
  , ( (modMask .|. shiftMask, xK_d)
    , spawn
        "dmenu_run -i -nb '#191919' -nf '#ffffff' -sb '#ffffff' -sf '#191919' -fn 'NotoMonoRegular:bold:pixelsize=14'")
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp)
  , ((modMask .|. shiftMask, xK_m), windows W.focusMaster)
  , ( (modMask .|. shiftMask, xK_r)
    , spawn "xmonad --recompile && xmonad --restart")
  , ((modMask .|. shiftMask, xK_q), kill)]

-- CONTROL + SUPER + ...
controlSuperKeys :: XConfig Layout -> [((KeyMask, KeySym), X ())]
controlSuperKeys conf@XConfig { XMonad.modMask = modMask } =
  [ ((controlMask .|. modMask, xK_Up), windows W.swapUp)
  , ((controlMask .|. modMask, xK_Down), windows W.swapDown)
  , ((controlMask .|. modMask, xK_Left), sendMessage (IncMasterN 1))
  , ((controlMask .|. modMask, xK_Right), sendMessage (IncMasterN (-1)))]

-- CONTROL + ...
controlKeys :: XConfig Layout -> [((KeyMask, KeySym), X ())]
controlKeys conf = [((controlMask, xK_Print), spawn "xfce4-screenshooter")]

-- CONTROL + ALT + ...
controlAltKeys :: XConfig Layout -> [((KeyMask, KeySym), X ())]
controlAltKeys conf =
  [ ((controlMask .|. mod1Mask, xK_Next), spawn "conky-rotate -n")
  , ((controlMask .|. mod1Mask, xK_Prior), spawn "conky-rotate -p")
  , ((controlMask .|. mod1Mask, xK_a), spawn "xfce4-appfinder")
  , ((controlMask .|. mod1Mask, xK_b), spawn "thunar")
  , ((controlMask .|. mod1Mask, xK_c), spawn "catfish")
  , ((controlMask .|. mod1Mask, xK_f), spawn "firefox")
  , ((controlMask .|. mod1Mask, xK_g), spawn "brave")
  , ((controlMask .|. mod1Mask, xK_k), io exitSuccess)
  , ((controlMask .|. mod1Mask, xK_l), io exitSuccess)
  , ((controlMask .|. mod1Mask, xK_m), spawn "xfce4-settings-manager")
  , ( (controlMask .|. mod1Mask, xK_o)
    , spawn "$HOME/.xmonad/scripts/picom-toggle.sh")
  , ((controlMask .|. mod1Mask, xK_p), spawn "pamac-manager")
  , ((controlMask .|. mod1Mask, xK_r), spawn "rofi-theme-selector")
  , ((controlMask .|. mod1Mask, xK_s), spawn "spotify")
  , ((controlMask .|. mod1Mask, xK_t), spawn $ XMonad.terminal conf)
  , ((controlMask .|. mod1Mask, xK_u), spawn "pavucontrol")
  , ((controlMask .|. mod1Mask, xK_v), spawn "vivaldi-stable")
  , ((controlMask .|. mod1Mask, xK_Left), prevWS)
  , ((controlMask .|. mod1Mask, xK_Right), nextWS)]

-- CONTROL + SHIFT + ...
controlShiftKeys :: XConfig Layout -> [((KeyMask, KeySym), X ())]
controlShiftKeys conf =
  [ ((controlMask .|. shiftMask, xK_Escape), spawn "xfce4-taskmanager")
  , ((controlMask .|. shiftMask, xK_h), sendMessage Shrink)
  , ((controlMask .|. shiftMask, xK_l), sendMessage Expand)
  , ((controlMask .|. shiftMask, xK_t), withFocused $ windows . W.sink)]

-- ALT + ...
altKeys :: XConfig Layout -> [((KeyMask, KeySym), X ())]
altKeys conf =
  [ ((mod1Mask, xK_f), spawn "variety -f")
  , ((mod1Mask, xK_n), spawn "variety -n")
  , ((mod1Mask, xK_p), spawn "variety -p")
  , ((mod1Mask, xK_r), spawn "xmonad --restart")
  , ((mod1Mask, xK_t), spawn "variety -t")
  , ((mod1Mask, xK_Up), spawn "variety --pause")
  , ((mod1Mask, xK_Down), spawn "variety --resume")
  , ((mod1Mask, xK_Left), spawn "variety -p")
  , ((mod1Mask, xK_Right), spawn "variety -n")
  , ((mod1Mask, xK_F2), spawn "gmrun")
  , ((mod1Mask, xK_F3), spawn "xfce4-appfinder")]

-- MULTIMEDIA FN-KEYS
multimediaKeys :: XConfig Layout -> [((KeyMask, KeySym), X ())]
multimediaKeys conf =
  [ ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle")
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 5%-")
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+")
  , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
  , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
  , ((0, xF86XK_AudioNext), spawn "playerctl next")
  , ((0, xF86XK_AudioPrev), spawn "playerctl previous")
  , ((0, xF86XK_AudioStop), spawn "playerctl stop")]

-- WORKSPACE KEYS
workspaceKeys :: XConfig Layout -> [((KeyMask, KeySym), X ())]
workspaceKeys conf@XConfig { XMonad.modMask = modMask } =
  [((m .|. modMask, k), windows $ f i)
  | (i, k) <- zip
      (XMonad.workspaces conf)
      [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0]
  , (f, m) <- [ (W.greedyView, 0)
              , (W.shift, shiftMask)
              , (\i -> W.greedyView i . W.shift i, shiftMask)]]
  ++ [( (m .|. controlMask, key)
      , screenWorkspace sc >>= flip whenJust (windows . f))
     | (key, sc) <- zip [xK_w, xK_e] [0 ..]
     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList
  $ superKeys conf
  ++ functionKeys conf
  ++ superShiftKeys conf
  ++ controlKeys conf
  ++ controlAltKeys conf
  ++ altKeys conf
  ++ multimediaKeys conf
  ++ controlShiftKeys conf
  ++ controlSuperKeys conf
  ++ workspaceKeys conf
