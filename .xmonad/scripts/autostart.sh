#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

#change your keyboard if you need it
#setxkbmap -layout be

#cursor active at boot
xsetroot -cursor_name left_ptr &

#start the conky to learn the shortcuts
#(conky -c $HOME/.xmonad/scripts/system-overview) &

#msi steelseries keyboard backlight
$HOME/.xmonad/scripts/kbd-color.sh

#starting utility applications at boot time
run nm-applet &
run xfce4-power-manager &
picom --config $HOME/.xmonad/scripts/picom.conf &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
/usr/lib/xfce4/notifyd/xfce4-notifyd &
trayer --edge top --align right --width 10 --height 30 --expand true --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x2F343F --monitor 1 --padding 1 &


#starting user applications at boot time
variety &
