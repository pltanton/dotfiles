module Autorun where

import XMonad
import XMonad.Util.SpawnOnce

autorunHook = do
    -- Uncomment after xmonad 0.14 release and change spawn to spawnOnce
    -- spawnOnce "/scripts/monitor-hotplug.sh"
    spawn "feh --bg-fill /home/anton/.xmonad/wall.png"
    spawn "/scripts/kbd.sh -d"
    spawn "/scripts/touchpad.sh -d"
    spawn "xsetroot -cursor_name left_ptr"
    spawn "compton"
    spawn "telegram-desktop"
    spawnOnce "kbdd"
    spawnOnce "dunst"
    spawnOnce "xbanish"

