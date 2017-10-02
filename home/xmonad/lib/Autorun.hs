module Autorun where

import XMonad
import XMonad.Util.SpawnOnce

autorunHook = do
    -- Uncomment after xmonad 0.14 release and change spawn to spawnOnce
    -- spawnOnce "/scripts/monitor-hotplug.sh"
    spawn "xautolock -time 10 -locker 'i3lock-next'"
    spawn "/scripts/wallaper.sh"
    spawn "/scripts/kbd.sh -d"
    spawn "/scripts/touchpad.sh -d"
    spawn "xsetroot -cursor_name left_ptr"
    spawn "compton"
    spawn "kbdd"
    spawn "dunst"
    spawnOnce "echo nothing"
    -- spawnOnce "xssproxy"

