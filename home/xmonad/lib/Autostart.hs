module Autostart (hook) where

import XMonad.Actions.SpawnOn
import XMonad

hook = do
    spawnOn "NSP" "telegram-desktop"
    {-spawn "compton -c"-}
    {-spawn "xsetroot -cursor_name left_ptr"-}
    {-spawn "hsetroot -solid '#202020'"-}
    {-spawn "/scripts/wallaper.sh"-}
    {-spawn "xautolock -time 10 -locker 'lock.sh'"-}
    {-spawn "udiskie --no-tray --use-udisks2 --quiet --no-file-manager"-}
    {-spawn "twmnd"-}
    {-spawn "unclutter -grab"-}
    {-spawn "/scripts/monitor-hotplug.sh"-}

