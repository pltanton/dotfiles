module Manage where
-- XMonad imports
import XMonad

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Fullscreen

import XMonad.Util.NamedScratchpad

-- My imports
import Defaults
import Scratchpads

myManageHook :: ManageHook
myManageHook = composeAll
    [ manageDocks
    , fullscreenManageHook
    , namedScratchpadManageHook myScratchPads
    , isFullscreen                      --> doFullFloat
    , className =? myBrowser            --> doShift (head myWorkspaces)
    , className =? "Plugin-container"   --> doFloat
    , className =? "gnuplot_qt"         --> doFloat
    , className =? "Octave-gui"         --> doFloat
    , className =? "NeercGame"          --> doFloat
    , className =? "TelegramDesktop"    --> doFloat <+> doShift "NSP"
    , className =? "mpv"                --> doFloat
    , isDialog                          --> doCenterFloat
    , resource  =? "desktop_window"     --> doIgnore
    , className =? "steam"              --> doShift (myWorkspaces !! 6)
    , isFullscreen                      --> doFullFloat]
