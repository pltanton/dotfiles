module Manage where
-- XMonad imports
import XMonad

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Fullscreen

import XMonad.Util.NamedScratchpad

import XMonad.Actions.UpdatePointer

-- My imports
import Defaults
import Scratchpads
import Data.List

myManageHook :: ManageHook
myManageHook = composeAll
    [ manageDocks
    , fullscreenManageHook
    , namedScratchpadManageHook myScratchPads
    , isFullscreen                        --> doFullFloat
    , className =? myBrowser              --> doShift (head myWorkspaces)
    , className =? "Plugin-container"     --> doFloat
    , className =? "gnuplot_qt"           --> doFloat
    , className =? "Octave-gui"           --> doFloat
    , className =? "TelegramDesktop" <&&> (fmap (/= "Media viewer") title)  --> doFloat <+> doShift "NSP"
    --, className =? "TelegramDesktop" <&&> (fmap (== "Media viewer") title)  --> doFloat
    , className =? "mpv"                  --> doFloat
    , isDialog                            --> doCenterFloat
    , resource  =? "desktop_window"       --> doIgnore
    , className =? "steam"                --> doShift (myWorkspaces !! 6)
    , isFullscreen                        --> doFullFloat
    , className =? "Nm-connection-editor" --> doCenterFloat
    , className =? "Pavucontrol"          --> doCenterFloat
    ]
