module Defaults where

import XMonad

myTerminal :: String
myTerminal = "termite"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

myModMask :: KeyMask
myModMask = mod4Mask

myBrowser :: String
myBrowser = "qutebrowser"

myBrowserClass :: String
myBrowserClass = "qutebrowser"

iconFont :: String
iconFont = "FontAwesome:size=9"

defaultFont :: String
defaultFont  = "xos4 Terminus:size=8"

icoFont :: String -> String
icoFont      = (++ "^fn()") . (++) ("^fn(" ++ iconFont ++ ")")

myWorkspaces :: [String]
myWorkspaces = [ "\xf0ac" -- Globus icon
               , "\xf121" -- Code icon
               , "\xf120" -- Terminal icon
               , "4"
               , "5"
               , "6"
               , "7"
               , "8"
               , "\xf26c" -- TV icon
               ]
