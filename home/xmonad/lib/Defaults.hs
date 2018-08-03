module Defaults where

import XMonad
import XMonad.Prompt

import Data.List (isInfixOf)

import Colors

myTerminal :: String
myTerminal = "kitty"

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
defaultFont  = "xft:xos4 Terminus:size=12"

icoFont :: String -> String
icoFont      = (++ "^fn()") . (++) ("^fn(" ++ iconFont ++ ")")

myWorkspaces :: [String]
myWorkspaces = map show [1..9]
myWorkspacesOld = [ "\xf0ac" -- Globus icon
               , "\xf121" -- Code icon
               , "\xf120" -- Terminal icon
               , "4"
               , "5"
               , "6"
               , "7"
               , "8"
               , "\xf26c" -- TV icon
               ]

myXPConfig :: XPConfig
myXPConfig = def { font = "xft:Terminus:size=15:autohint=true"
                 , searchPredicate = isInfixOf
                 , bgColor = myBlack
                 , fgColor = myWhite
                 , bgHLight = myGray
                 , fgHLight = myWhite
                 , height = 29
                 , borderColor = myDarkestGray
                 , promptBorderWidth = 5
                 }
