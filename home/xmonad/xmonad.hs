-- XMonad imports
import XMonad
import XMonad.Config.Desktop

import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import XMonad.Util.Run
import XMonad.Util.EZConfig

import XMonad.Actions.Navigation2D

-- Haskell imports
import Data.Monoid

-- My imports
import Defaults
import Keys
import Layouts
import Manage
import StatusBar
import Colors

myEventHook :: Event -> X Data.Monoid.All
myEventHook = docksEventHook <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook

-- Run xmonad with all the defaults we set up.
main :: IO ()
main = xmonad =<< statusBar myXmobar myXmobarPP toggleStrutsKey myConfig
  where
    myNavigation2DConfig = def {
        defaultTiledNavigation = centerNavigation
    }

    toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

    myConfig = desktopConfig
        { terminal           = myTerminal
        , focusFollowsMouse  = myFocusFollowsMouse
        , clickJustFocuses   = myClickJustFocuses
        , borderWidth        = 2
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myDarkestGray
        , focusedBorderColor = myGray
        , mouseBindings      = myMouseBindings
        , layoutHook         = mkToggle (single NBFULL) $ avoidStruts myLayout
        , manageHook         = myManageHook <+> manageHook desktopConfig
        , handleEventHook    = myEventHook
        , startupHook        = docksStartupHook <+> setWMName "LG3D"
        } `additionalKeysP` myKeys

