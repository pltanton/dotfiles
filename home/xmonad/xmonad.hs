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
import XMonad.Actions.UpdatePointer

import System.Taffybar.Support.PagerHints (pagerHints)


-- Haskell imports
import Data.Monoid
import Data.Text.Encoding
import Data.Text

-- My imports
import Defaults
import Keys
import Layouts
import Manage
import StatusBar
import Colors

import Data.Maybe


myEventHook :: Event -> X Data.Monoid.All
myEventHook = docksEventHook <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook


-- Run xmonad with all the defaults we set up.
main :: IO ()
main = xmonad $ pagerHints $ myConfig
    myNavigation2DConfig = def 
        { defaultTiledNavigation = hybridNavigation
        }

    toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

    myConfig = withNavigation2DConfig myNavigation2DConfig $ desktopConfig
        { terminal           = myTerminal
        , logHook            = updatePointer (0.5, 0.5) (0, 0)
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
        , startupHook        = setWMName "LG3D"
        } `additionalKeysP` myKeys

