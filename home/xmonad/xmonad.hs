import XMonad
import Data.Monoid

import XMonad.Hooks.SetWMName                   -- to fix java's grey windows
import XMonad.Hooks.EwmhDesktops                -- to automaticly expand fullscreen apps
import XMonad.Layout.Fullscreen                 -- to manualy expend app to fullscreen
import XMonad.Layout.Named
import XMonad.Util.EZConfig                     -- ez shortcuts
import XMonad.Hooks.DynamicLog                  -- for bar
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CycleWS

import XMonad.Layout.IM                         -- layout for pidgin
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.HintedGrid
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.SimplestFloat

import XMonad.Actions.FloatKeys                 -- move windows by keyboard

import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition

import XMonad.Actions.WindowGo
import XMonad.Actions.Navigation2D

import Data.Ratio ((%))
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Actions.FlexibleResize as Flex

myTerminal = "termite"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 2
myModMask       = mod4Mask
myBrowser       = "firefox"

myWorkspaces = clickable ["\xf0ac","\xf044","\xf120","4","5","6","7","\xf26c","\xf086"]
    where clickable l = ["<action=`xdotool key super+" ++ i ++ "`>" ++ ws ++ "</action>" | (i,ws) <- zip keymap l ]
          keymap = ["U26", "U5B", "U7B", "U7D", "U28", "U3D", "U2A", "U29", "U2B"]


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
------------------------------------------------------------------------
myKeys = [ ("M-f",                      sendMessage $ Toggle NBFULL)
         , ("M-m",                      sendMessage ToggleStruts)
         , ("M1-<F4>",                  kill)
         , ("M-r",                      spawn "pkill -KILL xmobar || xmonad --recompile && xmonad --restart")
         , ("M-<F12>",                  spawn "xautolock -locknow")
         , ("M-<F11>",                  spawn "/scripts/monitor-hotplug.rb")
         ---------------------------------------------------------------
         -- Navigation
         ---------------------------------------------------------------
         , ("M-h",                      moveTo Prev $ WSIs notSP)
         , ("M-l",                      moveTo Next $ WSIs notSP)
         , ("M-<Left>",                 moveTo Prev $ WSIs notSP)
         , ("M-<Right>",                moveTo Next $ WSIs notSP)
         , ("M-p",                      prevNonEmptyWS)
         , ("M-n",                      nextNonEmptyWS)
         , ("M-S-h",                    shiftTo Prev (WSIs notSP) >> moveTo Prev (WSIs notSP))
         , ("M-S-l",                    shiftTo Next (WSIs notSP) >> moveTo Next (WSIs notSP))

         , ("M1-<Tab>",                 windows W.focusDown >> windows W.shiftMaster)
         , ("M1-S-<Tab>",               windows W.focusUp >> windows W.shiftMaster)
         , ("M1-j",                     windowGo D False)
         , ("M1-k",                     windowGo U False)
         , ("M1-h",                     windowGo L False)
         , ("M1-l",                     windowGo R False)

         , ("M1-S-j",                   windowSwap D False)
         , ("M1-S-k",                   windowSwap U False)
         , ("M1-S-h",                   windowSwap L False)
         , ("M1-S-l",                   windowSwap R False)

         , ("M-C-h",                    sendMessage Shrink)
         , ("M-C-l",                    sendMessage Expand)
         , ("M-C-k",                    sendMessage MirrorShrink)
         , ("M-C-j",                    sendMessage MirrorExpand)

         ---------------------------------------------------------------
         -- Run applications
         ---------------------------------------------------------------
         , ("M-<Return>",               spawn "dmenu.sh")
         , ("M-a",                      runOrRaise myBrowser (className =? myBrowser))
         , ("<XF86HomePage>",           runOrRaise myBrowser (className =? myBrowser))
         , ("M-e",                      spawn "spacefm")
         , ("M-u",                      spawn $ myTerminal ++ " -e ranger")
         , ("<XF86MyComputer>",         spawn $ myTerminal ++ " -e ranger")
         , ("M-o",                      spawn $ myTerminal ++ " -e nvim")
         , ("M-'",                      namedScratchpadAction myScratchPads "terminal")
         , ("M-q",                      namedScratchpadAction myScratchPads "torrent")

         ---------------------------------------------------------------
         -- MPD
         ---------------------------------------------------------------
         , ("C-M1-<Up>",                spawn "mpc toggle")
         , ("C-M1-<Down>",              spawn "mpc stop")
         , ("C-M1-<Left>",              spawn "mpc prev")
         , ("C-M1-<Right>",             spawn "mpc next")

         ---------------------------------------------------------------
         -- Media keys
         ---------------------------------------------------------------
         , ("<Print>",                  spawn "/scripts/screenshot.sh")
         , ("M-<Print>",                spawn "/scripts/screenshot.sh -e")
         , ("<XF86MonBrightnessUp>",    spawn "light -A 10")
         , ("<XF86MonBrightnessDown>",  spawn "light -U 10")
         , ("<XF86AudioRaiseVolume>",   spawn "pactl set-sink-mute 1 false ; pactl set-sink-volume 1 +2%")
         , ("<XF86AudioLowerVolume>",   spawn "pactl set-sink-mute 1 false ; pactl set-sink-volume 1 -2%")
         , ("<XF86AudioMute>",          spawn "pactl set-sink-mute 1 toggle")
         , ("<XF86Explorer>",           spawn "touchpad.sh -t")
         , ("<XF86LaunchA>",            spawn "kbd.sh -t")

         ] ++
         [ (m ++ [k], windows $ f i)
         | (i, k) <- zip myWorkspaces "&[{}(=*)+"
         , (f, m) <- [(W.greedyView, "M-"), (W.shift, "M-S-")]
         ] ++
         [ (m ++ [key], screenWorkspace sc >>= flip whenJust (windows . f))
         | (key, sc) <- zip ";,." [0..]
         , (f, m) <- [(W.view, "M-"), (W.shift, "M-S-")]]
         where
                notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
                nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
                    >>= \t -> windows . W.view $ t
                prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
                    >>= \t -> windows . W.view $ t
                getSortByIndexNoSP =
                    fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    , ((modm, button3), \w -> focus w >> Flex.mouseResizeWindow w)
    ]

------------------------------------------------------------------------
-- Layouts
------------------------------------------------------------------------
myLayout
    = smartBorders
    -- $ mkToggle (single NBFULL)
    $ onWorkspace (myWorkspaces !! 8) pidginLayot
    $ named "<icon=tiled.xpm/>" tiled |||
      named "<icon=mirror-tiled.xpm/>" (Mirror tiled) |||
      named "<icon=grid.xpm/>" grid |||
      named "<icon=full.xpm/>" full
    where
        pidginLayot = named "<icon=im.xpm/>" $ spaces $ withIM (1%7) (Role "buddy_list") $ GridRatio (3/4) True
        tiled       = spaces $ ResizableTall nmaster delta ratio []
        grid        = spaces $ Grid False
        spaces      = spacing 4
        full        = spaces Full
        nmaster     = 1
        ratio       = 1/2
        delta       = 3/100

------------------------------------------------------------------------
-- Window rules
------------------------------------------------------------------------
myManageHook = composeAll
    [ manageDocks
    , namedScratchpadManageHook myScratchPads
    , className =? myBrowser            --> doShift (head myWorkspaces)
    , className =? "Plugin-container"   --> doFloat
    , className =? "gnuplot_qt"         --> doFloat
    , className =? "Octave-gui"         --> doFloat
    , className =? "NeercGame"          --> doFloat
    , className =? "Pidgin"             --> doShift (myWorkspaces !! 8)
    , className =? "mpv"                --> doFloat
    , isDialog                          --> doFloat
    , resource  =? "desktop_window"     --> doIgnore
    , className =? "steam"              --> doShift (myWorkspaces !! 6)
    ]
    <+>
    composeOne [isFullscreen -?> doFullFloat]

------------------------------------------------------------------------
-- Event handling
------------------------------------------------------------------------
myEventHook = docksEventHook

------------------------------------------------------------------------
-- Colors
------------------------------------------------------------------------
myBlack         = "#1d1f21"
myDarkestGray   = "#282a2e"
myDarkGray      = "#373b41"
myGray          = "#969896"
myLightGray     = "#b4b7b4"
myLighterGray   = "#c5c8c6"
myLighterstGray = "#e0e0e0"
myWhite         = "#ffffff"
myRed           = "#cc6666"
myOrange        = "#de935f"
myYellow        = "#f0c674"
myGreen         = "#b5bd68"
myLemon         = "#8abeb7"
myBlue          = "#81a2be"
myPurple        = "#b294bb"
myDarkRed       = "#a3685a"

------------------------------------------------------------------------
-- Status bars and logging
------------------------------------------------------------------------
myLogHook xmproc = dynamicLogWithPP $ xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppCurrent = xmobarColor myBlue myBlack
                   , ppHiddenNoWindows = xmobarColor myDarkGray myBlack
                   , ppUrgent = xmobarColor myDarkGray myDarkRed
                   , ppVisible = xmobarColor "#90a959" "#151515"
                   , ppSep = " : "
                   , ppLayout = xmobarColor myLighterGray ""
                   , ppOrder = \(ws:l:t:_) -> [" " ++ l,ws,t]
                   , ppTitle = xmobarColor myLighterGray "" . shorten 140
                   , ppSort = (. namedScratchpadFilterOutWorkspace) <$> ppSort defaultPP
                   }

------------------------------------------------------------------------
-- Scratchpads
------------------------------------------------------------------------
myScratchPads = [ NS "terminal" "urxvtc -name 'scratchpad' -e bash -c 'tmux a -t scratchpad || tmux new -s scratchpad'" (resource =? "scratchpad") floatingTerm
                , NS "torrent" "urxvtc -name 'torrent' -e bash -c 'transmission-remote-cli'" (resource =? "torrent") floatingTerm
                --, NS "torrent" "transmission-gtk" (stringProperty "WM_NAME" =? "Transmission") floatingRt
                ]
                where
                    floatingTerm = customFloating $ W.RationalRect l t w h where
                        h = 0.4
                        w = 1
                        t = 1 - h
                        l = 1 - w
                    floatingRt = customFloating $ W.RationalRect l t w h where
                        h = 1
                        w = 0.3
                        t = 1 - h
                        l = 0
------------------------------------------------------------------------
-- Startup hook
------------------------------------------------------------------------
myStartupHook = setWMName "LG3D"

------------------------------------------------------------------------
-- Navigation 2D config
------------------------------------------------------------------------
myNavigation2DConfig = defaultNavigation2DConfig {
    defaultTiledNavigation = centerNavigation
}

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
------------------------------------------------------------------------

main = do
    xmproc <- spawnPipe "LC_ALL=ru_RU.UTF-8 .cabal/bin/xmobar 2> ~/err"
    xmonad $ withNavigation2DConfig myNavigation2DConfig
           $ defaults xmproc

defaults xmproc = defaultConfig
    { terminal            = myTerminal
      ,focusFollowsMouse  = myFocusFollowsMouse
      ,clickJustFocuses   = myClickJustFocuses
      ,borderWidth        = myBorderWidth
      ,modMask            = myModMask
      ,workspaces         = myWorkspaces
      ,normalBorderColor  = myDarkGray
      ,focusedBorderColor = myGray
      ,mouseBindings      = myMouseBindings
      ,layoutHook         = mkToggle (single NBFULL) $ avoidStruts myLayout
      ,manageHook         = fullscreenManageHook <+> myManageHook
      ,handleEventHook    = XMonad.Hooks.EwmhDesktops.fullscreenEventHook <+> myEventHook
      ,logHook            = myLogHook xmproc
      ,startupHook        = myStartupHook
      --,keys               = myKeys
    } `additionalKeysP` myKeys

