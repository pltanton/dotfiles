import XMonad
import XMonad.Config.Desktop

import XMonad.Hooks.SetWMName                   -- to fix java's grey windows
import XMonad.Hooks.EwmhDesktops                -- to automaticly expand fullscreen apps
import XMonad.Layout.Fullscreen                 -- to manualy expend app to fullscreen
import XMonad.Layout.Named
import XMonad.Util.EZConfig                     -- ez shortcuts
import XMonad.Hooks.DynamicLog                  -- for bar
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CycleWS

import XMonad.Layout.ResizableTile
import XMonad.Layout.HintedGrid
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders

import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.ManageHelpers

import XMonad.Actions.WindowGo
import XMonad.Actions.Navigation2D

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
{-myBrowser       = "firefox"-}
{-myBrowserClass  = "firefox"-}
myBrowser       = "qutebrowser-we"
myBrowserClass  = "qutebrowser"

iconFont     = "FontAwesome:size=9"
defaultFont  = "xos4 Terminus:size=8"
icoFont      = (++ ("^fn()")) . (++) ("^fn(" ++ iconFont ++ ")")
myWorkspaces = [ "\xf0ac", "\xf121", "\xf120"
               , "4", "5", "6", "7", "8", "\xf26c" ]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
------------------------------------------------------------------------
myKeys = [ ("M-f",                      sendMessage $ Toggle NBFULL)
         , ("M-m",                      sendMessage ToggleStruts)
         , ("M1-<F4>",                  kill)
         , ("M-r",                      spawn "/scripts/xmonad-restart.sh")
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
         , ("M-a",                      runOrRaise myBrowser (className =? myBrowserClass))
         , ("<XF86HomePage>",           runOrRaise myBrowser (className =? myBrowserClass))
         , ("M-e",                      spawn "spacefm")
         , ("M-u",                      spawn $ myTerminal ++ " -e ranger")
         , ("<XF86MyComputer>",         spawn $ myTerminal ++ " -e ranger")
         , ("M-o",                      spawn $ myTerminal ++ " -e nvim")
         , ("M-'",                      namedScratchpadAction myScratchPads "terminal")
         , ("M-q",                      namedScratchpadAction myScratchPads "torrent")
         , ("M-<Tab>",                  namedScratchpadAction myScratchPads "telegram")

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
         , ("<XF86AudioRaiseVolume>",   spawn "pamixer -i 2")
         , ("<XF86AudioLowerVolume>",   spawn "pamixer -d  2")
         , ("<XF86AudioMute>",          spawn "pamixer -t")
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
layoutIcon name = "^i(/home/anton/.xmonad/icons/" ++ name ++ ".xpm)"
myLayout
    = smartBorders
    $ named "T" tiled |||
      named "M" (Mirror tiled) |||
      named "G" grid |||
      named "F" full
    where
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

------------------------------------------------------------------------
-- Event handling
------------------------------------------------------------------------
myEventHook = docksEventHook <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook

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

-- Dzen2
wrapFillSquare = wrap "^p(_TOP)^r(4x4)^p(-6)^p()^ib(1)" "^ib(0)"
wrapEmptySquare = wrap "^p(_TOP)^ro(4x4)^p(-6)^p()^ib(1)" "^ib(0)"
dzenLogHook h = dynamicLogWithPP $ dzenPP
    {
        ppCurrent         = dzenColor myBlue myBlack . wrapFillSquare . pad
      , ppVisible         = dzenColor myLighterstGray myBlack . wrapEmptySquare . pad
      , ppHidden          = dzenColor myLighterstGray myBlack . wrapEmptySquare . pad
      , ppHiddenNoWindows = dzenColor myGray myBlack . pad
      , ppUrgent          = dzenColor myDarkRed myBlack
      , ppSep             = ":"
      , ppOrder           = \(ws:l:t:_) -> [" " ++ l,ws,t]
      , ppLayout          = dzenColor myLighterstGray myBlack . pad
      , ppTitle           = dzenColor myLighterstGray myBlack . shorten 140 . dzenEscape
      , ppOutput          = hPutStrLn h
      , ppSort            = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
    }

-- Lemonbar
lemonbarFG color str = "%{F" ++ color ++ "}" ++ str ++ "%{F-}"
lemonbarBG color str = "%{B" ++ color ++ "}" ++ str ++ "%{B-}"
lemonbarColor color  = lemonbarFG color . lemonbarBG color
lemonbarU str        = "%{u}" ++ str ++ "%{u-}"

lemonbarLogHook h = dynamicLogWithPP $ def
    { ppCurrent         = lemonbarU . lemonbarFG myBlue
    , ppVisible         = lemonbarFG myYellow
    , ppHidden          = lemonbarFG myLighterstGray
    , ppHiddenNoWindows = lemonbarFG myGray
    , ppTitle           = shorten 140

    , ppOutput          = hPutStrLn h
    , ppSort            = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
    , ppOrder           = \(ws:l:t:_) -> [" " ++ l,ws,t]
    }

myLemonBar = "yags ~/.config/yags/lemonbar.yml | LC_ALL=ru_RU.UTF-8 lemonbar -d " ++
    " -B '" ++ myBlack ++ "' -F '" ++ myLighterstGray ++ "'" ++
    " -f 'xos4 Terminus:size=9' -f 'FontAwesome:size=10'" ++
    " -g $(/scripts/geometry-for-lemonbar.sh)" ++
    {-" -g 1358x18+4+0" ++-}
    " -u 3"

------------------------------------------------------------------------
-- Scratchpads
------------------------------------------------------------------------
myScratchPads = [ NS "terminal" "termite --name 'scratchpad' -e 'sh -c \"tmux a -t scratchpad || tmux new -s scratchpad\"'" (resource =? "scratchpad") floatingTerm
                , NS "torrent" "termite --name 'torrent' -e 'sh -c \"tmux -L rt attach -t rt\"'" (resource =? "torrent") floatingTerm
                , NS "telegram" "telegram-desktop" (resource =? "telegram-desktop") floatingRight
                --, NS "torrent" "transmission-gtk" (stringProperty "WM_NAME" =? "Transmission") floatingRt
                ]
                where
                    floatingTerm = customFloating $ W.RationalRect l t w h where
                        h = 0.4
                        w = 1
                        t = 1 - h
                        l = 1 - w
                    floatingRight = customFloating $ W.RationalRect l t w h where
                        h = 1
                        w = 0.3
                        t = 1 - h
                        l = 1 - w

------------------------------------------------------------------------
-- Navigation 2D config
------------------------------------------------------------------------
myNavigation2DConfig = def {
    defaultTiledNavigation = centerNavigation
}

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
------------------------------------------------------------------------

main = do
    status <- spawnPipe myLemonBar
    xmonad $ withNavigation2DConfig myNavigation2DConfig
           $ defaults status

defaults xmproc = desktopConfig
    { terminal            = myTerminal
      ,focusFollowsMouse  = myFocusFollowsMouse
      ,clickJustFocuses   = myClickJustFocuses
      ,borderWidth        = myBorderWidth
      ,modMask            = myModMask
      ,workspaces         = myWorkspaces
      ,normalBorderColor  = myDarkestGray
      ,focusedBorderColor = myGray
      ,mouseBindings      = myMouseBindings
      ,layoutHook         = mkToggle (single NBFULL) $ avoidStruts myLayout
      ,manageHook         = myManageHook <+> manageHook desktopConfig
      ,handleEventHook    = myEventHook
      ,logHook            = lemonbarLogHook xmproc
      ,startupHook        = docksStartupHook <+> setWMName "LG3D"
      --,keys               = myKeys
    } `additionalKeysP` myKeys

