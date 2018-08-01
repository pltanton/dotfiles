module Keys (myKeys, myMouseBindings) where

-- Xmonad imports
import XMonad

import XMonad.Hooks.ManageDocks

import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare

import XMonad.Actions.WindowGo
import XMonad.Actions.Navigation2D
import XMonad.Actions.GridSelect
import XMonad.Actions.FloatKeys
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow

import XMonad.Prompt
import XMonad.Prompt.Pass
import XMonad.Prompt.RunOrRaise

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex

-- My imports
import Defaults
import Scratchpads

-- Keyboard bindings
myKeys :: [(String, X ())]
myKeys =
    [ ("M-f",      sendMessage $ Toggle NBFULL)
    , ("M-m",      sendMessage ToggleStruts)
    , ("M1-<F4>",  kill)
    , ("M-r",      spawn "xmonad --recompile && xmonad --restart")
    , ("M-<F12>",  spawn "loginctl lock-session")
    , ("M-<F11>",  spawn "autorandr --change && systemctl --user restart random-background")

    -- Navigation
    , ("M-h",        moveTo Prev $ WSIs notSP)
    , ("M-l",        moveTo Next $ WSIs notSP)
    , ("M-j",        prevEmptyWS)
    , ("M-k",        nextEmptyWS)
    , ("M-p",        prevNonEmptyWS)
    , ("M-n",        nextNonEmptyWS)
    , ("M-S-h",      shiftTo Prev (WSIs notSP) >> moveTo Prev (WSIs notSP))
    , ("M-S-l",      shiftTo Next (WSIs notSP) >> moveTo Next (WSIs notSP))

    , ("M-S-<Left>",   withFocused (keysResizeWindow (-10, 0) (1/2, 1/2)))
    , ("M-S-<Right>",  withFocused (keysResizeWindow (10, 0) (1/2, 1/2)))
    , ("M-S-<Up>",     withFocused (keysResizeWindow (0, 10) (1/2, 1/2)))
    , ("M-S-<Down>",   withFocused (keysResizeWindow (0, -10) (1/2, 1/2)))
    , ("M-<Left>",     withFocused (keysMoveWindow (-15, 0)))
    , ("M-<Right>",    withFocused (keysMoveWindow (15, 0)))
    , ("M-<Up>",       withFocused (keysMoveWindow (0, -15)))
    , ("M-<Down>",     withFocused (keysMoveWindow (0, 15)))

    , ("M1-<Tab>",    windows W.focusDown >> windows W.shiftMaster)
    , ("M1-S-<Tab>",  windows W.focusUp >> windows W.shiftMaster)
    , ("M1-j",        windows W.focusDown)
    , ("M1-k",        windows W.focusUp)
    , ("M1-h",        windowGo L False)
    , ("M1-l",        windowGo R False)
    , ("M1-f",        switchLayer)
    , ("M-v",         windows copyToAll)
    , ("M-S-v",       killAllOtherCopies)

    , ("M1-S-j",  windowSwap D False)
    , ("M1-S-k",  windowSwap U False)
    , ("M1-S-h",  windowSwap L False)
    , ("M1-S-l",  windowSwap R False)

    , ("M1-<Left>",   sendMessage Shrink)
    , ("M1-<Right>",  sendMessage Expand)
    , ("M1-<Down>",   sendMessage MirrorShrink)
    , ("M1-<Up>",     sendMessage MirrorExpand)

    , ("M-g", goToSelected myGSConfig)

    -- Run applications
    , ("<F12>",             spawn "rofi-pass")
    , ("M-a",               runOrRaise myBrowser (className =? myBrowserClass))
    , ("<XF86HomePage>",    runOrRaise myBrowser (className =? myBrowserClass))
    , ("M-e",               spawn "spacefm")
    , ("M-u",               spawn $ myTerminal ++ " -e ranger")
    , ("<XF86MyComputer>",  spawn $ myTerminal ++ " -e ranger")
    , ("M-o",               spawn $ myTerminal ++ " -e nvim")
    , ("M-'",               namedScratchpadAction myScratchPads "terminal")
    , ("M-q",               namedScratchpadAction myScratchPads "evolution")
    , ("M-<Tab>",           namedScratchpadAction myScratchPads "telegram")

    -- MPD
    , ("C-M1-<Up>",     spawn "mpc toggle")
    , ("C-M1-<Down>",   spawn "mpc stop")
    , ("C-M1-<Left>",   spawn "mpc prev")
    , ("C-M1-<Right>",  spawn "mpc next")

    -- Prompts
    --, ("<F12>",         passPrompt myXPConfig)
    --, ("M-<Return>",    runOrRaisePrompt myXPConfig)
    , ("M-<Return>",    spawn "rofi -columns 2 -show-icons -show drun -modi drun")
    , ("M-C-<Return>",  spawn "rofi -show run")
    

    -- Media keys
    , ("<Print>",                  spawn "~/.xmonad/scripts/screenshot.sh")
    , ("M-<Print>",                spawn "~/.xmonad/scripts/screenshot.sh -e")
    , ("<XF86MonBrightnessUp>",    spawn "xbacklight -inc 10")
    , ("<XF86MonBrightnessDown>",  spawn "xbacklight -dec 10")
    , ("<XF86AudioRaiseVolume>",   spawn "pamixer -i 2")
    , ("<XF86AudioLowerVolume>",   spawn "pamixer -d  2")
    , ("<XF86AudioMute>",          spawn "pamixer -t")
    , ("<XF86Explorer>",           spawn "~/.xmonad/scripts/touchpad.sh -t")
    , ("<XF86LaunchA>",            spawn "~/.xmonad/scripts/kbd.sh -t")
    ]

    ++

    [ (m ++ [k], windows $ f i)
    | (i, k) <- zip myWorkspaces "&[{}(=*)+"
    , (f, m) <- [(W.greedyView, "M-"), (W.shift, "M-S-")]
    ]

    ++

    [ (m ++ [key], screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip ";,." [0..]
    , (f, m) <- [(W.view, "M-"), (W.shift, "M-S-")]
    ]
  where
    notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
    nextNonEmptyWS =
        findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
        >>= \t -> windows . W.view $ t
    prevNonEmptyWS =
        findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
        >>= \t -> windows . W.view $ t
    nextEmptyWS =
        findWorkspace getSortByIndexNoSP Next HiddenEmptyWS 1
        >>= \t -> windows . W.view $ t
    prevEmptyWS =
        findWorkspace getSortByIndexNoSP Prev HiddenEmptyWS 1
        >>= \t -> windows . W.view $ t
    getSortByIndexNoSP =
        fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex

-- Mouse bindings
myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} =
    M.fromList
        [ ((modm, button1), \w -> focus w
                            >> mouseMoveWindow w
                            >> windows W.shiftMaster)
        , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
        , ((modm, button3), \w -> focus w >> Flex.mouseResizeWindow w)
        ]


myGSConfig = defaultGSConfig { gs_cellwidth = 250
                             , gs_cellpadding = 5
                             }

