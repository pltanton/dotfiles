module StatusBar where

-- XMonad imports
import XMonad
import XMonad.Hooks.DynamicLog

import XMonad.Util.Run
import XMonad.Util.NamedScratchpad

-- Haskell imports
import GHC.IO.Handle.Types

-- My imports
import Colors

-- Xmobar
xmobarLogHook :: Handle -> X ()
xmobarLogHook xmproc = dynamicLogWithPP $ xmobarPP
    { ppOutput          = hPutStrLn xmproc
    , ppCurrent         = xmobarColor myBlue myBlack
    , ppHiddenNoWindows = xmobarColor myDarkGray myBlack
    , ppUrgent          = xmobarColor myDarkGray myDarkRed
    , ppVisible         = xmobarColor "#90a959" "#151515"
    , ppSep             = " : "
    , ppLayout          = xmobarColor myLighterGray ""
    , ppOrder           = \(ws:l:t:_) -> [" " ++ l,ws,t]
    , ppTitle           = xmobarColor myLighterGray "" . shorten 140
    , ppSort            = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
    }

myXmobar :: String
myXmobar = "LC_ALL=ru_RU.UTF-8 xmobar"

-- Lemonbar
lemonbarLogHook :: GHC.IO.Handle.Types.Handle -> X ()
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
  where
    lemonbarFG color str = "%{F" ++ color ++ "}" ++ str ++ "%{F-}"
    lemonbarU str        = "%{u}" ++ str ++ "%{u-}"

myLemonBar :: String
myLemonBar =
    "yags ~/.config/yags/lemonbar.yml | LC_ALL=ru_RU.UTF-8 lemonbar -d " ++
    " -B '" ++ myBlack ++ "' -F '" ++ myLighterstGray ++ "'" ++
    " -f 'xos4 Terminus:size=9' -f 'FontAwesome:size=10'" ++
    " -g $(/scripts/geometry-for-lemonbar.sh)" ++
    " -u 3"

