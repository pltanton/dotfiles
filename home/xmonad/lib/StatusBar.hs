module StatusBar where

-- XMonad imports
import XMonad
import XMonad.Hooks.DynamicLog

import XMonad.Util.Run
import XMonad.Util.NamedScratchpad

-- Haskell imports
import GHC.IO.Handle.Types
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

-- My imports
import Colors

-- Xmobar
myXmobarPP :: PP
myXmobarPP = xmobarPP
    { ppCurrent         = xmobarColor myBlue myBlack
    , ppHiddenNoWindows = xmobarColor myGray myBlack
    , ppUrgent          = xmobarColor myDarkGray myDarkRed
    , ppVisible         = xmobarColor myGreen myBlack
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

-- Polybar
bg1       = "#3c3836"
bg2       = "#504945"
red       = "#fb4934"

myPolybar :: D.Client -> PP
myPolybar dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{B" ++ bg2 ++ "} ") " %{B-}"
    , ppVisible = wrap ("%{B" ++ bg1 ++ "} ") " %{B-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " : "
    , ppTitle = shorten 40
    }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"
