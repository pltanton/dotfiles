-- -*- mode:haskell -*-
module Main where

import System.Taffybar
import System.Taffybar.Hooks
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Widget.Generic.PollingLabel
import System.Taffybar.Widget.Util
import System.Taffybar.Widget.Workspaces
import System.Taffybar.Widget.SNITray
import System.Taffybar.Widget.XDGMenu.MenuWidget


main = do
  let myWorkspacesConfig =
        defaultWorkspacesConfig
        { minIcons = 1
        , widgetGap = 0
        , showWorkspaceFn = \w -> 
            foldr (&&) True $ map ($ w) [
              hideEmpty, 
              ("NSP" /=) . workspaceName
            ]
        }
      workspaces = workspacesNew myWorkspacesConfig
      clock = textClockNew Nothing "%a %b %_d %X" 1
      layout = layoutNew defaultLayoutConfig
      windows = windowsNew defaultWindowsConfig
          -- See https://github.com/taffybar/gtk-sni-tray#statusnotifierwatcher
          -- for a better way to set up the sni tray
      tray = sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt
      myConfig = defaultSimpleTaffyConfig
        { startWidgets = 
            workspaces : map (>>= buildContentsBox) [ layout, windows ]
        , endWidgets = map (>>= buildContentsBox)
          [ batteryIconNew
          , clock
          , tray
          ]
        , barPosition = Top
        , barPadding = 8 
        , barHeight = 20
        , widgetSpacing = 0
        }
  dyreTaffybar $ withBatteryRefresh $ withLogServer $ withToggleServer $
               toTaffyConfig myConfig
