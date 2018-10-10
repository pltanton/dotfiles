-- -*- mode:haskell -*-
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
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
import System.Taffybar.Widget.CommandRunner
import System.Taffybar.Util

import System.IO


import Control.Exception.Base
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader


cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

myGraphConfig =
  defaultGraphConfig
  { graphPadding = 0
  , graphBorderWidth = 0
  , graphWidth = 75
  , graphBackgroundColor = (0.0, 0.0, 0.0, 0.0)
}

cpuCfg = myGraphConfig
  { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)]
  , graphLabel = Just "cpu "
  }

battery = commandRunnerNew 3 "/bin/sh" ["/home/anton/.config/taffybar/bat.sh" ] "N/A"

main = do
  let myWorkspacesConfig =
        defaultWorkspacesConfig
        { minIcons = 1
        , getWindowIconPixbuf = myIcons
        , widgetGap = 0
        , showWorkspaceFn = \w -> 
            foldr (&&) True $ map ($ w) [
              hideEmpty, 
              ("NSP" /=) . workspaceName
            ]
        --, getWindowIconPixbuf = scaledWindowIconPixbufGetter getWindowIconPixbufFromEWMH
        }
      myIcons = scaledWindowIconPixbufGetter $
                unscaledDefaultGetWindowIconPixbuf <|||>
                (\size _ -> lift $ loadPixbufByName size "utilities-terminal")

      workspaces = workspacesNew myWorkspacesConfig
      clock = textClockNew Nothing "%a %b %_d %X" 1
      layout = layoutNew defaultLayoutConfig
      windows = windowsNew defaultWindowsConfig {
        getMenuLabel = truncatedGetMenuLabel 120,
        getActiveLabel = truncatedGetActiveLabel 120
      }
      tray = sniTrayNew
      myConfig = defaultSimpleTaffyConfig
        { startWidgets = 
            workspaces : map (>>= buildContentsBox) [ layout, windows ]
        , endWidgets = (map (>>= buildContentsBox)
          [-- textBatteryNew "$percentage$%"
            battery
          , clock
          , tray
          ])
        , barPosition = Top
        , barPadding = 8 
        , barHeight = 24
        , widgetSpacing = 0
        }
  dyreTaffybar $ withLogServer $
               toTaffyConfig myConfig
