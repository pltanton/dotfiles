module Layouts where

-- XMonad imports
import XMonad

import XMonad.Layout.Named
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders

layoutIcon :: String -> String
layoutIcon name = "^i(/home/anton/.xmonad/icons/" ++ name ++ ".xpm)"

myLayout :: ModifiedLayout SmartBorder (Choose (ModifiedLayout Rename (ModifiedLayout Spacing ResizableTall)) (Choose (ModifiedLayout Rename (Mirror (ModifiedLayout Spacing ResizableTall))) (Choose (ModifiedLayout Rename (ModifiedLayout Spacing Grid)) (ModifiedLayout Rename (ModifiedLayout Spacing Full))))) a
myLayout =
    smartBorders $
    named "T" tiled |||
    named "M" (Mirror tiled) |||
    named "G" grid |||
    named "F" full
  where
    tiled   = spaces $ ResizableTall nmaster delta ratio []
    grid    = spaces $ Grid
    spaces  = spacing 4
    full    = spaces Full
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

