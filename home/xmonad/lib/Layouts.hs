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
import XMonad.Layout.Tabbed
import XMonad.Layout.Gaps

layoutIcon :: String -> String
layoutIcon name = "^i(/home/anton/.xmonad/icons/" ++ name ++ ".xpm)"

myLayout =
    smartBorders $
    named "T" tiled |||
    named "M" (Mirror tiled) |||
    named "G" grid |||
    named "B" tabbed
  where
    tabbed  = gapsed simpleTabbed
    tiled   = spaces $ ResizableTall nmaster delta ratio []
    grid    = spaces $ Grid
    spaces  = spacing 4
    gapsed  = gaps $ zip [U, D, R, L] [2..]
    full    = spaces Full
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

