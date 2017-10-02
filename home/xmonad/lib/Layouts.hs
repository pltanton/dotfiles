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
import XMonad.Layout.Decoration
import XMonad.Layout.Simplest

import Colors
import Defaults

layoutIcon :: String -> String
layoutIcon name = "^i(/home/anton/.xmonad/icons/" ++ name ++ ".xpm)"

myLayout =
    smartBorders $
    named "T" tiled |||
    named "M" (Mirror tiled) |||
    named "G" grid |||
    named "B" myTabbed
  where
    myTabbed :: ModifiedLayout Gaps (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest) Window
    myTabbed = gapsed $ tabbed shrinkText tabConfig
    tiled  = spaces $ ResizableTall nmaster delta ratio []
    grid   = spaces $ Grid

    tabConfig = def { activeColor         = myDarkGray
                    , inactiveColor       = myBlack 
                    , fontName            = defaultFont
                    , activeBorderColor   = myDarkGray 
                    , inactiveBorderColor = myBlack 
                    , decoHeight          = 28 }
    spaces    = spacing 4
    gapsed    = gaps [(U,4), (D,4), (R,4), (L,4)]
    full      = spaces Full
    nmaster   = 1
    ratio     = 1/2
    delta     = 3/100

