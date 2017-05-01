module Layouts where
import XMonad

import XMonad.Layout.Named
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.HintedGrid
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier

layoutIcon :: String -> String
layoutIcon name = "^i(/home/anton/.xmonad/icons/" ++ name ++ ".xpm)"

myLayout :: Choose (XMonad.Layout.LayoutModifier.ModifiedLayout XMonad.Layout.Renamed.Rename (XMonad.Layout.LayoutModifier.ModifiedLayout Spacing ResizableTall)) (Choose (XMonad.Layout.LayoutModifier.ModifiedLayout XMonad.Layout.Renamed.Rename (Mirror (XMonad.Layout.LayoutModifier.ModifiedLayout Spacing ResizableTall))) (Choose (XMonad.Layout.LayoutModifier.ModifiedLayout XMonad.Layout.Renamed.Rename (XMonad.Layout.LayoutModifier.ModifiedLayout Spacing Grid)) (XMonad.Layout.LayoutModifier.ModifiedLayout XMonad.Layout.Renamed.Rename (XMonad.Layout.LayoutModifier.ModifiedLayout Spacing Full)))) a
myLayout =
    named "T" tiled |||
    named "M" (Mirror tiled) |||
    named "G" grid |||
    named "F" full
  where
    tiled   = spaces $ ResizableTall nmaster delta ratio []
    grid    = spaces $ Grid False
    spaces  = spacing 4
    full    = spaces Full
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

