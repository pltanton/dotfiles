module Scratchpads (myScratchPads) where

import XMonad
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W

runScratchpadTerminal :: String
runScratchpadTerminal =
    "termite --name 'scratchpad' -e \
    \ 'sh -c \"tmux a -t scratchpad || tmux new -s scratchpad\"'"

connectToRt :: String
connectToRt = "termite --name 'torrent' -e 'sh -c \"tmux -L rt attach -t rt\"'"

myScratchPads :: [NamedScratchpad]
myScratchPads =
    [ NS "terminal"
         runScratchpadTerminal
         (resource =? "scratchpad")
         floatingTerm
    , NS "telegram"
         "telegram-desktop"
         (resource =? "telegram-desktop" <&&> (isDialog >>= return . not))
         floatingRight
    , NS "evolution"
         "evolution"
         (resource =? "evolution")
         floatingCenter
    ]
  where
    floatingTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.4
        w = 1
        t = 1 - h
        l = 1 - w
    floatingRight = customFloating $ W.RationalRect l t w h
      where
        h = 1
        w = 0.3
        t = 1 - h
        l = 1 - w
    floatingCenter = customFloating $ W.RationalRect l t w h
      where
        offset = 0.03
        h = 1 - offset * 2
        w = 1 - offset * 2
        t = offset
        l = offset
