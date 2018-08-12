module PolybarLog where

import XMonad

import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import qualified XMonad.StackSet as W

-- Haskell imports
import Data.Monoid
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)

import System.IO

import Colors

runPolybar :: IO ()
runPolybar = do
  out <- runProcessWithInput "polybar" ["--list-monitors"] "" 
  let screens = map (takeWhile (/=':')) $ lines out
  mapM_ (\s -> spawn $ "MONITOR="++s++" polybar --reload -q top") screens

polybarLogHook = do
  ws <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ ws
  let curTag = W.currentTag ws
  let visible = map W.tag $ map W.workspace $ W.visible ws
  let wsStr = join $ map (fmt curTag visible) $ sort' $ namedScratchpadFilterOutWorkspace $ W.workspaces ws

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

  where fmt curTag visible w
          | curTag == W.tag w = "%{B"++base2++" u"++base13++" +u} " ++ W.tag w ++ " %{B- -u}"
          | W.tag w `elem` visible = "%{u"++base11++" +u} " ++ W.tag w ++ " %{-u}"
          | countWindows w > 0 = " %{u"++base10++" +u}" ++ W.tag w ++ "%{-u} "
          | otherwise    = " " ++ W.tag w ++ " "
        sort' = sortBy (compare `on` ((!! 0) . W.tag))
        countWindows = length . W.integrate' . W.stack
