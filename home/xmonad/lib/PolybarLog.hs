module PolybarLog where

import XMonad

import XMonad.Util.NamedWindows
import qualified XMonad.StackSet as W

-- Haskell imports
import Data.Monoid
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)

import System.IO

polybarLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let visible = map W.tag $ W.visible winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs visible) $ sort' wss

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

  where fmt currWs visible ws
          | currWs == ws = "[" ++ ws ++ "]"
          | ws `elem` visible = "(" ++ ws ++ ")"
          | otherwise    = " " ++ ws ++ " "
        sort' = sortBy (compare `on` (!! 0))
