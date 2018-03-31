{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Taffybar.KBDD where

import Control.Monad ( void )
import Data.Int ( Int32 )
import qualified Data.Map as M
import Data.Text ( Text )
import qualified Data.Text as T
import DBus
import DBus.Client
import Graphics.UI.Gtk hiding ( Signal, Variant )
import Text.Printf


setupDBus = do
  let layoutMatcher = matchAny { matchSender = Nothing
                               , matchDestination = Nothing
                               , matchPath = Just "/ru/gentoo/KbddService"
                               , matchInterface = Just "ru.gentoo.kbdd"
                               , matchMember = Just "LayoutNameChanged"
                               }
  client <- connectSession
  void $ addMatch client layoutMatcher callback

callback :: Signal -> IO ()
callback s = case fromVariant (signalBody s !! 0) of
    Just st -> print st
    _ -> return ()


