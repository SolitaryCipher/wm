--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import System.Taffybar
import Graphics.UI.Gtk (rcParseString)
import Local.Color
import Local.Taffybar.Sep
import Local.Taffybar.Volume
import Local.Taffybar.Battery2
import Local.Taffybar.Workspaces
import Local.Taffybar.Tray
import Local.Taffybar.Notification
import Local.Taffybar.Clock

font :: String
font = "Fira Mono Medium 11"

--------------------------------------------------------------------------------
main :: IO ()
main = do
  --putStrLn "hi.."
  colors <- getB16Colors 
  let cfg_str = ""
        ++ "style \"taffybar-default\" {"
        ++ "  color[\"black\"] = \"" ++ show (red colors) ++ "\""
        ++ "  color[\"white\"] = \"" ++ show (white colors) ++ "\""
        ++ "  color[\"green\"] = \"" ++ "#00ff00" ++ "\""
        ++ "  color[\"red\"]   = \"" ++ "#ff0000" ++ "\""
        ++ "  font_name        = \"" ++ font      ++ "\""
        ++ "  bg[NORMAL]       = \"" ++ show (darkGrey colors) ++ "\""
        ++ "  fg[NORMAL]       = \"" ++ show (white colors) ++ "\""
        ++ "  text[NORMAL]     = \"" ++ show (white colors) ++ "\""
        ++ "}"
        ++ "style \"taffybar-active-window\" = \"taffybar-default\" {"
        ++ "  fg[NORMAL] = @green"
        ++ "}"
        ++ "style \"taffybar-notification-button\" = \"taffybar-default\" {"
        ++ "  text[NORMAL] = @red"
        ++ "  fg[NORMAL]   = @red"
        ++ "}"
        ++ "widget \"Taffybar*\" style \"taffybar-default\""
        ++ "widget \"Taffybar*WindowSwitcher*label\" style \"taffybar-active-window\""
        ++ "widget \"*NotificationCloseButton\" style \"taffybar-notification-button\""
  writeFile "/home/nick/.config/taffybar/taffybar.rc" cfg_str
  rcParseString cfg_str

  defaultTaffybar defaultTaffybarConfig { barHeight = 24
                                        , startWidgets = [ sepW
                                                         , workspaceW colors
                                                         , notificationW colors
                                                         ]

                                        , endWidgets = [ sepW 
                                                       , clockW 
                                                       , trayW colors
                                                       , volumeW colors
                                                       , batteryW colors
                                                       ]
                                        }
