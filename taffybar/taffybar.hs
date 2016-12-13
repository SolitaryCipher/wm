import System.Taffybar

-- todo: colors improvement
--       volume
--       better battery (with icon)

import Sep
import Utils
import Color

import Workspaces
import Notification

import Volume
import Battery2
import Clock
import Tray

import Graphics.UI.Gtk.General.RcStyle (rcParseString)

font = "Fira Code Medium 11"

main = do
  colors <- getB16Colors 
  rcParseString $ ""
        ++ "style \"taffybar-default\" {"
        ++ "  color[\"black\"] = \"" ++ show (darkGrey colors) ++ "\""
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
