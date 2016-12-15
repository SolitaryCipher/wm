module Local.Taffybar.Tray (trayW) where

import System.Taffybar.Systray
import Graphics.UI.Gtk (Widget)

trayW :: a -> IO Widget
trayW _ = systrayNew
