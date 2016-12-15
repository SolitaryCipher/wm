module Local.Taffybar.Clock (clockW) where

import System.Taffybar.SimpleClock
import Graphics.UI.Gtk (Widget)

clockW :: IO Widget
clockW = textClockNew Nothing "%H:%M" 1
