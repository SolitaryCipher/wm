module Local.Taffybar.Sep (sepW) where

import Local.Taffybar.Label
import Graphics.UI.Gtk (Widget)

sepW :: IO Widget 
sepW = labelW $ return " "

