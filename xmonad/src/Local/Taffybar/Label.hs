module Local.Taffybar.Label (labelW) where

import Local.Utils (defaultDelay)
import System.Taffybar.Widgets.PollingLabel (pollingLabelNew)
import Graphics.UI.Gtk (Widget, widgetShowAll)

labelW :: IO String -> IO Widget
labelW printer = do
  w <- pollingLabelNew "---" defaultDelay printer
  widgetShowAll w
  return w
