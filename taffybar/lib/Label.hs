module Label (labelW) where

import Utils (defaultDelay)
import System.Taffybar.Widgets.PollingLabel (pollingLabelNew)
import Graphics.UI.Gtk (widgetShowAll)

labelW printer = do
  w <- pollingLabelNew "---" defaultDelay printer
  widgetShowAll w
  return w
