module Local.Taffybar.Workspaces (workspaceW) where

import System.Taffybar.TaffyPager
import Graphics.UI.Gtk (escapeMarkup, Widget)

import Local.Color


pagerConfig :: ColorSet -> PagerConfig 
pagerConfig colors = PagerConfig 
  { activeWindow     = const " " 
  , activeLayout     = escape
  , activeWorkspace  = setColor colors white (interpolate 0.4 darkGrey green) . spacer . escape
  , hiddenWorkspace  = spacer . escape
  , emptyWorkspace   = const ""
  , visibleWorkspace = escape
  , urgentWorkspace  = setColor colors white (interpolate 0.5 darkGrey red) . spacer . escape
  , widgetSep        = "  "
  }
  where surround s = \str -> s ++ str ++ s
        --surround "(" = \str -> "(" ++ str ++ ")"
        --surround "[" = \str -> "[" ++ str ++ "]"
        spacer = surround " "
        escape = escapeMarkup
        setColor colors' a b = colorize (show $ a colors') (show $ b colors')


workspaceW :: ColorSet -> IO Widget
workspaceW colors = taffyPagerNew $ pagerConfig colors


