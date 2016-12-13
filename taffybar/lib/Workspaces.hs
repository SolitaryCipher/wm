module Workspaces (workspaceW) where

import System.Taffybar.TaffyPager
import Graphics.UI.Gtk (escapeMarkup)

import Utils
import Label
import Color


pagerConfig :: ColorSet -> PagerConfig 
pagerConfig colors = PagerConfig { 
    activeWindow     = const " " 
  , activeLayout     = escape
  , activeWorkspace  = setColor colors white (interpolate 0.4 lightGrey magenta) . spacer . escape
  , hiddenWorkspace  = spacer . escape
  , emptyWorkspace   = const ""
  , visibleWorkspace = escape
  , urgentWorkspace  = setColor colors white (interpolate 0.4 lightGrey red) . spacer . escape
  , widgetSep        = "  "
  }
  where surround "(" = \str -> "(" ++ str ++ ")"
        surround "[" = \str -> "[" ++ str ++ "]"
        surround s = \str -> s ++ str ++ s
        spacer = \s -> surround " " s
        escape = escapeMarkup
        setColor colors a b = colorize (show $ a colors) (show $ b colors)

workspaceW colors = taffyPagerNew $ pagerConfig colors
--workspaceW colors = taffyPagerNew defaultPagerConfig 
  

