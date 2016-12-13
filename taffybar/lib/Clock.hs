module Clock (clockW) where

import System.Taffybar.SimpleClock

clockW = textClockNew Nothing "%H:%M" 1
