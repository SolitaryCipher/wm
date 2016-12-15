module Local.Taffybar.Battery (batteryW) where

import Local.Taffybar.Label
import Local.Color
import Local.Utils
import System.Information.Battery


getInfo = do
  (Just ctx) <- batteryContextNew 
  getBatteryInfo ctx

instance Show BatteryInfo where
  show info =    show (round $ batteryPercentage info) ++ "%"
              ++ " "
              ++ showStatus (batteryState info)

showStatus :: BatteryState -> String
showStatus BatteryStateCharging	        = "Charging"
showStatus BatteryStateDischarging	    = "Discharging"
showStatus BatteryStateEmpty	          = "Empty"
showStatus BatteryStateFullyCharged	    = "Charged"
--showStatus BatteryStateUnknown	        = "Unknown"
--showStatus BatteryStatePendingCharge	  = "Pending"
--showStatus BatteryStatePendingDischarge = "Pending"
showStatus _ = "Unknown"

getBatteryColor :: ColorSet -> BatteryInfo -> String -> String
getBatteryColor colors info
  | state == BatteryStateCharging = colorize "" $ interpolate' 0.3 green
  | state == BatteryStateUnknown  = colorize "" $ interpolate' 0.3 green
  | p < 25                        = colorize "" $ interpolate' 0.4 red
  | p < 50                        = colorize "" $ interpolate' 0.3 yellow
  | otherwise                     = colorize "" $ interpolate' 0.3 green
    where state = batteryState info
          p = batteryPercentage info
          interpolate' p c = show $ interpolate p (darkGrey colors) (c colors)

genStr colors = do
  info <- getInfo
  case info of Nothing  -> return "Unknown"
               (Just i) -> return $ spacer $ show i
               --(Just i) -> return $ (getBatteryColor colors i) (spacer $ show i)

batteryW = labelW . genStr 
