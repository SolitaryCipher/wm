{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Battery2 (batteryW) where

import qualified Control.Exception.Enclosed as E
import Data.Int ( Int64 )
import Data.IORef
import Graphics.UI.Gtk
import qualified System.IO as IO
import Text.Printf ( printf )
import Text.StringTemplate

import System.Information.Battery
import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingLabel

import ImageWidget

import Color


-- I do my own configuration.
batteryConfig :: ColorSet -> BarConfig
batteryConfig colors = BarConfig 
  { barColor = \p -> getColor BatteryStatePendingDischarge p colors
  , barBorderColor = rgbTuple $ darkGrey colors
  , barBackgroundColor = const $ rgbTuple $ black colors
  , barPadding = 2
  , barWidth = 15
  , barDirection = VERTICAL
  }

getColor :: BatteryState -> Double -> ColorSet -> (Double,Double,Double)
getColor BatteryStateCharging _ = rgbTuple . orange
getColor _ pct 
  | pct < 0.2 = rgbTuple . red    --(1, 0, 0)
  | pct < 0.5 = rgbTuple . yellow --(1, 0, 0)
  | pct < 0.9 = rgbTuple . white  --(0.5, 0.5, 0.5)
  | otherwise = rgbTuple . green  --(0, 1, 0)

batteryW colors = batteryBarNew (batteryConfig colors) "$percentage$%" 5


battImage :: BatteryState -> Double -> String
battImage BatteryStateUnknown _ = "battery-missing"
battImage status pct = "battery-" ++ sstate pct
  where chargeState BatteryStateCharging = "-charging"
        chargeState _ = ""
        sstate p
          | p < 10    = "empty"   ++ chargeState status 
          | p < 20    = "caution" ++ chargeState status 
          | p < 40    = "low"     ++ chargeState status 
          | p < 60    = "medium"  ++ chargeState status 
          | p < 90    = "good"    ++ chargeState status 
          | p <= 100  = "full"    ++ chargeState status 
          | otherwise = "missing" 

battImg :: IORef BatteryContext -> IO String
battImg r = do
  minfo <- safeGetBatteryInfo r
  case minfo of
    Just info -> return $ battImage (batteryState info) (batteryPercentage info)
    _ -> return "battery-missing"

-- | This module provides battery widgets using the UPower system
-- service.
--
-- Currently it reports only the first battery it finds.  If it does
-- not find a batterym it just returns an obnoxious widget with
-- warning text in it.  Battery hotplugging is not supported.  These
-- more advanced features could be supported if there is interest.
safeGetBatteryInfo :: IORef BatteryContext -> IO (Maybe BatteryInfo)
safeGetBatteryInfo mv = do
  ctxt <- readIORef mv
  E.catchAny (getBatteryInfo ctxt) $ \_ -> reconnect
  where
    reconnect = do
      mctxt <- batteryContextNew
      case mctxt of
        Nothing -> IO.hPutStrLn IO.stderr "Could not reconnect to UPower"
        Just ctxt -> writeIORef mv ctxt
      return Nothing

battInfo :: IORef BatteryContext -> String -> IO String
battInfo r fmt = do
  minfo <- safeGetBatteryInfo r
  case minfo of
    Nothing -> return ""
    Just info -> do
      let battPctNum :: Int
          battPctNum = floor (batteryPercentage info)
          formatTime :: Int64 -> String
          formatTime seconds =
            let minutes = seconds `div` 60
                hours = minutes `div` 60
                minutes' = minutes `mod` 60
            in printf "%02d:%02d" hours minutes'

          battTime :: String
          battTime = case (batteryState info) of
            BatteryStateCharging -> (formatTime $ batteryTimeToFull info)
            BatteryStateDischarging -> (formatTime $ batteryTimeToEmpty info)
            _ -> "-"
          
          battStatus :: String
          battStatus = case (batteryState info) of
            BatteryStateCharging -> "⚡ "
            --BatteryStateCharging -> "⚡  "
            --BatteryStateDischarging    -> ""
            _ -> ""
            

          tpl = newSTMP fmt
          tpl' = setManyAttrib [ ("percentage", show battPctNum)
                               , ("time", battTime)
                               , ("status", battStatus)
                               ] tpl
      return $ render tpl'

-- | A simple textual battery widget that auto-updates once every
-- polling period (specified in seconds).  The displayed format is
-- specified format string where $percentage$ is replaced with the
-- percentage of battery remaining and $time$ is replaced with the
-- time until the battery is fully charged/discharged.
textBatteryNew :: String    -- ^ Display format
                  -> Double -- ^ Poll period in seconds
                  -> IO Widget
textBatteryNew fmt pollSeconds = do
  battCtxt <- batteryContextNew
  case battCtxt of
    Nothing -> do
      let lbl :: Maybe String
          lbl = Just "No battery"
      labelNew lbl >>= return . toWidget
    Just ctxt -> do
      r <- newIORef ctxt
      l <- pollingLabelNew "" pollSeconds (battInfo r fmt)
      widgetShowAll l
      return l

-- | Returns the current battery percent as a double in the range [0,
-- 1]
battPct :: IORef BatteryContext -> IO Double
battPct r = do
  minfo <- safeGetBatteryInfo r
  case minfo of
    Nothing -> return 0
    Just info -> return (batteryPercentage info / 100)

-- | A default configuration for the graphical battery display.  The
-- bar will be red when power is critical (< 10%), green if it is full
-- (> 90%), and grey otherwise.
--
-- You can customize this with any of the options in 'BarConfig'
defaultBatteryConfig :: BarConfig
defaultBatteryConfig =
  defaultBarConfig colorFunc
  where
    colorFunc pct
      | pct < 0.1 = (1, 0, 0)
      | pct < 0.9 = (0.5, 0.5, 0.5)
      | otherwise = (0, 1, 0)

-- | A fancy graphical battery widget that represents the current
-- charge as a colored vertical bar.  There is also a textual
-- percentage readout next to the bar.
batteryBarNew :: BarConfig -- ^ Configuration options for the bar display
                 -> String
                 -> Double -- ^ Polling period in seconds
                 -> IO Widget
batteryBarNew battCfg fmt pollSeconds = do
  battCtxt <- batteryContextNew
  case battCtxt of
    Nothing -> do
      let lbl :: Maybe String
          lbl = Just "No battery"
      labelNew lbl >>= return . toWidget
    Just ctxt -> do
      -- This is currently pretty inefficient - each poll period it
      -- queries the battery twice (once for the label and once for
      -- the bar).
      --
      -- Converting it to combine the two shouldn't be hard.
      b <- hBoxNew False 1
      txt <- textBatteryNew fmt pollSeconds
      r <- newIORef ctxt
      --bar <- pollingBarNew battCfg pollSeconds (battPct r)
      img <- pollingNamedImageWidgetNew "battery-missing" IconSizeLargeToolbar 2 (battImg r)
      --img <- pollingIconImageWidgetNew "icons/battery-empty2.xbm" 2 (return "icons/battery-empty2.xbm") 
      boxPackStart b img PackNatural 0
      --boxPackStart b bar PackNatural 0
      boxPackStart b txt PackNatural 0
      widgetShowAll b
      return (toWidget b)
