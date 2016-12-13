module Volume (volumeW) where

import Graphics.UI.Gtk
import Sound.ALSA.Mixer
import Label
import Color
import ImageWidget 

type Vol = (Bool,Integer) -- mute, percentage

getVolume :: String -> String -> IO Vol
getVolume name mx = do
  withMixer mx $ \mixer -> do
    Just control <- getControlByName mixer name
    let Just playbackVolume = playback $ volume control
    let Just playbackMute = playback $ switch control
    (_, max) <- getRange playbackVolume
    Just vol <- getChannel FrontLeft $ value $ playbackVolume
    Just mute <- getChannel FrontLeft playbackMute
    range <- getRange playbackVolume
    return (mute, round $ (fromIntegral vol / fromIntegral max) * 100)


volumeImage :: Vol -> String
volumeImage (False,_) = "audio-volume-muted"
volumeImage (_,pct)
  | pct == 0  = "audio-volume-muted"
  | pct < 40  = "audio-volume-low"
  | pct < 80  = "audio-volume-medium"
  | otherwise = "audio-volume-high"


genStr = do 
  (mute,volume) <- getVolume "Master" "default"
  return $ case mute of True -> "Mute"
                        _ -> show volume ++ "%"

volumeW colors = volumeIconNew --labelW genStr

volumeIconNew :: IO Widget
volumeIconNew = do
  b <- hBoxNew False 1
  txt <- genStr
  --img <- pollingNamedImageWidgetNew "audio-volume-muted" IconSizeLargeToolbar 2 (return $ volumeImage (False,1))
  img <- pollingNamedImageWidgetNew "audio-volume-muted" IconSizeLargeToolbar 2 $ fmap volumeImage $ getVolume "Master" "default"
  return img

