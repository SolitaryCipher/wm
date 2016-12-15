module Local.Taffybar.Volume (volumeW) where

import Graphics.UI.Gtk
import qualified Sound.ALSA.Mixer as M
--import Local.Color
--import Local.Taffybar.Label
import Local.Taffybar.ImageWidget 

type Vol = (Bool,Integer) -- mute, percentage

getVolume :: String -> String -> IO Vol
getVolume name mx = M.withMixer mx $ \mixer -> do
  Just control <- M.getControlByName mixer name
  let Just playbackVolume = M.playback $ M.volume control
  let Just playbackMute = M.playback $ M.switch control
  (_, maxVol) <- M.getRange playbackVolume
  Just vol <- M.getChannel M.FrontLeft $ M.value playbackVolume
  Just mute <- M.getChannel M.FrontLeft playbackMute
  --range <- M.getRange playbackVolume
  let volume = (fromIntegral vol / fromIntegral maxVol) :: Double
  return (mute, round $ volume * 100)


volumeImage :: Vol -> String
volumeImage (False,_) = "audio-volume-muted"
volumeImage (_,pct)
  | pct == 0  = "audio-volume-muted"
  | pct < 40  = "audio-volume-low"
  | pct < 80  = "audio-volume-medium"
  | otherwise = "audio-volume-high"


--genStr = do 
--  (mute,volume) <- getVolume "Master" "default"
--  return $ case mute of True -> "Mute"
--                        _ -> show volume ++ "%"

volumeW :: a -> IO Widget
volumeW _ = volumeIconNew --labelW genStr

volumeIconNew :: IO Widget
volumeIconNew = pollingNamedImageWidgetNew "audio-volume-muted" IconSizeLargeToolbar 2 
                  $ volumeImage <$> getVolume "Master" "default"

