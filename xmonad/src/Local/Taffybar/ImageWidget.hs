module Local.Taffybar.ImageWidget ( fileImageWidgetNew 
                                  , namedImageWidgetNew
                                  , pollingFileImageWidgetNew
                                  , pollingNamedImageWidgetNew
                                  ) where

--import System.FilePath
import System.Glib.UTFString
import Control.Concurrent (forkIO, threadDelay)
import Graphics.UI.Gtk
import Control.Monad (forever)
import qualified Control.Exception as E
--import Data.IORef
--import System.Process

ignoreIOException :: E.IOException -> IO ()
ignoreIOException _ = return ()

pollingFileImageWidgetNew :: FilePath -> Double -> IO FilePath -> IO Widget
pollingFileImageWidgetNew path interval update = do
  img <- imageNewFromFile path
  imageWidgetNew (pollUpdate interval (update >>= imageSetFromFile img)) img

pollingNamedImageWidgetNew :: String -> IconSize -> Double -> IO String -> IO Widget
pollingNamedImageWidgetNew name size interval update = do
  img <- imageNewFromIconName name size 
  imageWidgetNew (pollUpdate interval (update >>= \n -> imageSetFromIconName img n size)) img


pollUpdate :: Double -> IO () -> Image -> IO ()
pollUpdate interval imgupdate img = do
  _ <- on img realize $ do
    _ <- forkIO $ forever $ do
      let tryUpdate = postGUIAsync imgupdate 
      E.catch tryUpdate ignoreIOException
      threadDelay $ floor (interval * 1000000)
    return ()
  return ()

fileImageWidgetNew :: FilePath -> IO Widget
fileImageWidgetNew fp = imageNewFromFile fp >>= imageWidgetNew (const $ return ())
 
namedImageWidgetNew :: GlibString s => s -> IconSize -> IO Widget
namedImageWidgetNew str size = imageNewFromIconName str size >>= imageWidgetNew (const $ return ())

imageWidgetNew :: (Image -> IO ()) -> Image -> IO Widget
imageWidgetNew imgupdate img = do
  box <- hBoxNew False 0
  imgupdate img
  boxPackStart box img PackNatural 0
  widgetShowAll box
  return $ toWidget box

