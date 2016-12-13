module ImageWidget ( fileImageWidgetNew 
                   , namedImageWidgetNew
                   , pollingFileImageWidgetNew
                   , pollingNamedImageWidgetNew
                   ) where

import System.FilePath
import System.Glib.UTFString
import Control.Concurrent (forkIO, threadDelay)
import Graphics.UI.Gtk
import Control.Monad (forever)
import qualified Control.Exception as E
import Data.IORef
import System.Process

ignoreIOException :: E.IOException -> IO ()
ignoreIOException _ = return ()

pollingFileImageWidgetNew :: FilePath -> Double -> IO FilePath -> IO Widget
pollingFileImageWidgetNew path interval update = do
  img <- imageNewFromFile path
  pollingImageWidgetNew img interval (update >>= imageSetFromFile img)

pollingNamedImageWidgetNew :: String -> IconSize -> Double -> IO String -> IO Widget
pollingNamedImageWidgetNew name size interval update = do
  img <- imageNewFromIconName name size
  pollingImageWidgetNew img interval (update >>= \n -> imageSetFromIconName img n size)

pollingImageWidgetNew :: Image -> Double -> IO () -> IO Widget
pollingImageWidgetNew img interval imgupdate = do
  box <- hBoxNew False 0
  let icon = img -- icon <- newImageFromFile path...
  _ <- on icon realize $ do
    _ <- forkIO $ forever $ do
      let tryUpdate = postGUIAsync imgupdate 
      E.catch tryUpdate ignoreIOException
      threadDelay $ floor (interval * 1000000)
    return ()
  boxPackStart box icon PackNatural 0
  widgetShowAll box
  return $ toWidget box

fileImageWidgetNew :: FilePath -> IO Widget
fileImageWidgetNew = imageWidgetNew . imageNewFromFile

namedImageWidgetNew :: GlibString s => s -> IconSize -> IO Widget
namedImageWidgetNew str = imageWidgetNew . imageNewFromIconName str

imageWidgetNew :: IO Image -> IO Widget
imageWidgetNew img = do
  box <- hBoxNew False 0
  icon <- img
  boxPackStart box icon PackNatural 0
  widgetShowAll box
  return $ toWidget box


  
