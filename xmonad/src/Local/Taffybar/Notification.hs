module Local.Taffybar.Notification (notificationW) where 

import System.Taffybar.FreedesktopNotifications
import qualified Data.Text as T
import Data.Monoid (mconcat)
import Graphics.UI.Gtk (Widget)


formatter :: Notification -> String
formatter note = msg
  where
    msg = if T.null (noteBody note) 
          then T.unpack $ noteSummary note
          else T.unpack $ mconcat [ T.pack "<span fgcolor='yellow'>Note:</span> "
                                  , noteSummary note, T.pack ": ", noteBody note ]

notificationW :: a -> IO Widget
notificationW _ = notifyAreaNew NotificationConfig 
    { notificationFormatter = formatter
    , notificationMaxLength = 150
    , notificationMaxTimeout = 10
    }
