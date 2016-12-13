module Notification (notificationW) where 

import System.Taffybar.FreedesktopNotifications
import qualified Data.Text as T
import Data.Monoid (mconcat)
import Data.Text (Text)


formatter :: Notification -> String
formatter note = msg
  where
    msg = case T.null (noteBody note) of
      True -> T.unpack $ noteSummary note
      False -> T.unpack $ mconcat [ T.pack "<span fgcolor='yellow'>Note:</span> "
                                  , noteSummary note, T.pack ": ", noteBody note ]

notificationW _ = notifyAreaNew 
  $ NotificationConfig 
    { notificationFormatter = formatter
    , notificationMaxLength = 150
    , notificationMaxTimeout = 10
    }
