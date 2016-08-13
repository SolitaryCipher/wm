
module Color    ( Color(..)
                , ColorSet(..)
                , getColor
                ) where

data Color = Red 
           | Orange
           | Yellow
           | Green
           | Cyan
           | Blue 
           | Magenta
           | Brown
           | Black
           | LightGrey
           | DarkGrey
           | White

data ColorSet = ColorSet { red       :: String
                         , orange    :: String
                         , yellow    :: String
                         , green     :: String
                         , cyan      :: String
                         , blue      :: String
                         , magenta   :: String
                         , brown     :: String
                         , black     :: String
                         , lightGrey :: String
                         , darkGrey  :: String
                         , white     :: String
                         }


getColor :: ColorSet -> Color -> String
getColor bc Red       = red       bc
getColor bc Orange    = orange    bc
getColor bc Yellow    = yellow    bc
getColor bc Green     = green     bc
getColor bc Cyan      = cyan      bc
getColor bc Blue      = blue      bc
getColor bc Magenta   = magenta   bc
getColor bc Brown     = brown     bc
getColor bc Black     = black     bc
getColor bc DarkGrey  = darkGrey  bc
getColor bc LightGrey = lightGrey bc
getColor bc White     = white     bc


