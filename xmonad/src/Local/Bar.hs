-----------------------------------------------------------------------------
-- | Helper functions to format output for 'bar'
-----------------------------------------------------------------------------

module Local.Bar ( clickable
                 , reverseColors
                 , bg
                 , fg
                 , ul
                 , ol
                 , barApp
                 ) where

import System.IO (hPutStrLn, Handle) 

clickable :: String -> String -> String
clickable script text = "%{A:" ++ script ++ ":}" ++ text ++ "%{A}" 

reverseColors :: String -> String
reverseColors text = "%{R}" ++ text ++ "%{R}"

bg :: String -> String -> String
bg c s = "%{B"++c++"}"++s++"%{B-}"

fg :: String -> String -> String
fg c s = "%{F"++c++"}"++s++"%{F-}"

ul :: String -> String -> String
ul c s = "%{U"++c++"}%{+u}"++s++"%{-u}%{U-}"


ol :: String -> String -> String
ol c s = "%{U"++c++"}%{+o}"++s++"%{-o}%{U-}"

barApp :: Handle -> String -> IO ()
barApp pipe app = hPutStrLn pipe $ startApp app

startApp :: String -> String
startApp app = " if [ -n $PID ] && [[ \"$(cat /proc/$PID/cmdline)\" = *urxvt* ]]; then \
                \    kill $PID; \
                \ fi; \
                \ if [[ $PREV != "++app++" ]] || [ ! -d \"/proc/$PID/\" ]; then  \
                \   urxvt -name barapp -g 22x10 -e ~/.config/admiral.d/bar_app.sh "++app++" & PID=$!; \
                \   PREV="++app++"; \
                \ fi"
