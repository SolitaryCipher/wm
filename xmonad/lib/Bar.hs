-----------------------------------------------------------------------------
-- | Helper functions to format output for 'bar'
-----------------------------------------------------------------------------

module Bar ( clickable
           , reverseColors
           , bg
           , fg
           , ul
           , ol
           , barApp
           ) where

import System.IO (hPutStrLn) 

clickable :: String -> String -> String
clickable script text = "%{A:" ++ script ++ ":}" ++ text ++ "%{A}" 

reverseColors :: String -> String
reverseColors text = "%{R}" ++ text ++ "%{R}"

bg :: String -> String -> String
bg c s = "%{B"++c++"}"++s++"%{B-}"

fg :: String -> String -> String
fg c s = "%{F"++c++"}"++s++"%{F-}"

ul :: String -> String -> String
ul c s = "%{U"++c++"}%{+u}"++s++"%{-u}"


ol :: String -> String -> String
ol c s = "%{U"++c++"}%{+o}"++s++"%{-o}"

barApp pipe app = hPutStrLn pipe $ start_app app

start_app app = " if [ -n $PID ] && [[ \"$(cat /proc/$PID/cmdline)\" = *urxvt* ]]; then \
                \    kill $PID; \
                \ fi; \
                \ if [[ $PREV != "++app++" ]] || [ ! -d \"/proc/$PID/\" ]; then  \
                \   urxvt -name barapp -g 22x10 -e ~/.xmonad/bar/app.sh "++app++" & PID=$!; \
                \   PREV="++app++"; \
                \ fi"
