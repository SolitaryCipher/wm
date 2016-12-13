#!/bin/bash

# -a [#clickalbe]
# -f [font]
# -p (dont close after stdin is closed)
# -b bottom

. $HOME/.config/admiral.d/lemonbar_config.sh

middle_script="$HOME/.config/admiral.d/for_all_mons.sh"

killall admiral     # cleanup
killall lemonbar
killall stalonetray

rm -f $MY_FIFO
mkfifo $MY_FIFO

(sleep 5 && stalonetray -bg "#$base02") &
(cat > $MY_FIFO) &                 # keep pipe open
$admiral | $HOME/.config/admiral.d/for_all_mons.sh | $bar "${barargs[@]}" | sh &  # start bar (reads from /tmp/bar.fifo)
(cat > $MY_FIFO)                   # write stdin to /tmp/bar.fifo
                                   # possible color and stuff?

