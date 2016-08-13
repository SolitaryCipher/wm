#!/bin/bash

# -a [#clickalbe]
# -f [font]
# -p (dont close after stdin is closed)
# -b bottom

. $HOME/.config/admiral.d/lemonbar_config.sh

middle_script="$HOME/.config/admiral.d/for_all_mons.sh"

killall admiral     # cleanup
killall lemonbar

if [[ ! -p $MY_FIFO ]]; then
    mkfifo /tmp/bar.fifo
fi


(cat > /tmp/bar.fifo) &                 # keep pipe open
$admiral | $HOME/.config/admiral.d/for_all_mons.sh | $bar "${barargs[@]}" | sh &  # start bar (reads from /tmp/bar.fifo)
(cat > /tmp/bar.fifo)                   # write stdin to /tmp/bar.fifo
                                        # possible color and stuff?

