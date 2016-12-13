#!/bin/bash 

# base00="#1d1f21" # Base 00 - Black
# base01="#282a2e" # Base 01 
# base02="#373b41" # Base 02
# base03="#969896" # Base 03 - Bright Black
# base04="#b4b7b4" # Base 04
# base05="#c5c8c6" # Base 05 - White
# base06="#e0e0e0" # Base 06
# base07="#ffffff" # Base 07 - Bright White
# 
# base08="#cc6666" # Base 08 - Red
# base09="#de935f" # Base 09 - Orange
# base0A="#f0c674" # Base 0A - Yellow
# base0B="#b5bd68" # Base 0B - Green
# base0C="#8abeb7" # Base 0C - Cyan
# base0D="#81a2be" # Base 0D - Blue
# base0E="#b294bb" # Base 0E - Magenta
# base0F="#a3685a" # Base 0F - Brown

BAR_BG=$base02
BAR_FG=$base06
BAR_FONT="Hack-regular:size=11"
BAR_WIDTH="1854"
BAR_HEIGHT="x23"
OFFSET_X="+0"
OFFSET_Y="+0"


export bar=$HOME/.config/admiral.d/lemonbar
export admiral=$HOME/.config/admiral.d/admiral
export MY_FIFO=/tmp/bar.fifo

export barargs=("-f" $BAR_FONT
                "-B" "#$BAR_BG"
                "-F" "#$BAR_FG"
                "-u" "2"
                "-g" "$BAR_WIDTH$BAR_HEIGHT$OFFSET_X$OFFSET_Y")


bar_critical() {
    echo "%{B#6F5353}%{F#$base07}%{U#$base08}%{+o}"
}

bar_warn() {
    echo "%{B#5B5750}%{U#$base0A}%{+o}"
}

bar_low() {
    echo "%{U#$base0B}%{F#$base05}%{+o}"
}

bar_good() {
    echo "%{B#56564F}%{F#$base07}%{U#$base0B}%{+o}"
}

bar_ok() {
    echo "%{B#$base02}%{U#$base09}%{+o}"
}

bar_bg() {
    echo "%{B#$1}"
}

bar_clickable() {
  echo "%{A:$1:}$2%{A}"
}
