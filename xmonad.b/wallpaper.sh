#!/bin/bash 

#WALLPAPER_DIR=~/.wallpapers/
WALLPAPER_DIR=~/.xmonad/wallpapers/
FEH_ARGS="--no-fehbg --bg-fill"

if [[ -n $@ ]] && [[ -f $1 ]]; then
    feh $FEH_ARGS $1
elif [[ -n $@ ]]; then
    feh $FEH_ARGS $WALLPAPER_DIR/$1
else
    feh --randomize $FEH_ARGS $WALLPAPER_DIR
fi


