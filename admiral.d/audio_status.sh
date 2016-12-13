#!/bin/bash
. ./lemonbar_config.sh

cmd=$(amixer sget Master)


vol=$(echo "$cmd" | awk '$0~/%/{print $4}' | tr -d '[]')
mute=$(echo "$cmd" | awk '$0~/%/{print $6}' | tr -d '[]')
headset=$(amixer sget Headphone | awk '$0~/%/{print $4}' | head -n 1)
str=""
COLOR=""

if [ "$mute" == "on" ]; then
  str="$vol" 
else
  str="Mute"
fi

if [ "$headset" -eq "0" ]; then
  str="$str Speaker"
else
  str="$str Headset"
fi

echo "$COLOR $str"
