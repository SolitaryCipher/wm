#!/bin/bash
. lemonbar_config.sh

interface="wlp2s0"
essid=$(iwgetid -r)

if [ ! -z $essid ]
then
  #echo "$(bar_ok) Wifi: $essid "
  echo ""
fi
