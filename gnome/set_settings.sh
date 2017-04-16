#!/usr/bin/env bash


CONFIG_FILE=./gsettings_custom.txt

while read line
do
  echo "gsettings set $line"
  gsettings set $line
done < $CONFIG_FILE
