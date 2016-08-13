#!/bin/bash

monitors=$(xrandr | grep -o "^.* connected" | sed "s/ connected//")

while IFS= read -r line; do
  barout=""
  tmp=0
  for m in $(echo "$monitors"); do
    barout+="%{S${tmp}}$line"
    let tmp=$tmp+1
  done
  echo "$barout"
done

