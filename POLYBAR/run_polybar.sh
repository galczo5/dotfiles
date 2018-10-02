#!/bin/sh

killall polybar

if type "xrandr"; then
  for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
      MONITOR=$m polybar topbar -c .topbar &
      MONITOR=$m polybar bottombar -c .bottombar &
  done
else
    polybar topbar -c .topbar &
    polybar bottombar -c .bottombar
fi
