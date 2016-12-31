#!/bin/sh

killall polybar
polybar bar1 -c ~/.polybar &
polybar bar2 -c ~/.polybar &
polybar topbar1 -c ~/.polybar &
polybar topbar2 -c ~/.polybar &
