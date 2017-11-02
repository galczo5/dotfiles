#!/bin/sh

killall polybar
polybar traybar -c ~/.polybar &
polybar topbar1 -c ~/.polybar &
#polybar topbar2 -c ~/.polybar &
