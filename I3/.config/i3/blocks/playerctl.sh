#!/bin/bash
STATUS=$(playerctl status)

if [ "$STATUS" = "Playing" ] ; then
    echo "   Now Playing: " $(playerctl metadata title)

elif [ "$STATUS" = "Paused" ] ; then
    echo "   Paused: " $(playerctl metadata title)
fi
