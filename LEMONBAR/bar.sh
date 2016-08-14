#!/bin/bash

#set normal '%{F#ffffff}'
#set urgent '%{F#e8586e}'
#set good '%{F#00ff00}'
#set inactive '%{F#616161}'
#set separator $inactive' / '$normal

function workspace {
    echo "$(python bar.py)"
}

while [ true ] ; do
	  echo -n "%{c} $(workspace)"
	  echo    ' '
	  sleep .5
done

