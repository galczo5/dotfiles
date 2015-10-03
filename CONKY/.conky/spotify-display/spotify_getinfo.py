#!/usr/bin/python

import dbus
import os
import json
import urllib

#Copied from gnome-look
bus = dbus.SessionBus()
player = bus.get_object('com.spotify.qt', '/')
iface = dbus.Interface(player, 'org.freedesktop.MediaPlayer2')
info = iface.GetMetadata()

def perfect_length(str):
	if len(str) > 26:
		return str[:20] + '...'
	else:
		return str

playing_song = str(info['xesam:title'])
playing_artist = str(info['xesam:artist'][0])
playing_album = str(info['xesam:album'])

print(perfect_length(playing_artist) + " - " + perfect_length(playing_song))
print(perfect_length(playing_album))

fstored_album = open(os.getenv('HOME') + '/.conky/spotify-display/stored_album.txt', 'r')
stored_album = fstored_album.readline().strip('\n')
fstored_album.close()

if playing_album != stored_album:
        #Build url to lastfm api
        lastfm_api_key = "<YOUR API KEY>" 
        lastfm_api = "http://ws.audioscrobbler.com/2.0/?method=album.getinfo&api_key=" + lastfm_api_key + "&artist=" + playing_artist + "&album=" + playing_album + "&format=json"
        
        #Get all info from lastfm
        f = urllib.urlopen(lastfm_api)
        values = json.load(f)

        #Get the biggest image
        album_url = values["album"]["image"][-1]["#text"]
	os.system("wget -O $HOME/.conky/spotify-display/latest.jpg \"" + album_url + "\"")

	os.system("echo \"" + playing_album + "\" > $HOME/.conky/spotify-display/stored_album.txt")
