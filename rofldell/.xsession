#!/bin/sh
# .xsession 2.4   Time-stamp: <2014-03-24 01:04:03 PDT xoddf2>

# ~/.Xresources
xrdb -merge ~/.Xresources &

# XScreenSaver
xscreensaver -no-splash &

# CAPS LOCK as Compose
setxkbmap -option "compose:caps" &

# xearth or set wallpaper
xearth || xsetroot -mod 2 2 -bg Grey5 -fg Grey10 &

# Open an emacsclient frame:
emacsclient --alternate-editor="" -c &

# Start a WM, or, failing that, an xterm
fvwm || twm || xterm
