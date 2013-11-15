#!/bin/sh
# $Id: .xsession,v 2.1 2013/11/14 16:19:04 xoddf2 Exp $

# XScreenSaver
xscreensaver -no-splash &

# Dvorak, CAPS LOCK as ESC, and Compose
setxkbmap -layout us -variant dvorak \
          -option "compose:ralt" -option "caps:escape" &

# Set wallpaper
xsetroot -mod 2 2 -bg Grey5 -fg Grey10 &

# ssh-agent (Inspired by and stolen from OpenBSD)
id1=$HOME/.ssh/identity
id2=$HOME/.ssh/id_dsa
id3=$HOME/.ssh/id_rsa
id4=$HOME/.ssh/id_ecdsa
if [ -x /usr/bin/ssh-agent ] && [ -f $id1 -o -f $id2 -o -f $id3 -o -f $id4 ];
then
	eval $(ssh-agent -s)
	ssh-add < /dev/null
fi

# Start some nice programs
xclock -geometry -0-0 &
xload -geometry 184x48-0-34 &
xbiff -geometry -184-34 &

# ...and an emacsclient frame:
emacsclient --alternate-editor="" -c &

# Start the WM
fvwm

# Kill ssh-agent upon exiting
if [ "$SSH_AGENT_PID" ]; then
	ssh-add -D < /dev/null
	eval $(ssh-agent -s -k)
fi
