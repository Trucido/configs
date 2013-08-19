#!/bin/sh
# $Id: .xsession,v 1.2 2013/08/18 23:39:15 xoddf2 Exp $

# XScreenSaver
xscreensaver -no-splash &

# Set layout to Dvorak (must run just before xmodmap to avoid race condition)
setxkbmap -layout us -variant dvorak

# Make CAPS LOCK another ESC key and shuffle the modifiers
xmodmap ~/.Xmodmap &

# Set wallpaper
xsetroot -solid DarkGrey &

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

# Start the WM
fvwm

# Kill ssh-agent upon exiting
if [ "$SSH_AGENT_PID" ]; then
	ssh-add -D < /dev/null
	eval $(ssh-agent -s -k)
fi
