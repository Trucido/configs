#!/bin/sh
# $Id: .xsession,v 1.1 2013/08/07 02:31:45 xoddf2 Exp $

# XScreenSaver
xscreensaver -no-splash &

# Make CAPS LOCK another ESC key
setxkbmap -option caps:escape &

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
