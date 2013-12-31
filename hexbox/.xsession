#!/bin/sh
# .xsession 2.3   Time-stamp: <2013-12-30 20:04:31 xoddf2>

# XScreenSaver
xscreensaver -no-splash &

# Dvorak, CAPS LOCK as Compose (for vi, I recommend QWERTY with caps:escape.)
setxkbmap -layout us -variant dvorak -option "compose:caps" &

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

# Open an emacsclient frame:
emacsclient --alternate-editor="" -c &

# Start a WM, or, failing that, an xterm
fvwm || twm || xterm

# Kill ssh-agent upon exiting
if [ "$SSH_AGENT_PID" ]; then
	ssh-add -D < /dev/null
	eval $(ssh-agent -s -k)
fi
