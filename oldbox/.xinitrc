#!/bin/sh

# woddfellow2's ~/.xinitrc (oldbox)
# by woddfellow2 | http://wlair.us.to/

# Screen saver
xset s 60 &
xsidle.sh slock &

# Make CAPS LOCK another ESC key
setxkbmap -option caps:escape &

# Set wallpaper
xsetroot -solid black &

# rxvt-unicode daemon
urxvtd -q -o -f

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

# Banish the pointer
banish &

# Start a terminal
urxvtc &

# Start the WM
conky | dwm

# Kill ssh-agent upon exiting
if [ "$SSH_AGENT_PID" ]; then
	ssh-add -D < /dev/null
	eval $(ssh-agent -s -k)
fi
