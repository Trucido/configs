#!/bin/sh

# woddfellow2's ~/.xsession (oldbox)
# by woddfellow2 | http://wlair.us.to/

# XScreenSaver
xscreensaver -no-splash &

# xmodmap
xmodmap ~/.Xmodmap &

# Set wallpaper
xsetroot -solid black &

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
i3

# Kill ssh-agent upon exiting
if [ "$SSH_AGENT_PID" ]; then
	ssh-add -D < /dev/null
	eval $(ssh-agent -s -k)
fi
