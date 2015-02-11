#!/bin/sh
# .xsession 3.7
# Time-stamp: <2015-02-11 12:27:09 PST xoddf2>

# ssh-agent
id1=$HOME/.ssh/identity
id2=$HOME/.ssh/id_dsa
id3=$HOME/.ssh/id_rsa
id4=$HOME/.ssh/id_ecdsa
if [ -x "$(which ssh-agent)" ] && [ -f $id1 -o -f $id2 -o -f $id3 -o -f $id4 ];
then
	eval $(ssh-agent -s)
	ssh-add < /dev/null
fi

# X resources
xrdb -merge ~/.Xresources

# XScreenSaver
xscreensaver -no-splash &

# CAPS LOCK as Compose
setxkbmap -option "compose:caps" &

# Root window image, pointer, and compositor
compton -b
xsetroot -cursor_name left_ptr &
feh --bg-center -z ~/img/wallpaper/ &

# rxvt-unicode daemon
urxvtd -q -o -f

# Start a WM, or, failing that, an xterm
xmonad || cwm || twm || xterm

# Kill ssh-agent at the end of the session
if [ "$SSH_AGENT_PID" ]; then
	ssh-add -D < /dev/null
	eval $(ssh-agent -s -k)
fi
