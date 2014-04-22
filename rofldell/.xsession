#!/bin/sh
# .xsession 2.5.1   Time-stamp: <2014-04-21 20:56:22 PDT xoddf2>

# ssh-agent (Under FreeBSD, this requires ports/security/openssh-askpass.)
id1=$HOME/.ssh/identity
id2=$HOME/.ssh/id_dsa
id3=$HOME/.ssh/id_rsa
id4=$HOME/.ssh/id_ecdsa
if [ -x "$(which ssh-agent)" ] && [ -f $id1 -o -f $id2 -o -f $id3 -o -f $id4 ];
then
	eval $(ssh-agent -s)
	ssh-add < /dev/null
fi

# ~/.Xresources
xrdb -merge ~/.Xresources

# XScreenSaver
xscreensaver -no-splash &

# CAPS LOCK as Compose
setxkbmap -option "compose:caps" &

# Set background colour
xsetroot -solid Grey20 &

# tmux
xterm -sl 0 -geometry 80x50+0+0 -e tmux attach-session &

# Start a WM, or, failing that, an xterm
cwm || fvwm || twm || xterm

# Kill ssh-agent at the end of the session
if [ "$SSH_AGENT_PID" ]; then
	ssh-add -D < /dev/null
	eval $(ssh-agent -s -k)
fi
