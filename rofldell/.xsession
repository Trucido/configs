#!/bin/sh
# .xsession 3.4   Time-stamp: <2014-11-16 21:14:14 PST xoddf2>

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

# Eye candy on the root window
compton -b
feh --bg-center -z ~/img/wallpaper/ &
conky &

# XClock
xclock -geometry +0-0 &

# rxvt-unicode daemon
urxvtd -q -o -f

# tmux
urxvtc -sl 0 -geometry 80x50+0+0 -e tmux attach-session &

# Start a WM, or, failing that, an xterm
cwm || fvwm || twm || xterm

# Kill ssh-agent at the end of the session
if [ "$SSH_AGENT_PID" ]; then
	ssh-add -D < /dev/null
	eval $(ssh-agent -s -k)
fi
