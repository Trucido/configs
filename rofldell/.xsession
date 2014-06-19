#!/bin/sh
# .xsession 3.1.1   Time-stamp: <2014-06-18 23:54:29 PDT xoddf2>

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

# Eye candy
compton -b
feh --bg-center -z ~/img/wallpaper/ &
xsetroot -cursor_name left_ptr &

# rxvt-unicode daemon
urxvtd -q -o -f

# Emacs and tmux
emacsclient --alternate-editor="" -c &
urxvtc -name tmux -sl 0 -e tmux attach-session &

# Start a WM, or, failing that, an xterm
xmonad || fvwm || twm || xterm

# Kill ssh-agent at the end of the session
if [ "$SSH_AGENT_PID" ]; then
	ssh-add -D < /dev/null
	eval $(ssh-agent -s -k)
fi
