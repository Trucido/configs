#!/bin/sh
# .xsession 1.5
# Time-stamp: <2016-11-17 15:48:34 PST xoddf2>

. ~/.profile

export GTK_IM_MODULE="uim"
export QT_IM_MODULE="uim"
uim-xim &
export XMODIFIERS="@im=uim"

# Monitor setup
monitor-switch --auto

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

# Set root window colour and mouse pointer
xsetroot -solid rgb:24/24/24 -cursor_name left_ptr &

# Disable bell
xset b off &

# nm-applet and Redshift
nm-applet &
redshift-gtk &

# Start a WM or an xterm
xmonad || twm || xterm

# Kill ssh-agent on exit
if [ "$SSH_AGENT_PID" ]; then
	ssh-add -D < /dev/null
	eval $(ssh-agent -s -k)
fi
