#!/bin/sh
# .xsession 1.3.4
# Time-stamp: <2016-06-04 00:58:16 PDT xoddf2>

. ~/.profile

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

# Disable bell
xset b off &

# nm-applet and Redshift
nm-applet &
redshift-gtk &

# rxvt-unicode daemon
urxvtd -q -o -f

# Auto-start tmux and Emacs
urxvtc -sl 0 -name tmux -e tmux attach-session &
emacsclient -a '' -c &

# Start a WM or an xterm
i3 || twm || xterm

# Kill ssh-agent on exit
if [ "$SSH_AGENT_PID" ]; then
	ssh-add -D < /dev/null
	eval $(ssh-agent -s -k)
fi
