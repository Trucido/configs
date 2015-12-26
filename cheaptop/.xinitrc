#!/bin/sh
# .xinitrc 1.0.2.1
# Time-stamp: <2015-12-26 02:11:23 PST xoddf2>

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

# CAPS LOCK as CTRL, and Menu as Compose
setxkbmap -option "ctrl:nocaps" -option "compose:menu" &

# Eye candy
compton -b
xsetroot -cursor_name left_ptr &
feh --bg-center --no-fehbg -z ~/img/wallpaper/ &

# Conky
conky &

# rxvt-unicode daemon
urxvtd -q -o -f

# Emacs
urxvtc -geometry 80x50+0+0 -sl 0 -name tmux -e tmux attach-session &
emacsclient -a '' -c -F "((top . 20) (left . 20))" &

# Start a WM or an xterm
cwm || twm || xterm

# Kill ssh-agent on exit
if [ "$SSH_AGENT_PID" ]; then
	ssh-add -D < /dev/null
	eval $(ssh-agent -s -k)
fi
