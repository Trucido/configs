#!/bin/sh
# .xsession 1.2.1.1
# Time-stamp: <2016-02-21 02:48:40 PST xoddf2>

. ~/.profile

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

# setxkbmap
setxkbmap -option "terminate:ctrl_alt_bksp" &

case "$(hostname)" in
	"rofldell.local")
		setxkbmap -option "compose:caps" & ;;
	"cheaptop")
		setxkbmap -option "ctrl:nocaps" -option "compose:menu" & ;;
esac

# Eye candy
case "$(hostname)" in
	"rofldell.local")
		xplanet -latitude 35 -longitude -95 -radius 75 -transparency & ;;
	"cheaptop")
		redshift &
		compton -b
		xsetroot -cursor_name left_ptr &
		feh --bg-center --no-fehbg -z ~/img/wallpaper/ &
		conky &
		;;
esac

# rxvt-unicode daemon
urxvtd -q -o -f

# Auto-start tmux and Emacs
case "$(hostname)" in
	"rofldell.local")
		urxvtc -sl 0 -name tmux -e tmux attach-session &
		emacsclient -a '' -c &
		;;
	"cheaptop")
		urxvtc -geometry 80x50+0+0 -sl 0 -name tmux -e tmux attach-session &
		emacsclient -a '' -c -F "((top . 20) (left . 20))" &
		;;
esac

# Start a WM or an xterm
case "$(hostname)" in
	"rofldell.local")
		fvwm ;;
	"cheaptop")
		cwm ;;
	*)
		twm || xterm ;;
esac

# Kill ssh-agent on exit
if [ "$SSH_AGENT_PID" ]; then
	ssh-add -D < /dev/null
	eval $(ssh-agent -s -k)
fi
