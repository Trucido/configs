#!/bin/sh
# .xinitrc
# Time-stamp: <2017-11-16 10:34:41 PST xoddf2>

. ~/.profile

# UIM
export GTK_IM_MODULE="uim"
export QT_IM_MODULE="uim"
uim-xim &
export XMODIFIERS="@im=uim"

# Use GTK theme in Qt 5 applications
export QT_QPA_PLATFORMTHEME="gtk2"
export QT_STYLE_OVERRIDE="gtk2"

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

# For notifications
eval $(dbus-launch --sh-syntax --exit-with-session)

# Configure monitors and set wallpaper
if (xrandr | grep -q "HDMI-2 connected"); then
	xrandr --output "LVDS-1" --auto --primary --output "HDMI-2" --auto --right-of "LVDS-1"
	feh --bg-fill --no-fehbg -z ~/media/img/wallpaper ~/media/img/wallpaper
else
	xrandr --output "LVDS-1" --auto --primary --output "HDMI-2" --off
	feh --bg-fill --no-fehbg -z ~/media/img/wallpaper
fi

# X resources
xrdb -merge ~/.Xresources

# Keyboard settings
remap

# XScreenSaver
xscreensaver -no-splash &
dunst-screensaver &

# Compositor
compton -b

# Set mouse pointer
xsetroot -cursor_name left_ptr

# Disable bell
xset b off

# System tray and notifications
trayer --edge bottom --align left --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --alpha 0 --tint 0x242424 --height 18 --margin 0 &
dunst &

# System tray icons
nm-applet &
redshift-gtk &
uim-toolbar-gtk-systray &

# Bottom status bar
xmobar ~/.xmonad/xmobar-bottom.hs &

# Start Emacs and tmux
pgrep -u $(whoami) -f "emacs --daemon" || emacs --daemon
pgrep -u $(whoami) -f "tmux -u start-server" || tmux -u start-server

# Start window manager
xmonad

# Kill ssh-agent on exit
if [ "$SSH_AGENT_PID" ]; then
	ssh-add -D < /dev/null
	eval $(ssh-agent -s -k)
fi
